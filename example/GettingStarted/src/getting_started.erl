%% 数据处理模块示例
-module(getting_started).
% 固定写法
-tdata(ds).

%% API
-export([
    transform_defines/0
]).

%% 配置
transform_defines() ->
    % sheet 读取配置
    SheetOpts = #{
        skip_comments => true,
        type_comment => true,
        only_rows => true
    },
    % excel 文件 读取配置
    ExcelLoaderOpts = #{
        <<"类型"/utf8>> => SheetOpts,
        <<"groups">> => SheetOpts#{
            groups => [level_1, level_2]
        },
        <<"same_level_groups">> => SheetOpts#{
            groups => [[level_1, level_2]]
        }
    },
    [#{
        % 输入文件列表 & 已经加载配置
        input_file_defines => [#{file =>"config.xlsx", opts => ExcelLoaderOpts}],
        % 生成目标文件
        output_file => "src/config.erl",
        % 转换数据函数
        % transform_fun => fun only_print/1,
        transform_fun => fun transform_fun/1,
        % 模板文件
        tpl_file => "config.erl.tpl"
    }].

only_print(InputList) ->
    io:format("~p~n", [InputList]),
    skip.

%% 转换数据
transform_fun([{_InputFile, Sheets}]) ->
    #{
        <<"类型"/utf8>> := RecordsRows,
        <<"groups">> := GroupsRows,
        <<"same_level_groups">> := SameLevelGroupsRows
    } = Sheets,
    Groups = maps:fold(
        fun(Level1, Level1Maps, Acc0) ->
            maps:fold(
                fun(Level2, Level2List, Acc1) ->
                    Level3List = [Level3 || #{level_3 := Level3} <- Level2List],
                    [#{level_1 => Level1, level_2 => Level2, level_3 => format_function_value(Level3List)} | Acc1]
                end, Acc0, Level1Maps)
        end, [], GroupsRows),

    SameLevelGroups = maps:fold(
        fun(Level1, Level1List, Acc0) ->
            #{level_2 := Level2} = hd(Level1List),
            Level3List = [Level3 || #{level_3 := Level3} <- Level1List],
            [#{level_1 => Level1, level_2 => Level2, level_3 => format_function_value(Level3List)} | Acc0]
        end, [], SameLevelGroupsRows),

    Records = [#{index => Index, data => term_to_string(Record)} || #{int := Index} = Record <- RecordsRows],

    {ok, #{
        module_name => "config",
        records => add_is_last(Records),
        groups => add_is_last(lists:reverse(Groups)),
        same_level_groups => add_is_last(lists:reverse(SameLevelGroups))
    }}.

term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

format_function_value(Value) ->
    io_lib:format("~w", [Value]).
%%    IoList = erl_prettypr:format(erl_syntax:abstract(Value), [{paper, 100}, {ribbon, 80}]),
%%    unicode:characters_to_binary(IoList).

add_is_last([]) -> [];
add_is_last(List) ->
    [H | T] = lists:reverse(List),
    case H of
        List when is_list(List) ->
            lists:reverse([[{is_last, true} | H] | T]);
        Map when is_map(Map) ->
            lists:reverse([H#{is_last => true} | T]);
        _ -> List
    end.
