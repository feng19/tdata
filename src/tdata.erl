-module(tdata).

%% API exports
-export([
    main/1,
    start/0,
    start/2,
    stop/0,
    set_key/2,
    get_key/1,
    transform_all/2,
    transform_files/3,
    transform_file/3
]).

-export_type([
    transform_define/0,
    transform_defines/0,
    global_config/0,
    transform_fun_config/0,
    output_results/0
]).

-type input_file() :: file:filename().
-type output_file() :: file:filename().
-type input_file_opts() :: any().
-type input_file_define() :: #{file := input_file(), opts := input_file_opts()}.
-type transform_define() :: #{
    input_file_defines := input_file_define() | [input_file_define()],
    output_file := output_file() | dynamic,
    transform_fun := function(),
    tpl_type => tdata_render:tpl_type(),
    tpl_file => file:filename()
}.

-type transform_defines() :: [transform_define()].

-type global_config() :: #{
    input_dir := file:filename(),
    output_dir := file:filename(),
    template_dir => file:filename()
}.
-type transform_fun_config() :: any().
-type output_error_status() :: any().
-type output_status() :: ok | skipped | output_error_status().
-type output_result() :: {output_file(), output_status()}.
-type output_results() :: [output_result()].

-callback transform_defines() -> transform_defines().
-callback transform_defines(transform_fun_config()) -> transform_defines().
-optional_callbacks([transform_defines/0, transform_defines/1]).
%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    tdata_es:main(Args).

start() ->
    application:ensure_all_started(?MODULE).

start(_, _) ->
    tdata_loader:start(),
    Pid = spawn_link(fun() -> receive shutdown -> ok end end),
    {ok, Pid}.

-spec stop() -> ok.
stop() ->
    tdata_loader:stop(),
    ok.

set_key(Key, Value) ->
    application:set_env(?MODULE, Key, Value).

get_key(Key) ->
    application:get_env(?MODULE, Key, undefined).

-spec transform_all(global_config(), transform_fun_config()) -> [output_results()].
transform_all(GlobalConfig, TransformConfig) ->
    [transform_files(DefineModule, GlobalConfig, TransformConfig) ||
        DefineModule <- tdata_loader:all_attr_modules(behavior, [?MODULE])].

-spec transform_files(DefineModule :: module() | transform_define() | transform_defines(),
    global_config(), transform_fun_config()) -> output_results().
transform_files(DefineModule, GlobalConfig, TransformConfig) when is_atom(DefineModule) ->
    TransformDefines =
        case erlang:function_exported(DefineModule, transform_defines, 1) of
            true ->
                DefineModule:transform_defines(TransformConfig);
            false ->
                DefineModule:transform_defines()
        end,
    transform_files(TransformDefines, GlobalConfig, TransformConfig);
transform_files(TransformDefines, GlobalConfig, TransformConfig) when is_list(TransformDefines) ->
    lists:flatten([transform_file(TransformDefine, GlobalConfig, TransformConfig)
        || TransformDefine <- TransformDefines]);
transform_files(TransformDefine, GlobalConfig, TransformConfig) when is_map(TransformDefine) ->
    transform_file(TransformDefine, GlobalConfig, TransformConfig).

-spec transform_file(transform_define(), global_config(), transform_fun_config()) -> output_result() | output_results().
transform_file(TransformDefine, GlobalConfig, TransformConfig) ->
    case check_transform_define(TransformDefine) of
        ok ->
            #{input_file_defines := InputFileDefines} = TransformDefine,
            #{input_dir := InputDir} = GlobalConfig,
            InputFiles = get_input_files_from_defines(InputFileDefines, InputDir),
            case is_all_file(InputFiles) of
                true ->
                    transform_file_1(InputFiles, TransformDefine, GlobalConfig, TransformConfig);
                _ ->
                    Format = string:join(lists:duplicate(length(InputFiles), "~ts"), " "),
                    {lists:flatten(io_lib:format(Format, InputFiles)), input_file_miss}
            end;
        Result -> Result
    end.

%%====================================================================
%% Internal functions
%%====================================================================

transform_file_1(InputFiles, #{input_file_defines := InputFileDefines, output_file := OutputFile0,
    transform_fun := TransformFun} = TransformDefine,
    #{input_dir := InputDir, output_dir := OutputDir} = GlobalConfig, TransformConfig) ->
    IsForce = maps:get(force, GlobalConfig, false),
    case OutputFile0 of
        dynamic -> % always call transform_file to get OutputList
            {InputDataList, OutputList} = transform_file_do(InputFileDefines, OutputFile0,
                TransformFun, InputDir, TransformConfig),
            HeaderComments = mk_header_comments(InputDataList),
            LastMaxInputTime = max_last_modified(InputFiles),
            {TplType, TplFile} = get_tpl_info(TransformDefine, GlobalConfig),
            case OutputList of
                _ when is_list(OutputList) ->
                    [transform_file_render_list(Output, OutputDir, LastMaxInputTime,
                        TplType, TplFile, HeaderComments, IsForce) || Output <- OutputList];
                Output ->
                    transform_file_render_list(Output, OutputDir, LastMaxInputTime,
                        TplType, TplFile, HeaderComments, IsForce)
            end;
        _ ->
            OutputFile = filename:join(OutputDir, OutputFile0),
            ok = filelib:ensure_dir(OutputFile),
            {TplType, TplFile} = get_tpl_info(TransformDefine, GlobalConfig),
            case IsForce orelse is_need_transform(InputFiles, OutputFile, TplFile) of
                true ->
                    {InputDataList, Data} = transform_file_do(InputFileDefines, OutputFile0,
                        TransformFun, InputDir, TransformConfig),
                    HeaderComments = mk_header_comments(InputDataList),
                    transform_file_render_do(Data, TplType, TplFile, OutputFile0, OutputFile, HeaderComments);
                false ->
                    {OutputFile0, skipped}
            end
    end.

transform_file_render_list(Output, OutputDir, LastMaxInputTime, TplType, TplFile, HeaderComments, IsForce) ->
    case Output of
        {ok, OutputFile1, RenderData} ->
            transform_file_render({ok, RenderData},
                OutputDir, OutputFile1, LastMaxInputTime, TplType, TplFile, HeaderComments, IsForce);
        {ok, OutputFile1, RenderData, RenderOptions} ->
            transform_file_render({ok, RenderData, RenderOptions},
                OutputDir, OutputFile1, LastMaxInputTime, TplType, TplFile, HeaderComments, IsForce);
        Error ->
            {"dynamic", Error}
    end.

transform_file_render(Data, OutputDir, OutputFile0, LastMaxInputTime, TplType, TplFile, HeaderComments, IsForce) ->
    OutputFile = filename:join(OutputDir, OutputFile0),
    case IsForce orelse is_need_transform(LastMaxInputTime, OutputFile, TplFile) of
        true ->
            case filelib:ensure_dir(OutputFile) of
                ok ->
                    transform_file_render_do(Data,
                        TplType, TplFile, OutputFile0, OutputFile, HeaderComments);
                {error, Reason} ->
                    {OutputFile, Reason}
            end;
        false ->
            {OutputFile0, skipped}
    end.

transform_file_render_do(Data, TplType, TplFile, OutputFile0, OutputFile, HeaderComments) ->
    case tdata_render:render(Data, TplType, TplFile, OutputFile, HeaderComments) of
        ok -> {OutputFile0, ok};
        Error -> {OutputFile0, Error}
    end.

mk_header_comments(InputDataList) ->
    SourceFiles = unicode:characters_to_binary(string:join([InputFile || {InputFile, _} <- InputDataList], ",")),
    <<"%% Automatically generated, do not edit\n%% Source Files: ", SourceFiles/binary, "\n">>.

transform_file_do(InputFileDefines, OutputFile0, TransformFun, InputDir, TransformConfig) ->
    InputDataList = tdata_loader:load_input_files(InputFileDefines, InputDir),
    Data =
        case erlang:fun_info(TransformFun, arity) of
            {arity, 1} ->
                TransformFun(InputDataList);
            {arity, 2} ->
                TransformFun(OutputFile0, InputDataList);
            {arity, 3} ->
                TransformFun(OutputFile0, InputDataList, TransformConfig)
        end,
    {InputDataList, Data}.

check_transform_define(#{output_file := OutputFile0} = TransformDefine) ->
    case maps:get(input_file_defines, TransformDefine, miss) of
        miss ->
            {OutputFile0, miss_input_file_defines};
        _ ->
            case maps:get(transform_fun, TransformDefine, miss) of
                miss ->
                    {OutputFile0, miss_transform_fun};
                _ ->
                    ok
            end
    end;
check_transform_define(#{file_defines := _FileDefines} = _TransformDefine) ->
    ok;
check_transform_define(_TransformDefine) ->
    error(miss_output_file).

get_input_files_from_defines(#{file := InputFile}, InputDir) ->
    [filename:join(InputDir, InputFile)];
get_input_files_from_defines(InputFileDefines, InputDir) when is_list(InputFileDefines) ->
    lists:foldl(
        fun(InputFileDefine, Acc) ->
            get_input_files_from_defines(InputFileDefine, InputDir) ++ Acc
        end, [], InputFileDefines);
get_input_files_from_defines(InputFileDefines, _InputDir) ->
    error({error_input_file_defines, InputFileDefines}).

get_tpl_info(TransformDefine, GlobalConfig) ->
    case maps:get(tpl_file, TransformDefine, miss) of
        miss ->
            {undefined, undefined};
        TplFile0 ->
            TplType0 = get_tpl_type(TplFile0, TransformDefine),
            TplDir = get_tpl_dir(TransformDefine, GlobalConfig),
            {TplType0, filename:join(TplDir, TplFile0)}
    end.

get_tpl_type(TplFile, TransformDefine) ->
    case maps:get(tpl_type, TransformDefine, auto) of
        auto ->
            case filename:extension(TplFile) of
                ".tpl" -> mustache;
                ".dtl" -> dtl;
                _ -> unknow
            end;
        TplType -> TplType
    end.

get_tpl_dir(TransformDefine, Config) ->
    case maps:get(template_dir, TransformDefine, miss) of
        miss ->
            maps:get(template_dir, Config);
        TplDir -> TplDir
    end.

is_all_file([]) -> true;
is_all_file([InputFile | InputFiles]) ->
    filelib:is_file(InputFile) andalso not filelib:is_dir(InputFile) andalso is_all_file(InputFiles).

is_need_transform(InputFiles, OutputFile, TplFile0) when is_list(InputFiles) ->
    Input = max_last_modified(InputFiles),
    is_need_transform(Input, OutputFile, TplFile0);
is_need_transform(Input, OutputFile, TplFile0) ->
    Output = filelib:last_modified(OutputFile),
    case TplFile0 of
        undefined ->
            Input > Output;
        TplFile ->
            Tpl = filelib:last_modified(TplFile),
            Input > Output orelse Tpl > Output
    end.

max_last_modified(Files) ->
    lists:max([filelib:last_modified(File) || File <- Files]).