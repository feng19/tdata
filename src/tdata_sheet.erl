-module(tdata_sheet).

%% API
-export([
    load_sheets/3
]).

load_sheets(PythonPid, ExcelFile, TargetConfig) ->
    Data = python:call(PythonPid, load_data, load_excel, [list_to_binary(ExcelFile)]),
    Sheets = maps:from_list([Sheet || Sheet <- Data, not_empty_rows_sheet(Sheet)]),
    loop_all_sheet_name(Sheets, [{excel, ExcelFile} | TargetConfig]).

not_empty_rows_sheet({undefined, _Rows}) -> false;
not_empty_rows_sheet({[], _Rows}) -> false;
not_empty_rows_sheet({_SheetName, undefined}) -> false;
not_empty_rows_sheet({_SheetName, []}) -> false;
not_empty_rows_sheet({_SheetName, _Rows}) -> true.

loop_all_sheet_name(Sheets, TargetConfig0) ->
    case lists:keytake(sheet_name, 1, TargetConfig0) of
        false ->
            {miss_sheet_name, TargetConfig0};
        {value, {sheet_name, all, SheetConfig}, TargetConfig1} ->
            TargetConfig = tdata_util:key_delete_all(sheet_name, 1, TargetConfig1),
            AllSheetNameConfig = [{sheet_name, SheetName, SheetConfig} || SheetName <- maps:keys(Sheets)],
            loop_all_sheet_name(Sheets, AllSheetNameConfig ++ TargetConfig);
        {value, {sheet_name, SheetName, SheetConfig}, TargetConfig} ->
            case sheet_name(SheetName, Sheets, SheetConfig) of
                {ok, SheetData} ->
                    loop_all_sheet_name_do(Sheets, TargetConfig, #{SheetName => SheetData});
                Err -> Err
            end
    end.

loop_all_sheet_name_do(Sheets, TargetConfig0, Data) ->
    case lists:keytake(sheet_name, 1, TargetConfig0) of
        false ->
            {ok, Data};
        {value, {sheet_name, SheetName, SheetConfig}, TargetConfig} ->
            case sheet_name(SheetName, Sheets, SheetConfig) of
                {ok, SheetData} ->
                    loop_all_sheet_name_do(Sheets, TargetConfig, Data#{SheetName => SheetData});
                Err -> Err
            end
    end.

sheet_name(SheetName, Sheets, TargetConfig) ->
    case maps:find(SheetName, Sheets) of
        {ok, Rows} ->
            Data = #{sheet_name => SheetName, rows => Rows},
            case skip_comments(Data, TargetConfig) of
                {ok, _} = Res -> Res;
                Err ->
                    {do_sheet_config_err, SheetName, Err}
            end;
        error ->
            {sheet_name_undefined, SheetName}
    end.

skip_comments(Data, TargetConfig0) ->
    case lists:keytake(skip_comments, 1, TargetConfig0) of
        {value, {skip_comments, true}, TargetConfig} ->
            #{rows := [Comments0 | Rows]} = Data,
            Comments = lists:map(fun get_cell_value/1, Comments0),
            NewData = Data#{comments => Comments, rows => Rows},
            zip_header(NewData, TargetConfig);
        {value, {skip_comments, N}, TargetConfig} when is_integer(N) ->
            #{rows := Rows0} = Data,
            {CommentsRows, Rows} = lists:split(N, Rows0),
            Comments = [lists:map(fun get_cell_value/1, Comments0) || Comments0 <- CommentsRows],
            NewData = Data#{comments => Comments, rows => Rows},
            zip_header(NewData, TargetConfig);
        _ ->
            zip_header(Data, TargetConfig0)
    end.

zip_header(Data, TargetConfig0) ->
    {NewData, NewTargetConfig} =
        case lists:keytake(data_structure, 1, TargetConfig0) of
            {value, {_, map}, TargetConfig} ->
                {zip_header_map(Data), TargetConfig};
%%            {value, {_, RecordName}, TargetConfig} ->
%%                Module = proplists:get_value(module, TargetConfig),
%%                {zip_header_record(Module, RecordName, Data), TargetConfig};
            false -> %% default atom map
                {zip_header_map(Data), TargetConfig0}
        end,
    case NewData of
        _ when is_map(NewData) ->
            check_data_and_groups(NewData, NewTargetConfig);
        Err -> Err
    end.

zip_header_map(#{rows := [Header0 | Rows0]} = Data) ->
    Header = get_header(Header0),
    Rows = [maps:from_list(lists:zip(Header, Row)) || Row <- Rows0],
    Data#{data_structure => map, header => Header, rows => Rows}.

%%zip_header_record(Module, RecordName, #{rows := [Header0 | Rows0]} = Data) ->
%%    Header = get_header(Header0),
%%    case erlang:function_exported(Module, '#new-', 1) of
%%        true ->
%%            Record = Module:'#new-'(RecordName),
%%            Rows = [zip_header_record_do(Module, Record, Header, Row) || Row <- Rows0],
%%            Data#{data_structure => record, header => Header, rows => Rows};
%%        false ->
%%            {not_exprecs_module, Module}
%%    end.
%%
%%zip_header_record_do(Module, Record, Header, Row) ->
%%    Proplists = lists:zip(Header, Row),
%%    Module:'#fromlist-'(Proplists, Record).

check_data_and_groups(Data, TargetConfig0) ->
    Checks = proplists:get_value(checks, TargetConfig0, []),
    case lists:keytake(groups, 1, TargetConfig0) of
        {value, {groups, []}, _TargetConfig} ->
            check_data(Data, Checks);
        {value, {groups, Groups0}, _TargetConfig} ->
            #{data_structure := DataStructure, header := Header} = Data,
            Groups = lists:flatten(Groups0),
            case Groups -- Header of
                [] ->
                    {GroupChecks, NewChecks} =
                        lists:splitwith(
                            fun(Check) ->
                                lists:member(element(1, Check), Groups)
                            end, Checks),
                    case check_data(Data, Groups, NewChecks) of
                        {ok, NewData} ->
                            groups(DataStructure, Groups0, NewData, GroupChecks);
                        Err -> Err
                    end;
                MissGroups ->
                    {undefined_groups, MissGroups}
            end;
        false ->
            check_data(Data, Checks)
    end.

check_data(Data, Checks) ->
    check_data(Data, [], Checks).
check_data(#{header := Header, rows := Rows} = Data, FilterFields, Checks) ->
    NewHeader = [Field || Field <- Header, not lists:member(Field, FilterFields)],
    case check_cols_map(NewHeader, Checks, Rows) of
        {ok, NewRows} ->
            {ok, Data#{rows => NewRows}};
        Err -> Err
    end.

check_cols_map([Field | Header], Checks, Rows) ->
    case check_rows_map(Field, Rows, Checks, []) of
        {ok, NewRows} ->
            check_cols_map(Header, Checks, NewRows);
        Err -> Err
    end;
check_cols_map([], _Checks, NewRows) ->
    {ok, NewRows}.

check_rows_map(Field, [Row | Rows], Checks, Acc) ->
    CheckList = get_field_checks(Field, Checks),
    Cell0 = maps:get(Field, Row),
    case get_cell_value(Cell0, CheckList) of
        {ok, CellValue} ->
            NewRow = Row#{Field => CellValue},
            check_rows_map(Field, Rows, Checks, [NewRow | Acc]);
        Err -> Err
    end;
check_rows_map(_Field, [], _Checks, Acc) ->
    {ok, lists:reverse(Acc)}.

groups(DataStructure, Groups, #{rows := Rows} = Data, Checks) ->
    Fun = get_group_fun_by_data_structure(DataStructure),
    case Fun(Groups, Rows, Checks) of
        {ok, NewRows} ->
            {ok, Data#{rows := NewRows}};
        Err -> Err
    end.

get_group_fun_by_data_structure(map) -> fun groups_map/3.

groups_map([[GroupKey | SameGroups] | Groups], Rows, Checks) when is_atom(GroupKey) ->
    case groups_map_first_row(GroupKey, SameGroups, Rows, Checks) of
        {ok, GroupAcc} ->
            GroupList = maps:to_list(GroupAcc),
            case groups_map_do(GroupList, Groups, Checks, #{}) of
                {ok, Data} ->
                    {ok, #{GroupKey => Data}};
                Err -> Err
            end;
        Err -> Err
    end;
groups_map([GroupKey | Groups], Rows, Checks) when is_atom(GroupKey) ->
    groups_map([[GroupKey] | Groups], Rows, Checks);
groups_map([], Rows, _Checks) -> {ok, Rows}.

groups_map_do([{Key, Rows0} | GroupList], Groups, Checks, Acc) ->
    Rows = lists:reverse(Rows0),
    case groups_map(Groups, Rows, Checks) of
        {ok, Data} ->
            groups_map_do(GroupList, Groups, Checks, Acc#{Key => Data});
        Err -> Err
    end;
groups_map_do([], _Groups, _Checks, Acc) -> {ok, Acc}.

groups_map_first_row(GroupKey, SameGroups, [Row | Rows], Checks) ->
    Cell = maps:get(GroupKey, Row),
    case is_cell_empty(Cell) of
        false ->
            FieldCheckList = get_field_checks(GroupKey, Checks),
            SameGroupChecks = get_fields_checks(SameGroups, Checks),
            groups_map_same_groups(GroupKey, Cell, Row, Rows, FieldCheckList, SameGroupChecks, #{});
        true ->
            {first_row_is_empty, GroupKey}
    end.

groups_map_same_groups(GroupKey, Cell0, Row, Rows, FieldCheckList, SameGroupChecks, Acc) ->
    case get_cell_value(Cell0, FieldCheckList) of
        {ok, CellValue} ->
            case get_cells_map(SameGroupChecks, Row, #{GroupKey => CellValue}) of
                {ok, DefaultRow} ->
                    NewRow = maps:merge(Row, DefaultRow),
                    groups_map_rows(GroupKey, Rows, CellValue, DefaultRow, FieldCheckList, SameGroupChecks,
                        Acc#{CellValue => [NewRow]});
                Err -> Err
            end;
        Err -> Err
    end.

groups_map_rows(GroupKey, [Row | Rows], LastCell, DefaultRow, FieldCheckList, SameGroupChecks, Acc) ->
    Cell = maps:get(GroupKey, Row),
    case is_cell_empty(Cell) of
        false ->
            groups_map_same_groups(GroupKey, Cell, Row, Rows, FieldCheckList, SameGroupChecks, Acc);
        true ->
            NewRow = maps:merge(Row, DefaultRow),
            NewAcc = Acc#{LastCell => [NewRow | maps:get(LastCell, Acc)]},
            groups_map_rows(GroupKey, Rows, LastCell, DefaultRow, FieldCheckList, SameGroupChecks, NewAcc)
    end;
groups_map_rows(_GroupKey, [], _LastCell, _DefaultRow, _FieldCheckList, _SameGroupChecks, Acc) ->
    {ok, Acc}.

get_header(Header) ->
    [binary_to_atom(Cell, utf8) || {_Row, _Col, _Type, Cell} <- Header].

is_empty(<<>>) -> true;
is_empty([]) -> true;
is_empty(_) -> false.

is_cell_empty(Cell) ->
    is_empty(get_cell_value(Cell)).

get_cell_value({_Row, _Col, _Type, Cell}) -> Cell.

get_field_checks(Field, Checks) ->
    proplists:get_value(Field, Checks, []).

get_fields_checks(Fields, Checks) ->
    get_fields_checks(Fields, Checks, []).
get_fields_checks([Field | Fields], Checks, Acc) ->
    get_fields_checks(Fields, Checks, [{Field, get_field_checks(Field, Checks)} | Acc]);
get_fields_checks([], _Checks, Acc) -> Acc.

get_cells_map([{Field, FieldCheckList} | CheckFields], Row, Acc) ->
    case get_cell_value(maps:get(Field, Row), FieldCheckList) of
        {ok, Cell} ->
            get_cells_map(CheckFields, Row, Acc#{Field => Cell});
        Err -> Err
    end;
get_cells_map([], _Row, Acc) -> {ok, Acc}.

get_cell_value({_Row, _Col, _Type, Cell}, []) -> {ok, Cell};
get_cell_value({Row, Col, Type, Cell}, CheckList) ->
    NewCheckList = [Check || Check <- CheckList, is_function(Check, 1) orelse
        is_function(Check, 2) orelse is_atom(Check)],
    case check_fun_cell(NewCheckList, Type, Cell) of
        {ok, _} = Res -> Res;
        Err ->
            {check_field_fail, [
                {row, Row}, {col, Col}, {type, Type}, {cell, Cell}, {err, Err}
            ]}
    end.

check_fun_cell([Check | CheckList], Type, Cell) when is_function(Check, 1) ->
    case Check(Cell) of
        {ok, NewCell} ->
            check_fun_cell(CheckList, Type, NewCell);
        Err -> Err
    end;
check_fun_cell([Check | CheckList], Type, Cell) when is_function(Check, 2) ->
    case Check(Type, Cell) of
        {ok, NewCell} ->
            check_fun_cell(CheckList, Type, NewCell);
        Err -> Err
    end;
check_fun_cell([Check|CheckList], Type, Cell) when is_atom(Check) ->
    code:ensure_loaded(tdata_util),
    case erlang:function_exported(tdata_util, Check, 1) of
        true ->
            case tdata_util:Check(Cell) of
                {ok, NewCell} ->
                    check_fun_cell(CheckList, Type, NewCell);
                Err -> Err
            end;
        false ->
            case erlang:function_exported(tdata_util, Check, 2) of
                true ->
                    case tdata_util:Check(Type, Cell) of
                        {ok, NewCell} ->
                            check_fun_cell(CheckList, Type, NewCell);
                        Err -> Err
                    end;
                false ->
                    {undefined_check_fun, Check}
            end
    end;
check_fun_cell([], _Type, Cell) -> {ok, Cell}.
