-module(tdata_excel_loader).
-behavior(tdata_loader).

%% API
-export([
    start/0,
    stop/0,
    set_python_dir/0,
    set_python_dir/2,
    load_sheets/2,
    load_sheets/3
]).

-export_type([load_sheets_opts/0]).

-type field_name() :: atom().
-type field_check_opt() ::
    check_integer |
    check_float |
    check_not_empty |
    cell_trans_punctuation |
    function().
-type check_field() :: {field_name(), [field_check_opt()]}.
-type sheet_name() :: binary() | all.
-type load_sheet_opts() :: #{
    skip_comments := boolean() | pos_integer(), %% default false
    groups := [field_name()],
    checks := [check_field()]
}.
-type load_sheets_opts() :: #{sheet_name() => load_sheet_opts()}.

%%====================================================================
%% API functions
%%====================================================================

start() ->
    init_excel_loader().

stop() ->
    case tdata:get_key(python_pid) of
        undefined -> ok;
        PythonPid ->
            python:stop(PythonPid)
    end,
    tdata_loader:del_loader(),
    tdata_loader:del_loader(".xls"),
    tdata_loader:del_loader(".xlsx"),
    ok.

set_python_dir() ->
    PythonPath = filename:join(code:priv_dir(tdata), "python/"),
    ErlPortPath = filename:join(code:priv_dir(erlport), "python2/"),
    set_python_dir(PythonPath, ErlPortPath).
set_python_dir(PythonPath, ErlPortPath) ->
    tdata:set_key(python_path, PythonPath),
    tdata:set_key(erl_port_path, ErlPortPath),
    ok.

-spec init_excel_loader() -> ok.
init_excel_loader() ->
    PythonPath =
        case tdata:get_key(python_path) of
            undefined ->
                set_python_dir(),
                tdata:get_key(python_path);
            T -> T
        end,
    ErlPortPath = tdata:get_key(erl_port_path),
    init_excel_loader(ErlPortPath, PythonPath).
-spec init_excel_loader(file:filename(), file:filename()) -> ok.
init_excel_loader(ErlPortPath, PythonPath) ->
    {ok, PythonPid} = python:start([{cd, ErlPortPath}, {python_path, PythonPath}]),
    ExcelLoader =
        fun(ExcelFile, LoadSheetsOpts) ->
            load_sheets(PythonPid, ExcelFile, LoadSheetsOpts)
        end,
    tdata_loader:set_loader(ExcelLoader),
    tdata_loader:set_loader(".xls", ExcelLoader),
    tdata_loader:set_loader(".xlsx", ExcelLoader),
    tdata:set_key(python_pid, PythonPid),
    ok.

-spec load_sheets(ExcelFile :: file:filename(), load_sheets_opts()) ->
    {ok, Data :: map()} | tuple().
load_sheets(ExcelFile, LoadSheetsOpts) ->
    PythonPid = tdata:get_key(python_pid),
    load_sheets(PythonPid, ExcelFile, LoadSheetsOpts).
-spec load_sheets(PythonPid :: pid(), ExcelFile :: file:filename(), load_sheets_opts()) ->
    {ok, Data :: map()} | tuple().
load_sheets(PythonPid, ExcelFile, LoadSheetsOpts) when is_map(LoadSheetsOpts) ->
    Data = python:call(PythonPid, load_data, load_excel, [unicode:characters_to_binary(ExcelFile)]),
    case [Sheet || Sheet <- Data, filter_empty_rows_sheet(Sheet)] of
        [] ->
            {empty_sheets, ExcelFile};
        Sheets ->
            loop_all_sheet_name(LoadSheetsOpts, maps:from_list(Sheets))
    end;
load_sheets(_PythonPid, ExcelFile, LoadSheetsOpts) ->
    {load_sheets_opts_not_map, ExcelFile, LoadSheetsOpts}.

%%====================================================================
%% Internal functions
%%====================================================================

filter_empty_rows_sheet({undefined, _Rows}) -> false;
filter_empty_rows_sheet({[], _Rows}) -> false;
filter_empty_rows_sheet({_SheetName, undefined}) -> false;
filter_empty_rows_sheet({_SheetName, []}) -> false;
filter_empty_rows_sheet({_SheetName, _Rows}) -> true.

loop_all_sheet_name(#{all := LoadSheetOpts} = LoadSheetsOpts, Sheets) ->
    AllLoadSheetsOpts = maps:from_list([{SheetName, LoadSheetOpts} || SheetName <- maps:keys(Sheets)]),
    NewLoadSheetsOpts = maps:merge(AllLoadSheetsOpts, maps:remove(all, LoadSheetsOpts)),
    loop_all_sheet_name(NewLoadSheetsOpts, Sheets);
loop_all_sheet_name(LoadSheetsOpts, Sheets) when is_map(LoadSheetsOpts) ->
    loop_all_sheet_name_do(maps:to_list(LoadSheetsOpts), Sheets, #{}).

loop_all_sheet_name_do([{SheetName, LoadSheetOpts} | LoadSheetsOpts], Sheets, Data)
    when is_binary(SheetName) andalso is_map(LoadSheetOpts) ->
    case sheet_name(SheetName, Sheets, LoadSheetOpts) of
        {ok, SheetData} ->
            loop_all_sheet_name_do(LoadSheetsOpts, Sheets, Data#{SheetName => SheetData});
        Err -> Err
    end;
loop_all_sheet_name_do([{SheetName, _LoadSheetOpts} | _], _Sheets, _Data) when not is_binary(SheetName) ->
    {error_sheet_name, SheetName};
loop_all_sheet_name_do([{SheetName, LoadSheetOpts} | _], _Sheets, _Data) when not is_map(LoadSheetOpts) ->
    {error_load_sheet_opts, SheetName, LoadSheetOpts};
loop_all_sheet_name_do([], _Sheets, Data) -> {ok, Data}.

sheet_name(SheetName, Sheets, LoadSheetOpts) ->
    case maps:find(SheetName, Sheets) of
        {ok, Rows} ->
            Data = #{sheet_name => SheetName, rows => Rows},
            case skip_comments(LoadSheetOpts, Data) of
                {ok, _} = Res -> Res;
                Err ->
                    {do_sheet_config_err, SheetName, Err}
            end;
        error ->
            {unfound_sheet, SheetName}
    end.

skip_comments(#{skip_comments := true} = LoadSheetOpts, Data) -> %% N=1
    #{rows := [Comments0 | Rows]} = Data,
    Comments = lists:map(fun get_cell_value/1, Comments0),
    NewData = Data#{comments => Comments, rows => Rows},
    zip_header(NewData, LoadSheetOpts);
skip_comments(#{skip_comments := N} = LoadSheetOpts, Data) when is_integer(N) ->
    #{rows := Rows0} = Data,
    {CommentsRows, Rows} = lists:split(N, Rows0),
    Comments = [lists:map(fun get_cell_value/1, Comments0) || Comments0 <- CommentsRows],
    NewData = Data#{comments => Comments, rows => Rows},
    zip_header(NewData, LoadSheetOpts);
skip_comments(LoadSheetOpts, Data) -> %% false
    zip_header(Data, LoadSheetOpts).

zip_header(#{rows := [Header0 | Rows0]} = Data, LoadSheetOpts) ->
    case check_header(Header0) of
        {ok, Header} ->
            Rows = [maps:from_list(lists:zip(Header, Row)) || Row <- Rows0],
            check_data_and_groups(Data#{header => Header, rows => Rows}, LoadSheetOpts);
        Err -> Err
    end.

check_data_and_groups(Data, LoadSheetOpts) ->
    Checks = maps:get(checks, LoadSheetOpts, []),
    case maps:get(groups, LoadSheetOpts, []) of
        [] ->
            check_data(Data, Checks);
        Groups0 ->
            #{header := Header} = Data,
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
                            groups(Groups0, NewData, GroupChecks);
                        Err -> Err
                    end;
                MissGroups ->
                    {undefined_groups, MissGroups}
            end
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

groups(Groups, #{rows := Rows} = Data, Checks) ->
    case groups_map(Groups, Rows, Checks) of
        {ok, NewRows} ->
            {ok, Data#{rows := NewRows}};
        Err -> Err
    end.

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

check_header([]) -> empty_header;
check_header(Header) -> get_header(Header, []).

get_header([{Row, Col, Type, Cell} | Header], NewHeader) ->
    case catch binary_to_atom(Cell, utf8) of
        {'EXIT', Reason} ->
            {header_trans_to_atom, [
                {row, Row}, {col, Col}, {type, Type}, {cell, Cell}, {err, Reason}
            ]};
        Field ->
            get_header(Header, [Field | NewHeader])
    end;
get_header([], Header) -> {ok, lists:reverse(Header)}.

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
check_fun_cell([Check | CheckList], Type, Cell) when is_atom(Check) ->
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