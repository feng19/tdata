-module(tdata_loader).

%% API
-export([
    ensure_ets/0,
    init_excel_loader/1,
    set_loader/2,
    get_loader/1,
    load_input_files/3
]).

%%====================================================================
%% API functions
%%====================================================================

ensure_ets() ->
    case ets:info(?MODULE, size) of
        undefined ->
            ets:new(?MODULE, [named_table, public, set]);
        _ -> ?MODULE
    end.

init_excel_loader(PythonPid) ->
    ExcelLoader =
        fun(ExcelFile, LoadSheetsOpts) ->
            tdata_excel_loader:load_sheets(PythonPid, ExcelFile, LoadSheetsOpts)
        end,
    set_loader("default", ExcelLoader),
    set_loader(".xls", ExcelLoader),
    set_loader(".xlsx", ExcelLoader),
    ok.

set_loader(Ext, Loader) when is_list(Ext) andalso is_function(Loader, 2) ->
    ets:insert(?MODULE, {Ext, Loader}).

get_loader(Ext) when is_list(Ext) ->
    case ets:lookup(?MODULE, Ext) of
        [{_, Loader}] -> Loader;
        _ -> undefined
    end.

load_input_files({InputFile, LoadSheetsOpts}, InputDir, Acc) ->
    Res = load_input_file(InputFile, LoadSheetsOpts, InputDir),
    [Res | Acc];
load_input_files([{InputFile, LoadSheetsOpts} | InputFiles], InputDir, Acc) ->
    Res = load_input_file(InputFile, LoadSheetsOpts, InputDir),
    load_input_files(InputFiles, InputDir, [Res | Acc]);
load_input_files([], _InputDir, Acc) -> Acc.

%%====================================================================
%% Internal functions
%%====================================================================

load_input_file(InputFile0, LoadSheetsOpts, InputDir) ->
    InputFile = filename:join(InputDir, InputFile0),
    Ext = filename:extension(InputFile),
    Loader =
        case get_loader(Ext) of
            undefined -> get_loader("default");
            Loader0 -> Loader0
        end,
    case Loader(InputFile, LoadSheetsOpts) of
        {ok, SheetsData} ->
            {InputFile0, SheetsData};
        Err ->
            error({load_error, {InputFile0, Err}})
    end.