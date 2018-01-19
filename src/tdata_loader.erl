-module(tdata_loader).

%% API
-export([
    start/0,
    stop/0,
    set_loader/2,
    get_loader/1,
    del_loader/1,
    load_input_files/3,
    all_attr_modules/2
]).

-callback start() -> ok.
-callback stop() -> ok.

%%====================================================================
%% API functions
%%====================================================================

start() ->
    ensure_ets(),
    [Module:start()||Module <- all_attr_modules(behavior, [?MODULE])],
    ok.

stop() ->
    ets:delete(?MODULE),
    [Module:stop()||Module <- all_attr_modules(behavior, [?MODULE])],
    ok.

ensure_ets() ->
    case ets:info(?MODULE, size) of
        undefined ->
            ets:new(?MODULE, [named_table, public, set]);
        _ -> ?MODULE
    end.

set_loader(Ext, Loader) when is_list(Ext) andalso is_function(Loader, 2) ->
    ets:insert(?MODULE, {Ext, Loader}).

get_loader(Ext) when is_list(Ext) ->
    case ets:lookup(?MODULE, Ext) of
        [{_, Loader}] -> Loader;
        _ -> undefined
    end.

del_loader(Ext) ->
    ets:delete(?MODULE, Ext).

load_input_files({InputFile, LoadSheetsOpts}, InputDir, Acc) ->
    Res = load_input_file(InputFile, LoadSheetsOpts, InputDir),
    [Res | Acc];
load_input_files([{InputFile, LoadSheetsOpts} | InputFiles], InputDir, Acc) ->
    Res = load_input_file(InputFile, LoadSheetsOpts, InputDir),
    load_input_files(InputFiles, InputDir, [Res | Acc]);
load_input_files([], _InputDir, Acc) -> Acc.

all_attr_modules(Attr, Value) ->
    Targets =
        lists:usort(
            lists:append(
                [[Module || Module <- Modules] ||
                    {App, _, _} <- application:loaded_applications(),
                    {ok, Modules} <- [application:get_key(App, modules)]])),
    lists:filter(
        fun(Module) ->
            lists:member({Attr, Value}, Module:module_info(attributes))
        end, Targets).

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