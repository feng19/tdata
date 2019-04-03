-module(tdata_loader).

%% API
-export([
    start/0,
    stop/0,
    set_loader/1, set_loader/2,
    get_loader/0, get_loader/1,
    del_loader/0, del_loader/1,
    load_input_files/2, load_input_files/3,
    all_attr_modules_app/2,
    all_attr_modules/2,
    all_attr_modules_ebin/1,
    get_ebin_dirs/1
]).

-callback start() -> ok.
-callback stop() -> ok.

%%====================================================================
%% API functions
%%====================================================================

start() ->
    ensure_ets(),
    [Module:start() || Module <- all_attr_modules()],
    ok.

stop() ->
    [Module:stop() || Module <- all_attr_modules()],
    ets:delete(?MODULE),
    ok.

ensure_ets() ->
    case ets:info(?MODULE, size) of
        undefined ->
            ets:new(?MODULE, [named_table, public, set]);
        _ -> ?MODULE
    end.

set_loader(Loader) ->
    set_loader("default", Loader).
set_loader(Ext, Loader) when is_list(Ext) andalso is_function(Loader, 2) ->
    ets:insert(?MODULE, {Ext, Loader}).

get_loader() ->
    get_loader("default").
get_loader(Ext) when is_list(Ext) ->
    case ets:lookup(?MODULE, Ext) of
        [{_, Loader}] -> Loader;
        _ -> undefined
    end.

del_loader() ->
    del_loader("default").
del_loader(Ext) ->
    ets:delete(?MODULE, Ext).

-spec load_input_files(tdata:input_file_define(), InputDir :: file:filename()) ->
    [{tdata:input_file(), OutputData :: any()}].
load_input_files(InputFileDefine, InputDir) ->
    load_input_files(InputFileDefine, InputDir, []).
load_input_files(#{file := InputFile, opts := Opts}, InputDir, Acc) ->
    Res = load_input_file(InputFile, Opts, InputDir),
    [Res | Acc];
load_input_files([#{file := InputFile, opts := Opts} | InputFiles], InputDir, Acc) ->
    Res = load_input_file(InputFile, Opts, InputDir),
    load_input_files(InputFiles, InputDir, [Res | Acc]);
load_input_files([], _InputDir, Acc) -> Acc.

all_attr_modules_app(undefined, _Type) -> [];
all_attr_modules_app(App, Type) ->
    Targets =
        lists:usort(
            lists:append(
                [[Module || Module <- Modules] ||
                    {ok, Modules} <- [application:get_key(App, modules)]])),
    lists:filter(
        fun(Module) ->
            lists:member({tdata, [Type]}, Module:module_info(attributes))
        end, Targets).

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

all_attr_modules_ebin(Type) ->
    {ok, Cwd} = file:get_cwd(),
    EbinDirs = get_ebin_dirs(Cwd),
    Targets = [list_to_atom(filename:basename(File, ".beam")) ||
        EbinDir <- EbinDirs, File <- filelib:wildcard(filename:join(EbinDir, "*.beam"))],
    lists:filter(
        fun(Module) ->
            lists:member({tdata, [Type]}, Module:module_info(attributes))
        end, Targets).

get_ebin_dirs(Cwd) ->
    EbinDirs = [filename:join(Cwd, "ebin") | filelib:wildcard("_build/default/lib/*/ebin")],
    [Dir || Dir <- EbinDirs, filelib:is_dir(Dir)].

%%====================================================================
%% Internal functions
%%====================================================================

all_attr_modules() ->
    App = application:get_env(tdata, app, undefined),
    lists:usort(all_attr_modules_app(App, loader) ++
        all_attr_modules_ebin(loader) ++
        all_attr_modules(behavior, [?MODULE])).

-spec load_input_file(tdata:input_file(), tdata:input_file_opts(), InputDir :: file:filename()) ->
    {tdata:input_file(), OutputData :: any()}.
load_input_file(InputFile0, Opts, InputDir) ->
    InputFile = filename:join(InputDir, InputFile0),
    Ext = filename:extension(InputFile),
    Loader =
        case get_loader(Ext) of
            undefined -> get_loader();
            Loader0 -> Loader0
        end,
    case Loader(InputFile, Opts) of
        {ok, OutputData} ->
            {InputFile0, OutputData};
        Err ->
            error({load_error, {InputFile0, Err}})
    end.