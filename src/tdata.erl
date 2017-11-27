-module(tdata).

%% API exports
-export([
    start/0,
    init_excel_loader/0,
    init_excel_loader/2,
    stop/0,
    get_python_pid/0,
    transform_files/3,
    transform_file/3
]).

-type input_file() :: file:filename().
-type output_file() :: file:filename().
-type input_file_define() :: {input_file(), tdata_excel_loader:load_sheets_opts()}.
-type transform_define() :: #{
    input_file_defines := input_file_define() | [input_file_define()],
    output_file := output_file(),
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
-type output() :: {output_file(), output_status()}.
-type output_files() :: [output()].

-callback transform_defines() -> [transform_define()].

%%====================================================================
%% API functions
%%====================================================================

start() ->
    ensure_ets(),
    tdata_loader:ensure_ets(),
    ok.

-spec init_excel_loader() -> ok.
init_excel_loader() ->
    PythonPath = filename:join(code:priv_dir(?MODULE), "python/"),
    ErlPortPath = filename:join(code:priv_dir(erlport), "python2/"),
    init_excel_loader(ErlPortPath, PythonPath).
-spec init_excel_loader(file:filename(), file:filename()) -> {ok, pid()} | {error, Reason :: term()}.
init_excel_loader(ErlPortPath, PythonPath) ->
    {ok, PythonPid} = python:start([{cd, ErlPortPath}, {python_path, PythonPath}]),
    tdata_loader:init_excel_loader(PythonPid),
    ets:insert(?MODULE, {python_pid, PythonPid}),
    ok.

-spec stop() -> ok.
stop() ->
    case get_python_pid() of
        undefined -> ok;
        PythonPid ->
            python:stop(PythonPid)
    end,
    ets:delete(?MODULE),
    ets:delete(tdata_loader),
    ok.

-spec get_python_pid() -> pid() | undefined.
get_python_pid() ->
    case ets:lookup(?MODULE, python_pid) of
        [{_, PythonPid}] -> PythonPid;
        _ -> undefined
    end.

-spec transform_files(HandleModule :: module() | transform_define() | transform_defines(),
    global_config(), transform_fun_config()) -> output_files().
transform_files(HandleModule, Config, TransformConfig) when is_atom(HandleModule) ->
    TransformDefines = HandleModule:transform_defines(),
    transform_files(TransformDefines, Config, TransformConfig);
transform_files(TransformDefines, Config, TransformConfig) when is_list(TransformDefines) ->
    [transform_file(TransformDefine, Config, TransformConfig)
        || TransformDefine <- TransformDefines];
transform_files(TransformDefine, Config, TransformConfig) when is_map(TransformDefine) ->
    transform_file(TransformDefine, Config, TransformConfig).

-spec transform_file(transform_define(), global_config(), transform_fun_config()) -> output().
transform_file(TransformDefine, Config, TransformConfig) ->
    case check_transform_define(TransformDefine) of
        ok ->
            #{input_file_defines := InputFileDefines, output_file := OutputFile0} = TransformDefine,
            #{input_dir := InputDir, output_dir := OutputDir} = Config,

            InputFiles = get_input_files_from_defines(InputFileDefines, InputDir),
            OutputFile = filename:join(OutputDir, OutputFile0),
            ok = filelib:ensure_dir(OutputFile),
            {TplType, TplFile} = get_tpl_info(TransformDefine, Config),
            case is_need_transform(InputFiles, OutputFile, TplFile) of
                true ->
                    transform_file_do(InputFileDefines, OutputFile0, OutputFile, TransformDefine,
                        InputDir, TplType, TplFile, TransformConfig);
                false ->
                    {OutputFile0, skipped}
            end;
        Result -> Result
    end.

%%====================================================================
%% Internal functions
%%====================================================================

ensure_ets() ->
    case ets:info(?MODULE, size) of
        undefined ->
            ets:new(?MODULE, [named_table, public, set]);
        _ -> ?MODULE
    end.

transform_file_do(InputFileDefines, OutputFile0, OutputFile, TransformDefine,
    InputDir, TplType, TplFile, TransformConfig) ->
    InputDataList = tdata_loader:load_input_files(InputFileDefines, InputDir, []),
    #{transform_fun := TransformFun} = TransformDefine,
    Data = TransformFun(OutputFile0, InputDataList, TransformConfig),
    SourceFiles = unicode:characters_to_binary(string:join([InputFile || {InputFile, _} <- InputDataList], ",")),
    HeaderComments = <<"%% Automatically generated, do not edit\n%% Source Files: ", SourceFiles/binary, "\n">>,

    case tdata_render:render(Data, TplType, TplFile, OutputFile, HeaderComments) of
        ok -> {OutputFile0, ok};
        Err -> {OutputFile0, Err}
    end.

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
check_transform_define(_TransformDefine) ->
    error(miss_output_file).

get_input_files_from_defines({InputFile, _}, InputDir) ->
    [filename:join(InputDir, InputFile)];
get_input_files_from_defines(InputFileDefines, InputDir) when is_list(InputFileDefines) ->
    [filename:join(InputDir, InputFile) || {InputFile, _} <- InputFileDefines];
get_input_files_from_defines(InputFileDefines, _InputDir) ->
    error({error_input_file_defines, InputFileDefines}).

get_tpl_info(TransformDefine, Config) ->
    case maps:get(tpl_file, TransformDefine, miss) of
        miss ->
            {undefined, undefined};
        TplFile0 ->
            TplType0 = get_tpl_type(TplFile0, TransformDefine),
            TplDir = get_tpl_dir(TransformDefine, Config),
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

is_need_transform(InputFiles, OutputFile, TplFile0) ->
    Input = lists:max([filelib:last_modified(InputFile) || InputFile <- InputFiles]),
    Output = filelib:last_modified(OutputFile),
    case TplFile0 of
        undefined ->
            Input > Output;
        TplFile ->
            Tpl = filelib:last_modified(TplFile),
            Input > Output orelse Tpl > Output
    end.