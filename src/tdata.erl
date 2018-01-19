-module(tdata).

%% API exports
-export([
    start/0,
    start/2,
    stop/0,
    set_key/2,
    get_key/1,
    transform_all/2,
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

-spec transform_all(global_config(), transform_fun_config()) -> output_files().
transform_all(Config, TransformConfig) ->
    [transform_files(HandleModule, Config, TransformConfig) ||
        HandleModule <- tdata_loader:all_attr_modules(behavior, [?MODULE])].

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