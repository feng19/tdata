-module(tdata).

%% API exports
-export([
    start/0,
    stop/1,
    transform_files/4,
    transform_file/4
]).

-type input_file() :: file:filename().
-type output_file() :: file:filename().
-type input_file_define() :: {input_file(), tdata_sheet:load_sheets_opts()}.
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

-spec start() -> {ok, pid()} | {error, Reason :: term()}.
start() ->
    PythonPath = filename:join(code:priv_dir(?MODULE), "python/"),
    ErlPortPath = filename:join(code:priv_dir(erlport), "python2/"),
    python:start([{cd, ErlPortPath}, {python_path, PythonPath}]).

-spec stop(pid()) -> ok.
stop(PythonPid) ->
    python:stop(PythonPid).

-spec transform_files(PythonPid :: pid(), HandleModule :: module() | transform_define() | transform_defines(),
    global_config(), transform_fun_config()) -> output_files().
transform_files(PythonPid, HandleModule, Config, TransformConfig) when is_atom(HandleModule) ->
    TransformDefines = HandleModule:transform_defines(),
    transform_files(PythonPid, TransformDefines, Config, TransformConfig);
transform_files(PythonPid, TransformDefines, Config, TransformConfig) when is_list(TransformDefines) ->
    [transform_file(TransformDefine, PythonPid, Config, TransformConfig)
        || TransformDefine <- TransformDefines];
transform_files(PythonPid, TransformDefine, Config, TransformConfig) when is_map(TransformDefine) ->
    transform_file(TransformDefine, PythonPid, Config, TransformConfig).

-spec transform_file(transform_define(), PythonPid :: pid(), global_config(), transform_fun_config()) -> output().
transform_file(TransformDefine, PythonPid, Config, TransformConfig) ->
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
                        InputDir, TplType, TplFile, TransformConfig, PythonPid);
                false ->
                    {OutputFile0, skipped}
            end;
        Result -> Result
    end.

%%====================================================================
%% Internal functions
%%====================================================================

transform_file_do(InputFileDefines, OutputFile0, OutputFile, TransformDefine,
    InputDir, TplType, TplFile, TransformConfig, PythonPid) ->
    ExcelDataList = load_excel_files(InputFileDefines, InputDir, PythonPid, []),
    #{transform_fun := TransformFun} = TransformDefine,
    Data = TransformFun(OutputFile0, ExcelDataList, TransformConfig),
    SourceFiles = unicode:characters_to_binary(string:join([Excel||{Excel, _}<-ExcelDataList], ",")),
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

load_excel_files({ExcelFile, LoadSheetsOpts}, InputDir, PythonPid, Acc) ->
    Res = load_excel_file(ExcelFile, LoadSheetsOpts, InputDir, PythonPid),
    [Res | Acc];
load_excel_files([{ExcelFile, LoadSheetsOpts} | ExcelFiles], InputDir, PythonPid, Acc) ->
    Res = load_excel_file(ExcelFile, LoadSheetsOpts, InputDir, PythonPid),
    load_excel_files(ExcelFiles, InputDir, PythonPid, [Res | Acc]);
load_excel_files([], _InputDir, _PythonPid, Acc) -> Acc.

load_excel_file(ExcelFile0, LoadSheetsOpts, InputDir, PythonPid) ->
    ExcelFile = filename:join(InputDir, ExcelFile0),
    case tdata_sheet:load_sheets(PythonPid, ExcelFile, LoadSheetsOpts) of
        {ok, SheetsData} ->
            {ExcelFile0, SheetsData};
        Err ->
            error({load_sheets_error, {ExcelFile0, Err}})
    end.