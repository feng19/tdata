-module(tdata).

%% API exports
-export([
    start/0,
    stop/1,
    transform_files/4,
    transform_file/4
]).

%%====================================================================
%% API functions
%%====================================================================
start() ->
    PythonPath = filename:join(code:priv_dir(?MODULE), "python/"),
    ErlPortPath = filename:join(code:priv_dir(erlport), "python2/"),
    python:start([{cd, ErlPortPath}, {python_path, PythonPath}]).

stop(PythonPid) ->
    python:stop(PythonPid).

transform_files(PythonPid, HandleModule, Config, TransformConfig) when is_atom(HandleModule) ->
    TransformDefines = HandleModule:transform_defines(),
    transform_files(PythonPid, TransformDefines, Config, TransformConfig);
transform_files(PythonPid, TransformDefines, Config, TransformConfig) when is_list(TransformDefines) ->
    [transform_file(TransformDefine, PythonPid, Config, TransformConfig)
        || TransformDefine <- TransformDefines];
transform_files(PythonPid, TransformDefine, Config, TransformConfig) when is_tuple(TransformDefine) ->
    transform_file(TransformDefine, PythonPid, Config, TransformConfig).

transform_file(TransformDefine, PythonPid,
    #{input_dir := InputDir, output_dir := OutputDir} = Config, TransformConfig) ->
    {InputFiles0, OutputFile0, TplFile, TransformFun, TemplateDir} =
        case TransformDefine of
            {InputFilesTemp, OutputFileTemp, TransformFunTemp} ->
                {InputFilesTemp, OutputFileTemp, undefined, TransformFunTemp, undefined};
            {InputFilesTemp, OutputFileTemp, TplFileTemp, TransformFunTemp} ->
                TemplateDirTemp = maps:get(template_dir, Config),
                {InputFilesTemp, OutputFileTemp, TplFileTemp, TransformFunTemp, TemplateDirTemp}
        end,
    InputFiles =
        if
            is_list(InputFiles0) ->
                [filename:join(InputDir, InputFile) || {InputFile, _} <- InputFiles0];
            is_tuple(InputFiles0) andalso tuple_size(InputFiles0) == 2 ->
                {InputFile, _} = InputFiles0,
                [filename:join(InputDir, InputFile)];
            true ->
                error({error_input_files, InputFiles0})
        end,
    OutputFile = filename:join(OutputDir, OutputFile0),
    case is_need_transform(InputFiles, OutputFile, undefined) of
        true ->
            ExcelData = load_excel_files(InputFiles0, InputDir, PythonPid, []),
            Data = TransformFun(OutputFile0, ExcelData, TransformConfig),
            case tdata_render:render(Data, TplFile, TemplateDir, OutputFile) of
                ok ->
                    {OutputFile0, ok};
                Err ->
                    {OutputFile0, Err}
            end;
        false ->
            {OutputFile0, skipped}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

is_need_transform(InputFiles, OutputFile, TplFile0) ->
    Input = lists:max([filelib:last_modified(InputFile) || InputFile <- InputFiles]),
    Output = filelib:last_modified(OutputFile),
    case TplFile0 of
        undefined ->
            Input > Output;
        {_, TplFile} ->
            Tpl = filelib:last_modified(TplFile),
            Input > Output orelse Tpl > Output
    end.

load_excel_files({ExcelFile, LoadSheetsConfig}, InputDir, PythonPid, Data) ->
    Res = load_excel_file(ExcelFile, LoadSheetsConfig, InputDir, PythonPid),
    [Res | Data];
load_excel_files([{ExcelFile, LoadSheetsConfig} | ExcelFiles], InputDir, PythonPid, Data) ->
    Res = load_excel_file(ExcelFile, LoadSheetsConfig, InputDir, PythonPid),
    load_excel_files(ExcelFiles, InputDir, PythonPid, [Res | Data]);
load_excel_files([], _InputDir, _PythonPid, Data) ->
    Data.

load_excel_file(ExcelFile0, LoadSheetsConfig, InputDir, PythonPid) ->
    ExcelFile = filename:join(InputDir, ExcelFile0),
    case tdata_sheet:load_sheets(PythonPid, ExcelFile, LoadSheetsConfig) of
        {ok, SheetsData} ->
            {ExcelFile0, SheetsData};
        Err ->
            error({load_sheets_error, {ExcelFile0, Err}})
    end.