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
        || TransformDefine <- TransformDefines].

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
    InputFiles = [filename:join(InputDir, InputFile) || {InputFile, _} <- InputFiles0],
    OutputFile = filename:join(OutputDir, OutputFile0),
    case is_need_transform(InputFiles, OutputFile, undefined) of
        true ->
            ExcelData = load_excel_files(InputFiles0, InputDir, PythonPid, []),
            Data = TransformFun(OutputFile0, ExcelData, TransformConfig),
            tdata_render:render(Data, TplFile, TemplateDir, OutputFile);
        false ->
            skipped
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

load_excel_files([{ExcelFile0, LoadSheetsConfig} | ExcelFiles], InputDir, PythonPid, Data) ->
    ExcelFile = filename:join(InputDir, ExcelFile0),
    case tdata_sheet:load_sheets(PythonPid, ExcelFile, LoadSheetsConfig) of
        {ok, SheetsData} ->
            load_excel_files(ExcelFiles, InputDir, PythonPid, [{ExcelFile0, SheetsData} | Data]);
        Err ->
            error(load_sheets_error, {ExcelFile0, Err})
    end;
load_excel_files([], _InputDir, _PythonPid, Data) ->
    Data.