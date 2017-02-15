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
    PythonPath = filename:join(code:priv_dir(rebar3_edata_plugin), "python/"),
    ErlPortPath = filename:join(code:priv_dir(erlport), "python2/"),
    python:start([{cd, ErlPortPath}, {python_path, PythonPath}]).

stop(PythonPid) ->
    python:stop(PythonPid).

transform_files(PythonPid, HandleModule, Config, TransformConfig) when is_atom(HandleModule) ->
    TransformFiles = HandleModule:transform_files(),
    transform_files(PythonPid, TransformFiles, Config, TransformConfig);
transform_files(PythonPid, TransformFiles, Config, TransformConfig) when is_list(TransformFiles) ->
    [transform_file(TransformFile, PythonPid, Config, TransformConfig)
        || TransformFile <- TransformFiles].

transform_file({InputFiles0, OutputFile0, TransformFun}, PythonPid,
    #{input_dir := InputDir, output_dir := OutputDir}, TransformConfig) ->
    InputFiles = [filename:join(InputDir, InputFile) || {InputFile, _} <- InputFiles0],
    OutputFile = filename:join(OutputDir, OutputFile0),
    case is_need_transform(InputFiles, OutputFile, undefined) of
        true ->
            ExcelData = load_excel_files(InputFiles, InputDir, PythonPid, []),
            Data = TransformFun(OutputFile0, ExcelData, TransformConfig),
            tdata_render:render(Data, undefined, undefined, OutputFile);
        false ->
            skipped
    end;
transform_file({InputFiles0, OutputFile0, TplFile, TransformFun}, PythonPid,
    #{input_dir := InputDir, output_dir := OutputDir, template_dir := TemplateDir}, TransformConfig) ->
    InputFiles = [filename:join(InputDir, InputFile) || {InputFile, _} <- InputFiles0],
    OutputFile = filename:join(OutputDir, OutputFile0),
    case is_need_transform(InputFiles, OutputFile, TplFile) of
        true ->
            ExcelData = load_excel_files(InputFiles, InputDir, PythonPid, []),
            Data = TransformFun(OutputFile0, ExcelData, TransformConfig),
            tdata_render:render(Data, TplFile, TemplateDir, OutputFile);
        false ->
            skipped
    end.

%%====================================================================
%% Internal functions
%%====================================================================

is_need_transform(InputFiles, OutputFile, undefined) ->
    Input = lists:max([filelib:last_modified(InputFile) || {InputFile, _} <- InputFiles]),
    Output = filelib:last_modified(OutputFile),
    Input > Output;
is_need_transform(InputFiles, OutputFile, {_, TplFile}) ->
    Input = lists:max([filelib:last_modified(InputFile) || {InputFile, _} <- InputFiles]),
    Output = filelib:last_modified(OutputFile),
    Tpl = filelib:last_modified(TplFile),
    Input > Output orelse Tpl > Output.

load_excel_files([{ExcelFile0, InputLoadConfig} | ExcelFiles], InputDir, PythonPid, Data) ->
    ExcelFile = filename:join(InputDir, ExcelFile0),
    case tdata_sheet:load_sheets(PythonPid, ExcelFile, InputLoadConfig) of
        {ok, SheetsData} ->
            load_excel_files(ExcelFiles, InputDir, PythonPid, [{ExcelFile0, SheetsData} | Data]);
        Err ->
            error(load_sheets_error, {ExcelFile0, Err})
    end;
load_excel_files([], _InputDir, _PythonPid, Data) ->
    Data.