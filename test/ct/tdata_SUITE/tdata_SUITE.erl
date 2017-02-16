-module(tdata_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() ->
    [
        render
    ].

render(Config) ->
    {ok, PythonPid} = tdata:start(),
    DataDir = ?config(data_dir, Config),
    OutputDir = filename:join(DataDir, "output"),
    filelib:ensure_dir(filename:join(OutputDir, "temp")),
    lists:foreach(
        fun(TransformDefine) ->
            OutputFile = filename:join(OutputDir, element(2, TransformDefine)),
            filelib:is_file(OutputFile) andalso file:delete(OutputFile)
        end, transform_module:transform_defines()),
    tdata:transform_files(PythonPid, transform_module,
        #{input_dir => DataDir, output_dir => OutputDir, template_dir => DataDir}, []),
    tdata:stop(PythonPid),
    ok.