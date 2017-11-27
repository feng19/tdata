-module(tdata_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() ->
    [
        render
    ].

render(Config) ->
    DataDir = ?config(data_dir, Config),
    OutputDir = filename:join(DataDir, "output"),
    filelib:ensure_dir(filename:join(OutputDir, "temp")),
    Res = lists:map(
        fun(#{output_file := OutputFile0}) ->
            OutputFile = filename:join(OutputDir, OutputFile0),
            filelib:is_file(OutputFile) andalso file:delete(OutputFile),
            {OutputFile0, ok}
        end, transform_module:transform_defines()),
    Res = tdata:transform_files(transform_module,
        #{input_dir => DataDir, output_dir => OutputDir, template_dir => DataDir}, []),
    ok.