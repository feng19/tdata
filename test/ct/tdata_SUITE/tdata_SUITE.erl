-module(tdata_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() ->
    [
        render,
        render_output_list
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

render_output_list(Config) ->
    DataDir = ?config(data_dir, Config),
    OutputDir = filename:join(DataDir, "output"),
    filelib:ensure_dir(filename:join(OutputDir, "temp")),
    Res = lists:map(
        fun(OutputFile0) ->
            OutputFile = filename:join(OutputDir, OutputFile0),
            filelib:is_file(OutputFile) andalso file:delete(OutputFile),
            {OutputFile0, ok}
        end, ["transform_dynamic_1.data", "transform_dynamic_2.data", "transform_dynamic_3.data"]),
    Res = tdata:transform_files(transform_defines(),
        #{input_dir => DataDir, output_dir => OutputDir, template_dir => DataDir}, []),
    ok.


transform_defines() ->
    [
        #{input_file_defines => #{file => "transform_module.xlsx", opts => #{all => #{}}},
            output_file => dynamic,
            transform_fun => fun transform_dynamic_fun/3
        }
    ].

transform_dynamic_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := _Rows}}}], _TransformConfig) ->
    [
        {ok, "transform_dynamic_1.data", "1"},
        {ok, "transform_dynamic_2.data", "2"},
        {ok, "transform_dynamic_3.data", "3"}
    ].