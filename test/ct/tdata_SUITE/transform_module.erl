-module(transform_module).

%% API
-export([
    transform_defines/0
]).

transform_defines() ->
    [
        {{"transform_module.xlsx", [{sheet_name, all, []}]}, "transform_module.data", fun transform_fun/3},
        {{"transform_module.xlsx", [{sheet_name, all, []}]}, "transform_module_mustache.data",
            {mustache, "transform_module.tpl"}, fun transform_mustache_fun/3},
        {{"transform_module.xlsx", [{sheet_name, all, []}]}, "transform_module_dtl.data",
            {dtl, "transform_module.dtl"}, fun transform_dtl_fun/3}
    ].

transform_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := Rows}}}], _TransformConfig) ->
    {ok, io_lib:format("~p", [Rows])}.

transform_mustache_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := Rows}}}], _TransformConfig) ->
    {ok, [{rows, Rows}]}.

transform_dtl_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := Rows}}}], _TransformConfig) ->
    {ok, [{rows, Rows}]}.