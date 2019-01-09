-module(transform_module).
-behavior(tdata).

%% API
-export([
    transform_defines/0
]).

transform_defines() ->
    [
        #{input_file_defines => #{file => "transform_module.xlsx", opts => #{all => #{}}},
            output_file => "transform_module.data",
            transform_fun => fun transform_fun/3
        },
        #{input_file_defines => #{file => "transform_module.xlsx", opts => #{all => #{only_rows => true}}},
            output_file => "transform_module_only_rows.data",
            transform_fun => fun transform_only_rows_fun/3
        },
        #{input_file_defines => #{file => "transform_module.xlsx", opts => #{all => #{}}},
            output_file => "transform_module_mustache.data",
            transform_fun => fun transform_mustache_fun/3,
            tpl_type => mustache,
            tpl_file => "transform_module.tpl"
        },
        #{input_file_defines => [#{file => "transform_module.xlsx", opts => #{all => #{}}}],
            output_file => "transform_module_mustache_no_tpl_type.data",
            transform_fun => fun transform_mustache_fun/3,
            tpl_file => "transform_module.tpl"
        },
        #{input_file_defines => #{file => "transform_module.xlsx", opts => #{all => #{}}},
            output_file => "transform_module_dtl.data",
            transform_fun => fun transform_dtl_fun/3,
            tpl_type => dtl,
            tpl_file => "transform_module.dtl"
        },
        #{input_file_defines => #{file => "transform_module.xlsx", opts => #{all => #{}}},
            output_file => "transform_module_dtl_auto_tpl_type.data",
            transform_fun => fun transform_dtl_fun/3,
            tpl_type => auto,
            tpl_file => "transform_module.dtl"
        }
    ].

transform_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := Rows}}}], _TransformConfig) ->
    {ok, io_lib:format("~p", [Rows])}.

transform_only_rows_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := Rows}}], _TransformConfig) ->
    {ok, io_lib:format("~p", [Rows])}.

transform_mustache_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := Rows}}}], _TransformConfig) ->
    {ok, [{rows, Rows}]}.

transform_dtl_fun(_OutputFile, [{"transform_module.xlsx", #{<<"Sheet1">> := #{rows := Rows}}}], _TransformConfig) ->
    {ok, [{rows, Rows}]}.