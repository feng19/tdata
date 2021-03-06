-module(tdata_render).

%% API
-export([
    start/0,
    render/5
]).

-export_type([tpl_type/0]).
-type tpl_type() :: auto | mustache | dtl.

%%====================================================================
%% API functions
%%====================================================================

start() ->
    ets:new(?MODULE, [named_table, public, set, {read_concurrency, true}]),
    ok.

-spec render({ok, any()} | tuple() | atom(), tpl_type(), TplFile :: file:filename(),
    OutputFile :: file:filename(), HeaderComments :: binary()) -> ok | tuple().
render(skipped, _TplType, _TplFile, _OutputFile, _HeaderComments) -> skipped;
render({ok, RenderData}, mustache, TplFile, OutputFile, HeaderComments) ->
    render_mustache(TplFile, OutputFile, HeaderComments, RenderData, [{key_type, atom}]);
render({ok, RenderData, RenderOptions}, mustache, TplFile, OutputFile, HeaderComments) ->
    render_mustache(TplFile, OutputFile, HeaderComments, RenderData, RenderOptions);
render({ok, RenderData}, dtl, TplFile, OutputFile, HeaderComments) ->
    render_dtl(TplFile, OutputFile, HeaderComments, RenderData, []);
render({ok, RenderData, RenderOptions}, dtl, TplFile, OutputFile, HeaderComments) ->
    render_dtl(TplFile, OutputFile, HeaderComments, RenderData, RenderOptions);
render({ok, ExportList, BodyIoList}, _TplType, _TplFile, OutputFile, HeaderComments) ->
    render_erl(OutputFile, HeaderComments, ExportList, BodyIoList);
render({ok, IoList}, _TplType, _TplFile, OutputFile, HeaderComments) ->
    write_file(OutputFile, [HeaderComments | IoList]);
render(Err, _TplType, _TplFile, _OutputFile, _HeaderComments) -> Err.

%%====================================================================
%% Internal functions
%%====================================================================

render_mustache(TplFile, TargetFile, HeaderComments, RenderData, RenderOptions) ->
    case get_file(TplFile) of
        {ok, RenderBin} ->
            IoList = bbmustache:render(RenderBin, RenderData, RenderOptions),
            write_file(TargetFile, [HeaderComments | IoList]);
        Err -> Err
    end.

get_file(TplFile) ->
    case get(TplFile) of
        undefined ->
            case ets:lookup(?MODULE, TplFile) of
                [{_, RenderBin}] ->
                    put(TplFile, RenderBin),
                    {ok, RenderBin};
                _ ->
                    case file:read_file(TplFile) of
                        {ok, RenderBin} ->
                            put(TplFile, RenderBin),
                            ets:insert(?MODULE, {TplFile, RenderBin}),
                            {ok, RenderBin};
                        Err -> Err
                    end
            end;
        RenderBin -> {ok, RenderBin}
    end.

render_erl(TargetFile, HeaderComments, ExportList, BodyIoList) ->
    TargetModule = filename:rootname(filename:basename(TargetFile)),
    IoList = [
        gen_module_header(TargetModule),
        gen_export(ExportList),
        BodyIoList
    ],
    write_file(TargetFile, [HeaderComments | IoList]).

render_dtl(TplFile, TargetFile, HeaderComments, RenderData, RenderOptions) ->
    ModuleName = filename:rootname(filename:basename(TplFile)) ++ "_dtl",
    case erlydtl:compile_file(TplFile, ModuleName, [binary]) of
        {ok, Module} ->
            {ok, IoList} = Module:render(RenderData, RenderOptions),
            write_file(TargetFile, [HeaderComments | IoList]);
        {ok, Module, _} ->
            {ok, IoList} = Module:render(RenderData, RenderOptions),
            write_file(TargetFile, [HeaderComments | IoList]);
        Err -> Err
    end.

write_file(TargetFile, IoList) ->
    Binary = iolist_to_binary(IoList),
    case file:write_file(TargetFile, Binary) of
        ok -> ok;
        Err -> Err
    end.

gen_module_header(Module) ->
    ["-module(", Module, ").\n"].

gen_export(ExportList) ->
    Body = lists:map(
        fun({FunName, Arity}) ->
            gen_export_line(FunName, Arity)
        end,
        ExportList),
    IoList = lists:join(",\n", Body),
    [
        "-export([\n",
        lists:reverse(IoList),
        "\n]).\n"
    ].
gen_export_line(FunName, Arity) ->
    lists:concat(["    ", FunName, "/", Arity]).