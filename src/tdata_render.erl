-module(tdata_render).

%% API
-export([
    render/4
]).

-export_type([tpl_type/0]).
-type tpl_type() :: auto | mustache | dtl.

%%====================================================================
%% API functions
%%====================================================================

-spec render({ok, any()} | tuple(), tpl_type(), TplFile :: file:filename(),
    OutputFile :: file:filename()) -> ok | tuple().
render(skipped, _TplType, _TplFile, _OutputFile) -> skipped;
render({ok, RenderData}, mustache, TplFile, OutputFile) ->
    render_mustache(TplFile, OutputFile, RenderData, [{key_type, atom}]);
render({ok, RenderData, RenderOptions}, mustache, TplFile, OutputFile) ->
    render_mustache(TplFile, OutputFile, RenderData, RenderOptions);
render({ok, RenderData}, dtl, TplFile, OutputFile) ->
    render_dtl(TplFile, OutputFile, RenderData, []);
render({ok, RenderData, RenderOptions}, dtl, TplFile, OutputFile) ->
    render_dtl(TplFile, OutputFile, RenderData, RenderOptions);
render({ok, ExportList, BodyIoList}, _TplType, _TplFile, OutputFile) ->
    render_erl(OutputFile, ExportList, BodyIoList);
render({ok, IoList}, _TplType, _TplFile, OutputFile) ->
    write_file(OutputFile, IoList);
render(Err, _TplType, _TplFile, _OutputFile) -> Err.

%%====================================================================
%% Internal functions
%%====================================================================

render_mustache(TplFile, TargetFile, RenderData, RenderOptions) ->
    case file:read_file(TplFile) of
        {ok, RenderBin} ->
            IoList = bbmustache:render(RenderBin, RenderData, RenderOptions),
            write_file(TargetFile, IoList);
        Err -> Err
    end.

render_erl(TargetFile, ExportList, BodyIoList) ->
    TargetModule = filename:rootname(filename:basename(TargetFile)),
    IoList = [
        gen_module_header(TargetModule),
        gen_export(ExportList),
        BodyIoList
    ],
    write_file(TargetFile, IoList).

render_dtl(TplFile, TargetFile, RenderData, RenderOptions) ->
    ModuleName = filename:rootname(filename:basename(TplFile))++"_dtl",
    case erlydtl:compile_file(TplFile, ModuleName, [binary]) of
        {ok, Module} ->
            {ok, IoList} = Module:render(RenderData, RenderOptions),
            write_file(TargetFile, IoList);
        {ok, Module, _} ->
            {ok, IoList} = Module:render(RenderData, RenderOptions),
            write_file(TargetFile, IoList);
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