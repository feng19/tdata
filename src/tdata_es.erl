-module(tdata_es).

%% API exports
-export([main/1]).

-define(PROGRAM_NAME, "tdata").
-include_lib("stdlib/include/zip.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) -> % load config from tdata.config when no args
    case filelib:is_regular("tdata.config") of
        true ->
            {ok, Config0} = file:consult("tdata.config"),
            do(Config0);
        false ->
            main(["help"])
    end,
    ok;
main(Args) ->
    OptSpecList = [
        {input_dir, $i, "input_dir", {string, "data"}, "Input Dir eg: data or \"data/*\"."},
        {output_dir, $o, "output_dir", {string, "output"}, "Output Dir eg: output."},
        {template_dir, $t, "template_dir", {string, "templates"}, "Template Dir eg: template_dir."},
        {recursive, $r, "recursive", {boolean, false}, "is recursive mode: true or false."},
        {force, $f, "force", {boolean, false}, "force gen all output: true or false."},
        {child_dir, $c, "child_dir", string,
            "if recursive set to false, must set this arguments: '-c child_dir1 -c child_dir2 ...'"},
        {help, $h, "help", undefined, "print help."}
    ],
    case getopt:parse(OptSpecList, Args) of
        {ok, {_Options, ["help"]}} ->
            getopt:usage(OptSpecList, ?PROGRAM_NAME);
        {ok, {Options, _LastString}} ->
            case proplists:is_defined(help, Options) of
                true ->
                    getopt:usage(OptSpecList, ?PROGRAM_NAME);
                _ ->
%%                    io:format("Options:~p, LastString:~p~n", [Options, LastString]),
                    do(Options)
            end;
        {error, {Reason, Data}} ->
            io:format("~p : ~p~n", [Reason, Data]),
            getopt:usage(OptSpecList, ?PROGRAM_NAME)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

do(Config0) ->
    {ok, Cwd} = file:get_cwd(),
    add_paths(Cwd),
    Config = handle_config(Config0, Cwd, []),
    PythonDir = "python2",
    extract_python2(PythonDir),
    tdata_excel_loader:set_python_dir(PythonDir, PythonDir),
    App =
        case maps:get(app, Config, undefined) of
            undefined ->
                case filelib:wildcard("src/*.app.src") of
                    [AppT] -> list_to_atom(filename:basename(AppT, ".app.src"));
                    _ -> error(cannot_find_app)
                end;
            AppT -> AppT
        end,
    {ok, _} = application:ensure_all_started(App),
    application:set_env(tdata, app, App),
    tdata:start(),
    HandleModules = all_attr_modules(),
    case maps:get(recursive, Config, false) of
        false ->
            IsForce = maps:get(force, Config, false),
            OutputDir = maps:get(output_dir, Config),
            cleanup_dir(OutputDir, IsForce),
            loop_transform(HandleModules, Config);
        InputDirs ->
            do_recursive_dir(HandleModules, InputDirs, Config)
    end,
    tdata:stop().

extract_python2(PythonDir) ->
    ScriptName = filename:absname(escript:script_name()),
    case filelib:is_regular(ScriptName) of
        true ->
            {ok, Escript} = escript:extract(ScriptName, []),
            {archive, Archive} = lists:keyfind(archive, 1, Escript),
            zip:extract(Archive, [keep_old_files, {file_filter,
                fun(#zip_file{name = FileName}) ->
                    hd(filename:split(FileName)) == PythonDir
                end}]);
        false ->
            io:format("~p~n", [?LINE]),
            skip
    end,
    case filelib:is_dir(PythonDir) of
        true -> ok;
        false ->
            error({miss_python_dir, PythonDir})
    end.

all_attr_modules() ->
    App = application:get_env(tdata, app),
    lists:usort(tdata_loader:all_attr_modules(App) ++ tdata_loader:all_attr_modules(behavior, [tdata])).

do_recursive_dir(HandleModules, InputDirs, Config) ->
    IsForce = maps:get(force, Config, false),
    [transform(InputDir, HandleModules, Config, IsForce) || InputDir <- InputDirs].

transform(InputDir, HandleModules, Config, IsForce) ->
    case filelib:is_dir(InputDir) of
        true ->
            io:format("[input_dir:~ts] transforming...~n", [InputDir]),
            OutputDir = filename:join([maps:get(output_dir, Config), filename:basename(InputDir)]),
            cleanup_dir(OutputDir, IsForce),
            ensure_dir(OutputDir),
            NewConfig = Config#{input_dir => InputDir, output_dir => OutputDir},
            loop_transform(HandleModules, NewConfig),
            io:format("[input_dir:~ts] done~n", [InputDir]);
        false ->
            io:format("[input_dir:~ts] shouldn't exist~n", [InputDir])
    end.

loop_transform([HandleModule | HandleModules], Config) ->
    io:format("  [~p] transforming...~n", [HandleModule]),
    ResList = tdata:transform_files(HandleModule, Config, Config),
    [io:format("  ==> ~ts:~p~n", [OutputFile, Res]) || {OutputFile, Res} <- ResList],
    loop_transform(HandleModules, Config);
loop_transform([], _Config) -> ok.

handle_config([{input_dir, InputDir} | Config], Cwd, Acc) ->
    handle_config(Config, Cwd, [{input_dir, filename:join(Cwd, InputDir)} | Acc]);
handle_config([{output_dir, OutputDir0} | Config], Cwd, Acc) ->
    OutputDir = filename:join(Cwd, OutputDir0),
    ensure_dir(OutputDir),
    handle_config(Config, Cwd, [{output_dir, OutputDir} | Acc]);
handle_config([{template_dir, TemplateDir0} | Config], Cwd, Acc) ->
    TemplateDir = filename:join(Cwd, TemplateDir0),
    handle_config(Config, Cwd, [{template_dir, TemplateDir} | Acc]);
handle_config([{child_dir, ChildDir} | Config], Cwd, Acc) ->
    case lists:keytake(child_dirs, 1, Acc) of
        {value, {_, ChildDirs}, AccTemp} ->
            handle_config(Config, Cwd, [{child_dirs, [ChildDir | ChildDirs]} | AccTemp]);
        false ->
            handle_config(Config, Cwd, [{child_dirs, [ChildDir]} | Acc])
    end;
handle_config([Other | Config], Cwd, Acc) ->
    handle_config(Config, Cwd, [Other | Acc]);
handle_config([], _Cwd, Acc0) -> % last
    Acc =
        case lists:keytake(recursive, 1, Acc0) of
            {value, {recursive, true}, Acc1} ->
                InputDirs = recursive_input_dir(Acc1),
                [{recursive, InputDirs} | Acc1];
            _ ->
                case lists:keytake(child_dirs, 1, Acc0) of
                    {value, {child_dirs, ChildDirs0}, AccTemp} ->
                        InputDir = proplists:get_value(input_dir, AccTemp),
                        InputDirs = [filename:join(InputDir, ChildDir) || ChildDir <- ChildDirs0],
                        lists:keystore(recursive, 1, AccTemp, {recursive, InputDirs});
                    false -> Acc0
                end
        end,
    maps:from_list(Acc).

recursive_input_dir(Config) ->
    InputDir0 = proplists:get_value(input_dir, Config),
    InputDir =
        case [lists:last(InputDir0)] of
            "*" -> InputDir0;
            _ -> lists:concat([InputDir0, "/*"])
        end,
    filelib:wildcard(InputDir).

cleanup_dir(OutputDir, true) ->
    os:cmd("rm -rf " ++ OutputDir);
cleanup_dir(_OutputDir, _IsForce) -> ok.

ensure_dir(Dir) ->
    ok = filelib:ensure_dir(filename:join(Dir, "temp")).

add_paths(Cwd) ->
    code:add_paths([Dir || Dir <- [
        filename:join(Cwd, "ebin") |
        filelib:wildcard("_build/default/lib/*/ebin")], filelib:is_dir(Dir)
    ]),
    ok.