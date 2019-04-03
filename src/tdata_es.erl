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
            io:setopts([{encoding, unicode}]),
            {ok, Config0} = file:consult("tdata.config"),
            do(Config0);
        false ->
            main(["help"])
    end,
    ok;
main(Args) ->
    io:setopts([{encoding, unicode}]),
    UsageOptSpecList = opt_spec_list(usage),
    OptSpecList = opt_spec_list(parse),
    case getopt:parse(OptSpecList, Args) of
        {ok, {_Options, ["help"]}} ->
            getopt:usage(UsageOptSpecList, ?PROGRAM_NAME);
        {ok, {_Options, ["version"]}} ->
            print_version();
        {ok, {Options, _LastString}} ->
            case do_other_cmd(UsageOptSpecList, Options) of
                no_match ->
                    Config = get_config(Options),
                    do(Config);
                Res -> Res
            end;
        {error, {Reason, Data}} ->
            cf:print("~!r~p : ~p~n", [Reason, Data]),
            getopt:usage(UsageOptSpecList, ?PROGRAM_NAME)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get_config(Options) ->
    DefaultConfig = [
        {input_dir, "data"}, {output_dir, "output"}, {template_dir, "templates"},
        {recursive, false}, {force, false}, {config_file, "tdata.config"}
    ],
    ConfigFile = proplists:get_value(config_file, Options, "tdata.config"),
    case filelib:is_regular(ConfigFile) of
        true ->
            {ok, Config0} = file:consult(ConfigFile),
            merge_config([DefaultConfig, Config0, Options]);
        false ->
            merge_config([DefaultConfig, Options])
    end.

merge_config(ConfigList) ->
    merge_config(ConfigList, #{}).
merge_config([Config | List], Map) ->
    NewMap = maps:merge(Map, maps:from_list(Config)),
    merge_config(List, NewMap);
merge_config([], Map) -> maps:to_list(Map).

opt_spec_list(usage) ->
    opt_spec_list({string, "data"}, {string, "output"}, {string, "templates"},
        {boolean, false}, {boolean, false}, {string, "tdata.config"});
opt_spec_list(_) ->
    opt_spec_list(string, string, string, boolean, boolean, string).

opt_spec_list(InputDir, OutputDir, TemplateDir, Recursive, Force, ConfigFile) ->
    [
        {input_dir, $i, "input_dir", InputDir, "Input directory eg: data or \"data/*\"."},
        {output_dir, $o, "output_dir", OutputDir, "Output Dir eg: output."},
        {template_dir, $t, "template_dir", TemplateDir, "Template directory eg: template_dir."},
        {recursive, $r, "recursive", Recursive, "Recursive search ChildDir of InputDir: true or false."},
        {force, $f, "force", Force, "Force gen all output: true or false."},
        {child_dir, $c, "child_dir", string, "Specify ChildDir of InputDir: '-c child_dir1 -c child_dir2 ...'"},
        {config_file, $F, "config_file", ConfigFile, "ConfigFile eg: tdata.config."},
        {app, $a, "app", string, "Which application want to start."},
        {help, $h, "help", undefined, "Print help."},
        {version, $v, "version", undefined, "Print version."}
    ].

do_other_cmd(OptSpecList, Options) ->
    case proplists:is_defined(help, Options) of
        true ->
            getopt:usage(OptSpecList, ?PROGRAM_NAME);
        _ ->
            case proplists:is_defined(version, Options) of
                true ->
                    print_version();
                _ ->
                    no_match
            end
    end.

do(Config0) ->
    {ok, Cwd} = file:get_cwd(),
    Config = handle_config(Config0, Cwd, []),
    cf:print("~!gConfig: ~p~n", [Config]),
    PythonDir = "python2",
    extract_python2(PythonDir),
    tdata_excel_loader:set_python_dir(PythonDir, PythonDir),
    load_beam(Cwd, Config),
    DefineModules =
        case all_define_modules() of
            [] ->
                cf:print("~!rCan't find define modules~n"),
                init:stop();
            DefineModules0 -> DefineModules0
        end,
    tdata:start(),
    case maps:get(recursive, Config, false) of
        false ->
            loop_transform(DefineModules, Config);
        InputDirs ->
            do_recursive_dir(DefineModules, InputDirs, Config)
    end,
    tdata:stop().

extract_python2(PythonDir) ->
    ScriptName = filename:absname(escript:script_name()),
    case filelib:is_regular(ScriptName) of
        true ->
            {ok, EScript} = escript:extract(ScriptName, []),
            {archive, Archive} = lists:keyfind(archive, 1, EScript),
            zip:extract(Archive, [keep_old_files, {file_filter,
                fun(#zip_file{name = FileName}) ->
                    hd(filename:split(FileName)) == PythonDir
                end}]);
        false ->
            skip
    end,
    case filelib:is_dir(PythonDir) of
        true -> ok;
        false ->
            cf:print("~!rMiss python dir: ~ts~n", [PythonDir]),
            init:stop()
    end.

all_define_modules() ->
    App = application:get_env(tdata, app, undefined),
    lists:usort(tdata_loader:all_attr_modules_app(App, ds) ++
        tdata_loader:all_attr_modules_ebin(ds) ++
        tdata_loader:all_attr_modules(behavior, [tdata])).

do_recursive_dir(DefineModules, InputDirs, Config) ->
    [transform(InputDir, DefineModules, Config) || InputDir <- InputDirs].

transform(InputDir, DefineModules, Config) ->
    case filelib:is_dir(InputDir) of
        true ->
            cf:print("~!g[input_dir:~ts] transforming...~n", [InputDir]),
            OutputDir = filename:join([maps:get(output_dir, Config), filename:basename(InputDir)]),
            ensure_dir(OutputDir),
            NewConfig = Config#{input_dir => InputDir, output_dir => OutputDir},
            loop_transform(DefineModules, NewConfig),
            cf:print("~!g[input_dir:~ts] done~n", [InputDir]);
        false ->
            cf:print("~!r[input_dir:~ts] shouldn't exist~n", [InputDir])
    end.

loop_transform([DefineModule | DefineModules], Config) ->
    cf:print("~!g  [module:~p] transforming...~n", [DefineModule]),
    case catch tdata:transform_files(DefineModule, Config, Config) of
        {'EXIT', Reason} ->
            cf:print("~!rEXIT Reason:~n~p~n", [Reason]);
        ResList ->
            print_result(ResList),
            loop_transform(DefineModules, Config)
    end;
loop_transform([], _Config) -> ok.

print_result(ResList) ->
    [begin
         case Res of
             ok ->
                 cf:print("~!g  ==> ~ts : ~p~n", [OutputFile, Res]);
             skipped ->
                 cf:print("~!m  ==> ~ts : ~p~n", [OutputFile, Res]);
             _ ->
                 cf:print("~!r  ==> ~ts : ~p~n", [OutputFile, Res])
         end
     end || {OutputFile, Res} <- ResList].

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
handle_config([config_file | Config], Cwd, Acc) -> % useless, just skip
    handle_config(Config, Cwd, Acc);
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

ensure_dir(Dir) ->
    ok = filelib:ensure_dir(filename:join(Dir, "temp")).

print_version() ->
    application:load(tdata),
    {ok, V} = application:get_key(tdata, vsn),
    cf:print("version:~ts~n", [V]).

load_beam(Cwd, Config) ->
    add_paths(Cwd),
    start_app(Config),
    ok.

add_paths(Cwd) ->
    EbinDirs = tdata_loader:get_ebin_dirs(Cwd),
    code:add_paths(EbinDirs),
    ok.

start_app(Config) ->
    case maps:get(app, Config, undefined) of
        undefined ->
            case filelib:wildcard("src/*.app.src") of
                [AppT] ->
                    App = list_to_atom(filename:basename(AppT, ".app.src")),
                    catch application:ensure_all_started(App),
                    application:set_env(tdata, app, App);
                _ ->
                    skip
            end;
        App when is_atom(App) ->
            catch application:ensure_all_started(App),
            application:set_env(tdata, app, App);
        AppStr when is_list(AppStr) ->
            App = list_to_atom(AppStr),
            catch application:ensure_all_started(App),
            application:set_env(tdata, app, App)
    end,
    ok.