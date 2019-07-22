-module(tdata_json_loader).
-behavior(tdata_loader).

%% API
-export([
    start/0, stop/0
]).

start() ->
    JsonLoader =
        fun(Filename, LoadOpts) ->
            {ok, Binary} = file:read_file(Filename),
            Opts = lists:usort([return_maps | [T || T <- LoadOpts, is_atom(T)]]) ++
                lists:ukeymerge(1, [T || T <- LoadOpts, is_tuple(T)], [{labels, atom}]),
            Data = jsx:decode(Binary, Opts),
            {ok, Data}
        end,
    tdata_loader:set_loader(".json", JsonLoader),
    ok.

stop() ->
    tdata_loader:del_loader(".json"),
    ok.