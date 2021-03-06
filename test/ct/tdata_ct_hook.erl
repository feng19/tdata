-module(tdata_ct_hook).

%% API
-export([
    init/2,
    terminate/1
]).

init(Id, _Opts) ->
    {ok, _} = tdata:start(),
    {ok, Id}.

terminate(_) ->
    tdata:stop(),
    ok.