-module(tdata_ct_hook).

%% API
-export([
    init/2,
    terminate/1
]).

init(Id, _Opts) ->
    tdata:start(),
    tdata:init_excel_loader(),
    {ok, Id}.

terminate(_) ->
    tdata:stop(),
    ok.