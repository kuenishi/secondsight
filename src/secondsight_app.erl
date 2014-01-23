-module(secondsight_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                      %% {URIHost, list({URIPath, Handler, Opts})}
                                      {'_', [{'_', secondsight_handler, []}]}
                                     ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(secondsight, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    secondsight_sup:start_link().

stop(_State) ->
    ok.
