-module(secondsight_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    DispatchTable = [%% {URIHost, list({URIPath, Handler, Opts})}
                     {'_', [{"/", cowboy_static, {priv_file, secondsight, "static/index.html"}},
                            {"/assets/[...]", cowboy_static, {priv_dir, secondsight, "static/assets"}},
                            {"/v1/emit/[...]", secondsight_sink, []},
                            {"/v1/data/[...]", secondsight_handler, []},
                            {'_', secondsight_handler, []}
                           ]}],
    CompiledDispatch = cowboy_router:compile(DispatchTable),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(secondsight, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, CompiledDispatch}]}]),
    secondsight_sup:start_link().

stop(_State) ->
    ok.
