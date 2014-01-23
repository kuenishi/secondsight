-module(secondsight_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("secondsight.hrl").

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Body, _}= cowboy_req:body(Req),
    {HeaderVal, _} = cowboy_req:header(<<"authorization">>, Req), 
    _ = lager:info("auth: ~p", [HeaderVal]),
    case  authorize(HeaderVal) of
        ok ->
    %% {ok, Req2} = cowboy_req:reply(200, [
    %%                                     {<<"content-type">>, <<"text/plain">>}
    %%                                    ], <<"Hello World!">>, Req),
            {ok, Req2} = cowboy_req:reply(200, [], <<>>, Req),
            Event = binary_to_term(Body),
            store(Event),
            _ = lager:info("from: ~p", [Event]),
            {ok, Req2, State};
        _E ->
            _ = lager:error("auth error: ~p", [binary_to_term(Body)]),
            {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.


authorize(<<"sesame">>) -> ok;
authorize(_AuthHeader) -> error.

%% encode every event to json
event2json(Event) when is_tuple(Event) andalso element(1, Event) =:= lager_msg ->
    jsonx:encode([{<<"message">>, iolist_to_binary(lager_msg:message(Event))}]);
event2json(Event) when is_tuple(Event) ->
    jsonx:encode([{<<"message">>, io_lib:format("~w", [Event])}]);
event2json(Event) ->
    case jsonx:encode(Event) of
        Fail when is_tuple(Fail) ->
            jsonx:encode([{<<"message">>, io_lib:format("~w", [Event])}]);
        JSON when is_binary(JSON) ->
            JSON
    end.

store(?PACKAGE{event_type = Type, event=Event,
               timestamp = {MS, S, US},
               node = Node, ring_members = _Members} = _Package) ->
    Host = localhost,
    Port = 8087,
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),

    %% TODO measuring skew between remote and local would be useful
    %% {MS,S,US} = os:timestamp(),

    %% TODO we have to know user from auth info and Members

    Key = list_to_binary(io_lib:format(":~p:~p:~p", [MS, S, US])),
    RiakObj = riakc_obj:new({<<"opensesame:secondsight">>, atom_to_binary(Type, latin1)},
                            <<(atom_to_binary(Node, latin1))/binary, Key/binary>>,
                            event2json(Event), <<"application/json">>),
    
    ok = riakc_pb_socket:put(Pid, RiakObj, [{w,0}]),
    riakc_pb_socket:stop(Pid).
