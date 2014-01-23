%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 23 Jan 2014 by UENISHI Kota <kota@basho.com>

-module(secondsight_prober).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ENDPOINT_HOST, "localhost").
-define(ENDPOINT_PORT, 8080).

%% TODO depricate and use binary and secure protocol
-define(ENDPOINT_URL, "http://localhost:8080/emit/").
%% TODO use secure authorization
%% Authorization: sesame
-define(AUTH_KEY, "sesame").

%% keep how many message?
%% -define(BUFFER_SIZE, 1).
%% -define(FLUSH_PERIOD, 1024). %% ms
-define(POKE_INTERVAL, 1024*10). %% ms

-record(state, {
          %% connection?
         }).

-include("secondsight.hrl").

%% @doc
%% Creates an event manager
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @doc
%% Adds an event handler
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%% gen_event callbacks
%%%===================================================================

%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
-spec init(Args::[any()]) -> {ok, #state{}}.
init([]) ->
    poke_me_later(),
    emit_it_later(started, app_configs()),
    {ok, #state{}}.

%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%% @end
-spec handle_event(Event::any(), #state{}) ->
                          {ok, #state{}} |
                          {swap_handler, Args::[any()],
                           #state{}, Mod2::atom(), Args2::[any()]} |
                          remove_handler.
handle_event(Event, State) ->
    emit(event, Event),
    {ok, State}.


%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%% @end
-spec handle_call(Req::any(), #state{}) ->
                         {ok, Reply::any(), #state{}} |
                         {swap_handler, Reply::any(), Args::[any()],
                          #state{}, Mod2::atom(), Args2::[any()]} |
                         {remove_handler, Reply::any()}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%% @end
-spec handle_info(any(), #state{}) ->
                         {ok, #state{}} |
                         {swap_handler, Args::[any()],
                          #state{}, Mod2::atom(), Args2::[any()]} |
                         remove_handler.
handle_info({emit, Type, Event}, State) ->
    emit(Type, Event),
    {ok, State};
handle_info(poke, State) ->
    emit(stats, get_stats()),
    poke_me_later(),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
-spec terminate(Reason::any(), #state{}) -> any().
terminate(Reason, _State) ->
    emit(terminated, {Reason, _State}),
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
%% @end
-spec code_change(OldVsn::any(), State::#state{}, Extra::any()) ->
                         {ok, NewState::#state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
%%%===================================================================
-spec emit(EventType::atom(), Event::term()) -> any().
emit(EventType, Event) ->
    Package = ?PACKAGE{event_type = EventType,
                       event = Event,
                       timestamp = os:timestamp(),
                       node = node(),
                       ring_members = nodes()},
    Headers = [{"Authorization", ?AUTH_KEY}],
    Request = {?ENDPOINT_URL, Headers, "application/bert",
               term_to_binary(Package,[compressed])},
    {_Result, _} = httpc:request(put, Request, [], []),
    %% TODO to be debug log
    _ = lager:info("~p, ~p", [EventType, Event]),
    _Result.

-spec poke_me_later() -> any().
poke_me_later() ->
    erlang:send_after(?POKE_INTERVAL, ?MODULE, poke).

emit_it_later(EventType, Event) ->
    erlang:send_after(?POKE_INTERVAL, ?MODULE, {emit, EventType, Event}).
    

get_stats() ->
    {value, {disk, Disk}, Stats} = lists:keytake(disk, 1, riak_kv_stat:get_stats()),
    DiskFlat = [{struct, [{id, list_to_binary(Id)}, {size, Size}, {used, Used}]} || {Id, Size, Used} <- Disk],
    lists:append([Stats, [{disk, DiskFlat}], riak_core_stat:get_stats()]).


app_configs() ->
    [{AppName, application:get_all_env(AppName)} || {AppName, _, _} <- application:loaded_applications()].
