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

-record(state, {}).

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
handle_event(_Event, State) ->
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
terminate(_Reason, _State) ->
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
