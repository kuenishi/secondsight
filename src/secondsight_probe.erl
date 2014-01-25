%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 23 Jan 2014 by UENISHI Kota <kota@basho.com>

-module(secondsight_probe).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% temporary, for query!! not for probing
%%-export([map/3, reduce/2]).


-include("secondsight.hrl").

-record(state, {}).

%% @doc
%% Starts the server
%% @end
-spec start_link() -> {ok, pid()} | ignore | {error, Error::any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc
%% Initializes the server
%% @end
-spec init(list()) -> {ok, #state{}} |
                      {ok, #state{}, Timeout::non_neg_integer()} |
                      ignore |
                      {stop, Reason::any()}.
init([]) ->
    emit(started, app_configs()),
    poke_me_later(),
    {ok, #state{}}.

%% @private
%% @doc
%% Handling call messages
%% @end
%%
-spec handle_call(term(), term(), #state{}) ->
                         {reply, Reply::term(), #state{}} |
                         {reply, Reply::term(), #state{}, Timeout::non_neg_integer()} |
                         {noreply, #state{}} |
                         {noreply, #state{}, Timeout::non_neg_integer()} |
                         {stop, Reason::term(), Reply::term(), #state{}} |
                         {stop, Reason::term(), #state{}}.
handle_call({emit, ET, E}, _, State) ->
    emit(ET, E),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
%% @end
%%
-spec handle_cast(term(), #state{}) -> {noreply, #state{}} |
                                       {noreply, #state{}, Timeout::non_neg_integer()} |
                                       {stop, Reason::term(), #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info(poke, State) ->
    emit(stats, get_stats()),
    %% TODO: gather and emit other OS related resouces 
    poke_me_later(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
terminate(Reason, _State) ->
    emit(terminated, {Reason, _State}),
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
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
    _ = lager:info("~p, ~p", [EventType, self()]),
    _Result.

get_stats() ->
    {value, {disk, Disk}, Stats} = lists:keytake(disk, 1, riak_kv_stat:get_stats()),
    DiskFlat = [{struct, [{id, list_to_binary(Id)}, {size, Size}, {used, Used}]} || {Id, Size, Used} <- Disk],
    lists:append([Stats, [{disk, DiskFlat}], riak_core_stat:get_stats()]).

app_configs() ->
    [{AppName, application:get_all_env(AppName)} || {AppName, _, _} <- application:loaded_applications()].

-spec poke_me_later() -> any().
poke_me_later() ->
    erlang:send_after(?POKE_INTERVAL, self(), poke).



%% map({error, notfound}=NF, KD, Action) ->
%%     notfound_map_action(NF, KD, Action);
%% %% map(RiakObject, _, _) ->
%% %%     Key = riak_object:get_key(RiakObject),
%% %%     Value = riak_object:get_value(RiakObject),
%%    _ = lager:log(info, 
%%    [riak_object:get_value(RiakObject)].

