-module(secondsight_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("secondsight.hrl").

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    %%{ok, Body, _}= cowboy_req:body(Req),
    {HeaderVal, _} = cowboy_req:header(<<"authorization">>, Req), 
    _ = lager:info("auth: ~p", [HeaderVal]),
%%    case  authorize(HeaderVal) of
    case cowboy_req:body(Req) of
        {ok, <<>>, _} ->
            case cowboy_req:qs_val(<<"q">>, Req, undefined) of
                {undefined, _} ->
                    {ok, Req2} = cowboy_req:reply(402, [
                                                        {<<"content-type">>, <<"text/plain">>}
                                                       ], <<"malformed request">>, Req),
                    {ok, Req2, State};
                {QS, _} ->
                    {ok, Pid} = riakc_pb_socket:start_link(localhost, 8087),
                    {ok, Res} = riakc_pb_socket:search(Pid, <<"sesame_index">>, QS, [{rows, 1024}]),
                    ok = riakc_pb_socket:stop(Pid),
                    {search_results, Result, _, Num} = Res,
                    Str = io_lib:format("<body>~p <br/> ~ts</body>",
                                        [Num, unicode:characters_to_list(jsonx:encode(Result))]),
                    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}],
                                                  list_to_binary(Str), Req),
                    {ok, Req2, State}
            end;
        {ok, Bin, _} when is_binary(Bin) ->
            {ok, BodyQs, _} = cowboy_req:body_qs(Req),
            Key = proplists:get_value(<<"key">>, BodyQs, undefined),

            QS = <<Key/binary, ":*">>,  %iolist_to_binary([ [M, ":*" || M <- Metric
            {ok, Pid} = riakc_pb_socket:start_link(localhost, 8087),
            {ok, Res} = riakc_pb_socket:search(Pid, <<"sesame_index">>, QS, [{rows, 1024}]),
            {search_results, Result, _, _Num} = Res,
            {ok, MapRedResult} = get_all_keys(Pid, search_result_to_tbk(Result)),
            ok = riakc_pb_socket:stop(Pid),
            ExtractValue = fun(RiakObj) ->
                                   RKey = element(3, RiakObj),
                                   RCon0 = case element(4, RiakObj) of
                                               [RCon|_] -> RCon;
                                               RCon -> RCon
                                           end,
                                   JSON = element(3, RCon0),
                                   {Proplist} = jsonx:decode(JSON),
                                   %% lager:info("~p ~p", [Key, RKey]),
                                   Value = proplists:get_value(Key, Proplist), 
                                   {RKey, Value}
                           end,
            Values = map_mapred_result(ExtractValue, MapRedResult),
            _ = lager:info("~p", [Values]),
            {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}],
                                          jsonx:encode(Values), Req),
            {ok, Req2, State};
        _E ->
            _ = lager:error("auth error: ~p", [_E]),
            Str = <<>>, %io_lib:format("~p", [_E]),
            {ok, Req2} = cowboy_req:reply(500, [],
                                          Str, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

doc2tbk([], KD) -> KD;
doc2tbk([{<<"_yz_rt">>, Type}|TL], {{{undefined, B}, K}, undefined}) ->
    doc2tbk(TL, {{{Type, B}, K}, undefined});
doc2tbk([{<<"_yz_rb">>, Buck}|TL], {{{T, undefined}, K}, undefined}) ->
    doc2tbk(TL, {{{T, Buck}, K}, undefined});
doc2tbk([{<<"_yz_rk">>, Key }|_], {{{T, B}, undefined}, undefined}) ->
    {{{T, B}, Key}, undefined};
doc2tbk([_|TL], KD) ->
    doc2tbk(TL, KD).

search_result_to_tbk(Results) ->
    lists:map(fun({_, Doc}) ->
                      doc2tbk(Doc, {{{undefined, undefined}, undefined}, undefined})
              end, Results).
    
-spec get_all_keys(pid(), [{binary(),binary()}]) -> list().
get_all_keys(Riakc, BKs) ->
    MapRedQuery = [{map, {modfun, riak_kv_mapreduce, map_identity},
                    <<"filter_notfound">>, true},
                   {reduce, {modfun, riak_kv_mapreduce, reduce_sort},
                    undefined, true}],
    riakc_pb_socket:mapred(Riakc, BKs, MapRedQuery).


map_mapred_result(Fun, Result) ->
    map_mapred_result(Fun, Result, []).

map_mapred_result(_, [], Mapped) -> lists:reverse(Mapped);
map_mapred_result(Fun, [{_, Objects}|L], Mapped) ->
    Revmapped = lists:foldl(fun(Elem, Acc0) ->
                                    [Fun(Elem)|Acc0]
                            end, Mapped, Objects),
    map_mapred_result(Fun, L, Revmapped).
