%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(ffengine_utils).
-author("Ivan Dyachkov <ivan@dyachkov.org>").

%% API
-export([ kv/2
        , kv/3
        , id/0
        , utc_time/0
        , time_plus/2
        , time_to_iso8601/1
        , time_to_url/1
        , list_to_str/1
        , tuple_to_map/2
        , comma_separate/1
        ]).

%%%_* API ======================================================================
-spec kv(term(), proplists:proplist()) -> term().
kv(Key, Proplist) ->
  {Key, Value} = lists:keyfind(Key, 1, Proplist),
  Value.

-spec kv(term(), proplists:proplist(), term()) -> term().
kv(Key, Proplist, Default) ->
  case lists:keyfind(Key, 1, Proplist) of
    {Key, Value} -> Value;
    false        -> Default
  end.

id() ->
  erlang:system_time(). % nano seconds timestamp

utc_time() ->
  calendar:universal_time().

time_plus(DateTime, Seconds) ->
  calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(DateTime) + Seconds).

time_to_iso8601({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  io_lib:format("~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC",
                [Year, Month, Day, Hour, Minute, Second]).

time_to_url({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  io_lib:format("~4w~2..0w~2..0w_~2..0w~2..0w~2..0w_~5..0w",
                [Year, Month, Day, Hour, Minute, Second, random:uniform(99999)]).

list_to_str(L) ->
  string:strip(string:strip(lists:flatten(io_lib:format("~p", [L])), left, $[), right, $]).

tuple_to_map(Fields, Tuple) ->
  tuple_to_map(Fields, erlang:tuple_to_list(Tuple), #{}).

tuple_to_map([], [], Map) ->
  Map;
tuple_to_map([F | Fields], [<<"[null]">> | Values], Map) ->
  tuple_to_map(Fields, Values, Map#{F => []});
tuple_to_map([F | Fields], [V | Values], Map) ->
  tuple_to_map(Fields, Values, Map#{F => V}).

comma_separate([]) ->
  [];
comma_separate([_] = L) ->
  L;
comma_separate([_|_] = L) ->
  comma_separate(L, []).

comma_separate([X], Acc) ->
  [Acc, ",", X];
comma_separate([X | Tail], []) ->
  comma_separate(Tail, [X]);
comma_separate([X | Tail], Acc) ->
  comma_separate(Tail, [Acc, ",", X]).
