%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(ffengine_utils).
-author("Ivan Dyachkov <ivan@dyachkov.org>").

%% API
-export([ kv/2
        , kv/3
        , utc_time/0
        , utc_time/1
        , ts/0
        , ts_to_iso8601_sql/1
        , ts_to_url/1
        , list_to_str/1
        , tuple_to_map/2
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

ts() ->
  erlang:system_time(micro_seconds).

utc_time() ->
  calendar:universal_time().

utc_time(Ts) ->
  MegaSecs = Ts div 1000000000000,
  Secs = Ts div 1000000 - MegaSecs*1000000,
  MicroSecs = Ts rem 1000000,
  calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}).

ts_to_iso8601_sql(Ts) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = utc_time(Ts),
  io_lib:format("'~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC'",
                [Year, Month, Day, Hour, Minute, Second]).

ts_to_url(Ts) ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = utc_time(Ts),
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
