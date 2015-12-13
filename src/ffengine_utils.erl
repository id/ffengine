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
        , time_to_url/1
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

time_to_url({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  io_lib:format("~4w~2..0w~2..0w_~2..0w~2..0w~2..0w_~5..0w",
                [Year, Month, Day, Hour, Minute, Second, random:uniform(99999)]).

