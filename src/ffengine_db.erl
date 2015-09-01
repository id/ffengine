%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(ffengine_db).

-include_lib("epgsql/include/epgsql.hrl").
-include("ffengine.hrl").

%% API
-export([ equery/2
        , squery/1
        , tx_begin/0
        , tx_execute/2
        , tx_commit/1
        , tx_rollback/1
        , sql_escape_str/1
        , intersperse/2
        , intersperse/3
        , decode_string/1
        , decode_boolean/1
        ]).

-define(TIMEOUT, 5000).

%%%_ * API ---------------------------------------------------------------------
equery(Stmt, Params) ->
  F = fun(Worker) -> gen_server:call(Worker, {equery, Stmt, Params}) end,
  poolboy:transaction(?DB_CONN_POOL, F).

squery(Sql) ->
  F = fun(Worker) -> gen_server:call(Worker, {squery, Sql}) end,
  poolboy:transaction(?DB_CONN_POOL, F).

tx_begin() ->
  Worker = poolboy:checkout(?DB_CONN_POOL, true, ?TIMEOUT),
  {ok, [], []} = gen_server:call(Worker, {squery, "begin"}),
  {ok, Worker}.

tx_execute(Worker, Sql) ->
  gen_server:call(Worker, {squery, Sql}).

tx_commit(Worker) ->
  {ok, [], []} = gen_server:call(Worker, {squery, "commit"}),
  poolboy:checkin(?DB_CONN_POOL, Worker).

tx_rollback(Worker) ->
  {ok, [], []} = gen_server:call(Worker, {squery, "rollback"}),
  poolboy:checkin(?DB_CONN_POOL, Worker).

sql_escape_str(Bin0) when is_binary(Bin0) ->
  Bin = binary:replace(Bin0, <<"'">>, <<"''">>),
  <<"'", Bin/binary, "'">>;
sql_escape_str(Str0) ->
  Str = lists:reverse(
          lists:foldl(
            fun($', Acc) -> [$', $' | Acc];
               (C, Acc)  -> [C | Acc]
            end,
            [], Str0)),
  ["'", Str, "'"].

intersperse(X, L) ->
  lists:reverse(tl(lists:foldl(fun(Y, Acc) -> [X, Y | Acc] end, [], L))).

intersperse(X, L, F) when is_function(F) ->
  lists:reverse(tl(lists:foldl(fun(Y, Acc) -> [X, F(Y) | Acc] end, [], L))).

decode_boolean(<<"t">>) -> true;
decode_boolean(<<"f">>) -> false.

decode_string(null) -> "";
decode_string(Str) when is_list(Str) -> Str;
decode_string(Bin) when is_binary(Bin) -> binary_to_list(Bin).

