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
        , tx_commit/1
        , tx_rollback/1
        , verify_results/1
        , parse_select_res/1
        , statements/0
        , execute_batch/1
        , reload/0
        ]).

-define(TIMEOUT, 5000).

%%%_ * API ---------------------------------------------------------------------
equery(StmtName, Params) ->
  F = fun(Worker) -> ffengine_epgsql_worker:equery(Worker, StmtName, Params) end,
  poolboy:transaction(?DB_CONN_POOL, F).

squery(Sql) ->
  F = fun(Worker) -> ffengine_epgsql_worker:squery(Worker, Sql) end,
  poolboy:transaction(?DB_CONN_POOL, F).

tx_begin() ->
  Worker = poolboy:checkout(?DB_CONN_POOL, true, ?TIMEOUT),
  {ok, [], []} = ffengine_epgsql_worker:squery(Worker, "begin;"),
  {ok, Worker}.

tx_commit(Worker) ->
  {ok, [], []} = ffengine_epgsql_worker:squery(Worker, "commit;"),
  poolboy:checkin(?DB_CONN_POOL, Worker).

tx_rollback(Worker) ->
  {ok, [], []} = ffengine_epgsql_worker:squery(Worker, "rollback;"),
  poolboy:checkin(?DB_CONN_POOL, Worker).

execute_batch(Batch) ->
  F = fun(Worker) -> ffengine_epgsql_worker:execute_batch(Worker, Batch) end,
  poolboy:transaction(?DB_CONN_POOL, F).

reload() ->
  Workers = gen_server:call(?DB_CONN_POOL, get_avail_workers),
  lists:foreach(fun ffengine_epgsql_worker:reload/1, Workers).

%% function crashes on unexpected errors (server returns 500)
verify_results([]) ->
  ok;
verify_results([{ok, _} | Tail]) ->
  verify_results(Tail);
verify_results([{ok, _, _} | Tail]) ->
  verify_results(Tail);
verify_results([{error, _} = Error | _]) ->
  Error.

-spec parse_select_res({ok, [#column{}], [tuple()]}) -> {ok, [map()]}.
parse_select_res({ok, _Columns, []}) ->
  {error, not_found};
parse_select_res({ok, Columns, Rows}) ->
  Fields = lists:map(
             fun(#column{name = Name}) ->
                 erlang:binary_to_atom(Name, latin1)
             end, Columns),
  parse_rows(Fields, Rows, []);
parse_select_res({error, _} = Error) ->
  Error.

parse_rows(_Fields, [], []) ->
  {error, not_found};
parse_rows(_Fields, [], [Row]) ->
  {ok, Row};
parse_rows(_Fields, [], Acc) ->
  {ok, lists:reverse(Acc)};
parse_rows(Fields, [Row | Rows], Acc) ->
  L = erlang:tuple_to_list(Row),
  case lists:all(fun(null) -> true; (_) -> false end, L) of
    true ->
      parse_rows(Fields, Rows, Acc);
    false ->
      parse_rows(Fields, Rows, [parse_row(Fields, L) | Acc])
  end.

parse_row(Fields, Row) ->
  parse_row(Fields, Row, #{}).

parse_row([], [], Map) ->
  Map;
parse_row([F | Fields], [<<"[null]">> | Values], Map) ->
  parse_row(Fields, Values, Map#{F => []});
parse_row([F | Fields], [V | Values], Map) ->
  parse_row(Fields, Values, Map#{F => V}).

statements() ->
  [ {?INSERT_USER,
     "insert into users(user_id, username, pwdhash, email, created_at, updated_at) values($1, $2, $3, $4, $5, $6);",
     [int8, text, text, text, timestamptz, timestamptz]
    }
  , {?INSERT_TOKEN,
     "insert into tokens(token_id, user_id, token, token_name, created_at, valid_until) values($1, $2, $3, $4, $5, $6);",
     [int8, int8, text, text, timestamptz, timestamptz]
    }
  , {?INSERT_CHANNEL,
     "insert into channels(channel_id, channel_mode, channel_type) values ($1, $2, $3);",
     [int8, channel_mode, channel_type]
    }
  , {?INSERT_USER_CHANNEL,
     "insert into user_channels(user_id, channel_id) values ($1, $2);",
     [int8, int8]
    }
  , {?INSERT_FEED,
     "insert into feeds(feed_id, user_id, feed_name) values ($1, $2, $3);",
     [int8, int8, text]
    }
  , {?INSERT_FEED_CHANNEL,
     "insert into feed_channels(feed_id, channel_id) values ($1, $2);",
     [int8, int8]
    }
  , {?INSERT_POST,
     "insert into posts(post_id, user_id, permalink, body, created_at, updated_at, post_rating) "
      "values ($1, $2, $3, $4, $5, $6, $7);",
     [int8, int8, text, text, timestamptz, timestamptz, int8]
    }
  , {?INSERT_POST_CHANNEL,
     "insert into post_channels(post_id, channel_id) "
       "select $1 as post_id, channel_id "
       "from user_channels join users using (user_id) "
       "join channels using (channel_id) where user_id = $2 and channel_type = 'posts';",
     [int8, int8]
    }
  , {?INSERT_POST_ACTIVITY_HIST,
     "insert into post_activity_hist (post_id, user_id, post_activity, created_at) "
     "values($1, $2, $3, $4);",
     [int8, int8, post_activity, timestamptz]
    }
  , {?INSERT_COMMENT,
     "insert into comments (comment_id, user_id, post_id, body, created_at, updated_at) "
     "values($1, $2, $3, $4, $5, $6);",
     [int8, int8, int8, text, timestamptz, timestamptz]
    }
  , {?INSERT_POST_COMMENTS_CHANNEL,
     "with channel as (select channel_id from user_channels join channels using(channel_id) "
     "where user_id = $1 and channel_type = 'comments') "
     "insert into post_channels(post_id, channel_id) select $2, channel_id from channel "
     "where not exists (select post_id, channel_id from post_channels where post_id = $2 "
     "and channel_id = channel.channel_id);",
     [int8, int8]
    }
  , {?UPDATE_POST_RATING,
     "update posts set post_rating = $2 where post_id = $1;",
     [int8, int8]
    }
  ].
