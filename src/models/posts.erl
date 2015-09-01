%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(posts).
-author("Ivan Dyachkov").

-include("db_errors.hrl").
-include("../ffengine.hrl").

%% db operations
-export([ create/4
        , is_public/2
        , can_read/3
        , read/2
        ]).

%%%_* Types ====================================================================

%%%_* API ======================================================================
create(UserId, Permalink, Body, Ts) ->
  Fields = [username, permalink, body, created_at, updated_at, comments, reactions],
  Sql = io_lib:format("select ~s from create_post(~s, ~s, ~s, ~s, ~B);",
                     [ ffengine_utils:list_to_str(Fields)
                     , UserId
                     , ffengine_db:sql_escape_str(Permalink)
                     , ffengine_db:sql_escape_str(Body)
                     , ffengine_utils:ts_to_iso8601_sql(Ts)
                     , Ts]),
  case ffengine_db:squery(Sql) of
    {ok, _, [Post]} ->
      {ok, ffengine_utils:tuple_to_map(Fields, Post)};
    {error, {error, error, ?ERROR_DUPLICATE_KEY, _, _}} ->
      {error, already_exist}
  end.


is_public(Username, Permalink) ->
  Sql = io_lib:format("select is_post_public(~s, ~s);",
                     [ ffengine_db:sql_escape_str(Username)
                     , ffengine_db:sql_escape_str(Permalink)]),
  {ok, _, [{Res}]} = ffengine_db:squery(Sql),
  ffengine_db:decode_boolean(Res).

can_read(Token, Username, Permalink) ->
  Sql = io_lib:format("select can_read_post(~s, ~s, ~s, ~s);",
                      [ ffengine_db:sql_escape_str(Token)
                      , ffengine_db:sql_escape_str(Username)
                      , ffengine_db:sql_escape_str(Permalink)
                      , ffengine_utils:ts_to_iso8601_sql(ffengine_utils:ts())
                      ]),
  {ok, _, [{Res}]} = ffengine_db:squery(Sql),
  ffengine_db:decode_boolean(Res).

read(Username, Permalink) ->
  Fields = [username, permalink, body, created_at, updated_at, comments, reactions],
  Sql = io_lib:format("select ~s from get_post(~s, ~s);",
                     [ ffengine_utils:list_to_str(Fields)
                     , ffengine_db:sql_escape_str(Username)
                     , ffengine_db:sql_escape_str(Permalink)]),
  {ok, _, Res} = ffengine_db:squery(Sql),
  case Res of
    [] ->
      {error, not_found};
    [Post] ->
      {ok, ffengine_utils:tuple_to_map(Fields, Post)}
  end.

%%%_* Internal =================================================================

