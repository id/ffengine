%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(users).
-author("Ivan Dyachkov").

-include("../ffengine.hrl").
-include("db_errors.hrl").

%% db operations
-export([ create/4
        , read/1
        , subscribe/2
        ]).

%%%_* Types ====================================================================
-define(TOKEN_LIFETIME, "'1 year'").

%%%_* API ======================================================================
create(Username, Pwdhash, Email, Ts) ->
  Token = tokens:new(),
  Sql = io_lib:format("select create_user(~s, ~s, ~s, ~s, ~s, ~s, ~s);",
                     [ ffengine_db:sql_escape_str(Username)
                     , ffengine_db:sql_escape_str(Pwdhash)
                     , ffengine_db:sql_escape_str(Email)
                     , ffengine_utils:ts_to_iso8601_sql(Ts)
                     , ffengine_db:sql_escape_str(Token)
                     , "'default'"
                     , ?TOKEN_LIFETIME]),
  {ok, Tx} = ffengine_db:tx_begin(),
  case ffengine_db:tx_execute(Tx, Sql) of
    {ok, _, [{<<"t">>}]} ->
      ok = ffengine_db:tx_commit(Tx),
      true;
    {ok, _, [{<<"f">>}]} ->
      ok = ffengine_db:tx_rollback(Tx),
      false;
    {error, {error, error, ?ERROR_DUPLICATE_KEY, _, _}} ->
      {error, already_exist}
  end.

read(Username) ->
  Fields = [user_id, username, screen_name, pwdhash, email],
  Sql = io_lib:format("select ~s from users where username = ~s;",
                     [ ffengine_utils:list_to_str(Fields)
                     , ffengine_db:sql_escape_str(Username)]),
  {ok, _, Res} = ffengine_db:squery(Sql),
  case Res of
    [] ->
      {error, not_found};
    [User] ->
      {ok, ffengine_utils:tuple_to_map(Fields, User)}
  end.

subscribe(UserId, Username) ->
  Sql = io_lib:format("select subscribe_on(~s, ~s);",
                     [UserId, ffengine_db:sql_escape_str(Username)]),
  case ffengine_db:squery(Sql) of
    {ok, _, [{Res}]} ->
      ffengine_db:decode_boolean(Res);
    {error, _} = Error ->
      Error
  end.

%%%_* Internal =================================================================

