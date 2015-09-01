%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(tokens).
-author("Ivan Dyachkov").

-include("../ffengine.hrl").

%% db operations
-export([ new/0
        , read_default_token/1
        , is_valid/1
        ]).

-define(TOKEN_LENGTH, 64).

%%%_* API ======================================================================
-spec new() -> {ok, binary()}.
new() ->
  base64:encode(crypto:strong_rand_bytes(?TOKEN_LENGTH)).

-spec read_default_token(binary()) -> {ok, term()} | false.
read_default_token(UserId) ->
  %% TODO: auto-refresh valid_until or recrete token when expired
  Ts = ffengine_utils:ts(),
  Sql = io_lib:format("select token from tokens where user_id = ~s and "
                      "token_name = 'default' and valid_until > ~s;",
                      [ UserId
                      , ffengine_utils:ts_to_iso8601_sql(Ts)]),
  {ok, _, Res} = ffengine_db:squery(Sql),
  case Res of
    []        -> false;
    [{Token}] -> {ok, Token}
  end.

-spec is_valid(binary()) -> {ok, binary()} | false.
is_valid(Token) ->
  Ts = ffengine_utils:ts(),
  Sql = io_lib:format("select user_id from tokens where token = ~s and valid_until > ~s;",
                      [ ffengine_db:sql_escape_str(Token)
                      , ffengine_utils:ts_to_iso8601_sql(Ts)]),
  {ok, _, Res} = ffengine_db:squery(Sql),
  case Res of
    []          -> false;
    [{UserId}] -> {ok, UserId}
  end.

%%%_* Internal =================================================================

