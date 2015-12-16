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

-spec read_default_token(binary()) -> {ok, map()} | {error, any()}.
read_default_token(UserId) ->
  %% TODO: auto-refresh valid_until or recrete token when expired
  Time = ffengine_utils:utc_time(),
  Sql = "select token from tokens where user_id = $1 and token_name = 'default' and valid_until > $2;",
  Res = ffengine_db:equery(Sql, [UserId, Time]),
  case ffengine_db:parse_select_res(Res) of
    {ok, #{token := Token}} ->
      {ok, Token};
    {error, _} = Error ->
      Error
  end.

-spec is_valid(binary()) -> {ok, map()} | false.
is_valid(Token) ->
  Time = ffengine_utils:utc_time(),
  Sql = "select user_id from tokens where token = $1 and valid_until > $2;",
  Res = ffengine_db:equery(Sql, [Token, Time]),
  case ffengine_db:parse_select_res(Res) of
    {ok, #{user_id := UserId}} ->
      {ok, UserId};
    {error, _} ->
      false
  end.

%%%_* Internal =================================================================

