%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(feeds).
-author("Ivan Dyachkov").

-include("db_errors.hrl").
-include("../ffengine.hrl").

%% db operations
-export([ read/2
        ]).

%% TODO: make configurable
-define(LIMIT, 30).

%%%_* Types ====================================================================

%%%_* API ======================================================================
read(UserId, FeedName) ->
  Fields = [username, permalink, body, created_at, updated_at, comments, reactions],
  Sql = io_lib:format("select ~s from get_feed(~s, ~s, ~B);",
                      [ ffengine_utils:list_to_str(Fields)
                      , UserId
                      , ffengine_db:sql_escape_str(FeedName)
                      , ?LIMIT]),
  {ok, _, Res} = ffengine_db:squery(Sql),
  case Res of
    [] ->
      {error, not_found};
    Posts ->
      {ok, lists:map(fun(Post) -> ffengine_utils:tuple_to_map(Fields, Post) end, Posts)}
  end.

%%%_* Internal =================================================================

