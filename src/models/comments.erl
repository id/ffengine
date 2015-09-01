%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(comments).
-author("Ivan Dyachkov").

-include("db_errors.hrl").
-include("../ffengine.hrl").

%% db operations
-export([ create/5
        ]).

%%%_* Types ====================================================================

%%%_* API ======================================================================
create(Token, Username, PostPermalink, Body, Ts) ->
  %% TODO: wrap in transaction
  Sql = io_lib:format("select create_comment(~s, ~s, ~s, ~s, ~s, ~B);",
          [ ffengine_db:sql_escape_str(Token)
          , ffengine_db:sql_escape_str(Username)
          , ffengine_db:sql_escape_str(PostPermalink)
          , ffengine_db:sql_escape_str(Body)
          , ffengine_utils:ts_to_iso8601_sql(Ts)
          , Ts]),
  {ok, _, [_]} = ffengine_db:squery(Sql),
  ok.
