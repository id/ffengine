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
  Sql = "select * from get_feed($1, $2, $3, $4);",
  Res = ffengine_db:equery(Sql, [UserId, FeedName, ?LIMIT, 0]),
  ffengine_db:parse_select_res(Res).

%%%_* Internal =================================================================

