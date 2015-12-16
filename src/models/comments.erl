%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(comments).
-author("Ivan Dyachkov").

-include("db_errors.hrl").
-include("../ffengine.hrl").

%% db operations
-export([ create/4
        , read/1
        ]).

%%%_* Types ====================================================================

%%%_* API ======================================================================
create(UserId, PostId, Body, Time) ->
  CommentId = ffengine_utils:id(),
  Rating = CommentId,
  Batch = [ {?INSERT_COMMENT, [CommentId, UserId, PostId, Body, Time, Time]}
          , {?INSERT_POST_COMMENTS_CHANNEL, [UserId, PostId]}
          , {?INSERT_POST_ACTIVITY_HIST, [PostId, UserId, 'comment', Time]}
          , {?UPDATE_POST_RATING, [PostId, Rating]}],
  Res = ffengine_db:execute_batch(Batch),
  case ffengine_db:verify_results(Res) of
    ok ->
      read(CommentId);
    ?ERROR_DUPLICATE_KEY = Error ->
      ?ERROR("create comment from user_id ~B on post ~B at ~p failed: ~p",
             [UserId, PostId, Time, Error]),
      {error, already_exists};
    {error, _} = Error ->
      ?ERROR("create comment from user_id ~B on post ~B at ~p failed: ~p",
             [UserId, PostId, Time, Error]),
      Error
  end.

read(CommentId) ->
  Sql = "select * from get_comment($1)",
  Res = ffengine_db:equery(Sql, [CommentId]),
  ffengine_db:parse_select_res(Res).
