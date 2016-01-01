%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(reactions).
-author("Ivan Dyachkov").

-include("db_errors.hrl").
-include("../ffengine.hrl").

%% db operations
-export([ create/4
        , read/1
        ]).

%%%_* Types ====================================================================

%%%_* API ======================================================================
create(_UserId, _PostId, _Reaction, _Time) ->
  ok.
  %% ReactionId = ffengine_utils:id(),
  %% Rating = ReactionId,
  %% Batch = [ {?INSERT_REACTION, [CommentId, UserId, PostId, Body, Time, Time]}
  %%         , {?INSERT_POST_REACTIONS_CHANNEL, [UserId, PostId]}
  %%         , {?INSERT_POST_ACTIVITY_HIST, [PostId, UserId, 'reaction', Time]}
  %%         , {?MAYBE_UPDATE_POST_RATING, [PostId, Rating]}],
  %% Res = ffengine_db:execute_batch(Batch),
  %% case ffengine_db:verify_results(Res) of
  %%   ok ->
  %%     read(ReactionId);
  %%   ?ERROR_DUPLICATE_KEY = Error ->
  %%     ?ERROR("create reaction from user_id ~B on post ~B at ~p failed: ~p",
  %%            [UserId, PostId, Time, Error]),
  %%     {error, already_exists};
  %%   {error, _} = Error ->
  %%     ?ERROR("create reactction from user_id ~B on post ~B at ~p failed: ~p",
  %%            [UserId, PostId, Time, Error]),
  %%     Error
  %% end.

read(ReactionId) ->
  Sql = "select * from get_reaction($1)",
  Res = ffengine_db:equery(Sql, [ReactionId]),
  ffengine_db:parse_select_res(Res).
