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
        , read/1
        ]).

%%%_* Types ====================================================================

%%%_* API ======================================================================
create(UserId, Permalink, Body, Time) ->
  PostId = ffengine_utils:id(),
  %% here we exploit the fact that ffengine_utils:id() is a timestamp
  Rating = PostId,
  InsertPost = [{?INSERT_POST, [PostId, UserId, Permalink, Body, Time, Time, Rating]}],
  InsertPostChannel = [{?INSERT_POST_CHANNEL, [PostId, UserId]}],
  Batch = lists:append([InsertPost, InsertPostChannel]),
  Res = ffengine_db:execute_batch(Batch),
  case ffengine_db:verify_results(Res) of
    ok ->
      read(PostId);
    ?ERROR_DUPLICATE_KEY = Error ->
      ?ERROR("create post from user_id ~B with permalink ~s at ~p failed: ~p",
             [UserId, Permalink, Time, Error]),
      {error, already_exists};
    {error, _} = Error ->
      ?ERROR("create post from user_id ~B with permalink ~s at ~p failed: ~p",
             [UserId, Permalink, Time, Error]),
      Error
  end.


is_public(Username, Permalink) ->
  Sql = "select * from is_post_public($1, $2)",
  Res = ffengine_db:equery(Sql, [Username, Permalink]),
  case ffengine_db:parse_select_res(Res) of
    {ok, #{post_id := PostId}} ->
      {ok, PostId};
    _ ->
      false
  end.

can_read(UserId, Username, Permalink) ->
  Sql = "select * from can_read_post($1, $2, $3)",
  Res = ffengine_db:equery(Sql, [UserId, Username, Permalink]),
  case ffengine_db:parse_select_res(Res) of
    {ok, #{post_id := PostId}} ->
      {ok, PostId};
    _ ->
      false
  end.

read(PostId) ->
  Sql = "select * from get_post($1)",
  Res = ffengine_db:equery(Sql, [PostId]),
  ffengine_db:parse_select_res(Res).

%%%_* Internal =================================================================

