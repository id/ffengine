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

%% TODO: private posts
%%       - to private: delete entries from post_channels for posts from the user and
%%                     channels of type "comments" and "likes"
%%       - from private: recover entries in post_channels using post_history

  %% "with post_ids as (
  %%     select post_id from feeds f
  %%       join feed_channels using (feed_id)
  %%       join post_channels using (channel_id)
  %%       join posts using (post_id)
  %%       where f.user_id = $1 and f.feed_name = $2
  %%       order by post_rating desc limit $3 offset $4)
  %% select u.username, p.permalink, p.body, p.created_at, p.updated_at, p.post_rating,
  %%   json_agg(c.* order by c.created_at asc) as comments, json_agg(distinct r.*) as reactions
  %%   from posts p join users u using(user_id)
  %%   left join comments c using(post_id)
  %%   left join reactions r using(post_id)
  %%   where post_id in (select post_id from post_ids)
  %%   group by p.post_id, u.username;"
read(UserId, FeedName) ->
  Sql = "select * from get_feed($1, $2, $3, $4);",
  Res = ffengine_db:equery(Sql, [UserId, FeedName, ?LIMIT, 0]),
  ffengine_db:parse_select_res(Res).

%%%_* Internal =================================================================

