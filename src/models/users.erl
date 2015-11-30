%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(users).
-author("Ivan Dyachkov").

-include("../ffengine.hrl").
-include("db_errors.hrl").

%% db operations
-export([ create/3
        , read/1
        , subscribe/2
        ]).

%%%_* Types ====================================================================
-define(DEFAULT_TOKEN, "default").
-define(TOKEN_LIFETIME, 31536000). % 1 year in seconds, 60*60*24*365

-define(HOME_FEED, "home").

%%%_* API ======================================================================
create(Username, Pwdhash, Email) ->
  UserId = ffengine_utils:id(),
  Time = ffengine_utils:utc_time(),
  InsertUser = [{?INSERT_USER, [UserId, Username, Pwdhash, Email, Time, Time]}],

  TokenId = ffengine_utils:id(),
  Token = tokens:new(),
  ValidUntil = ffengine_utils:time_plus(Time, ?TOKEN_LIFETIME),
  InsertToken = [{?INSERT_TOKEN, [TokenId, UserId, Token, ?DEFAULT_TOKEN, Time, ValidUntil]}],

  ChannelTypes = ["posts", "comments", "reactions"],
  ChannelIds = [ffengine_utils:id() || _ <- ChannelTypes],
  InsertChannels = [{?INSERT_CHANNEL, [ChannelId, "public", ChannelType]} ||
                     {ChannelId, ChannelType} <- lists:zip(ChannelIds, ChannelTypes)],

  InsertUserChannels = [{?INSERT_USER_CHANNEL, [UserId, ChannelId]} || ChannelId <- ChannelIds],

  FeedId = ffengine_utils:id(),
  InsertFeed = [{?INSERT_FEED, [FeedId, UserId, ?HOME_FEED]}],

  InsertFeedChannels = [{?INSERT_FEED_CHANNEL, [FeedId, ChannelId]} || ChannelId <- ChannelIds],

  Batch = lists:append([InsertUser, InsertToken, InsertChannels,
                        InsertUserChannels, InsertFeed, InsertFeedChannels]),
  Res = ffengine_db:execute_batch(Batch),
  ffengine_db:verify_results(Res).

read(Username) ->
  Res = ffengine_db:equery("select * from users where username = $1;", [Username]),
  ffengine_db:parse_select_res(Res).

subscribe(SubscriberId, TargetUser) ->
  {ok, Tx} = ffengine_db:tx_begin(),
  Res = ffengine_db:equery("select channel_id from subscribe_on($1, $2, $3);",
                           [SubscriberId, ?HOME_FEED, TargetUser]),
  case ffengine_db:parse_select_res(Res) of
    {ok, [_|_]} ->
      ok = ffengine_db:tx_commit(Tx),
      ok;
    ?ERROR_DUPLICATE_KEY = Error ->
      ?ERROR("subscribe user_id ~B on ~s failed: ~p", [SubscriberId, TargetUser, Error]),
      ok = ffengine_db:tx_rollback(Tx),
      {error, already_exists};
    Other ->
      ok = ffengine_db:tx_rollback(Tx),
      ?ERROR("subscribe user_id ~B on ~s failed: ~p", [SubscriberId, TargetUser, Other]),
      {error, other}
  end.

%%%_* Internal =================================================================

