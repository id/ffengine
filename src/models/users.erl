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
  %% get target user's non-private channels
  Res0 = ffengine_db:equery("select channel_id from get_user_open_channels($1);",
                            [TargetUser]),
  OpenChannels = case ffengine_db:parse_select_res(Res0) of
                   {ok, Channels1}    -> Channels1;
                   {error, not_found} -> []
                 end,
  %% get target user's private channels
  Res1 = ffengine_db:equery("select channel_id from get_user_private_channels($1);",
                            [TargetUser]),
  PrivateChannels = case ffengine_db:parse_select_res(Res1) of
                      {ok, Channels2}    -> Channels2;
                      {error, not_found} -> []
                    end,
  %% get user's home feed id
  Res2 = ffengine_db:equery("select feed_id from feeds where user_id = $1 and feed_name = 'home';",
                            [SubscriberId]),
  {ok, #{feed_id := FeedId}} = ffengine_db:parse_select_res(Res2),
  %% insert records in feed_channels
  InsertFeedChannels = [{?INSERT_FEED_CHANNEL, [FeedId, ChannelId]} ||
                         #{channel_id := ChannelId} <- OpenChannels],
  %% insert records in subscription_requests
  InsertSubscriptionRequests = [{?INSERT_SUBSCRIPTION_REQUEST, [FeedId, ChannelId]} ||
                                 #{channel_id := ChannelId} <- PrivateChannels],
  Batch = lists:append([InsertFeedChannels, InsertSubscriptionRequests]),
  Res = ffengine_db:execute_batch(Batch),
  case ffengine_db:verify_results(Res) of
    ok ->
      ok;
    ?ERROR_DUPLICATE_KEY = Error ->
      ?ERROR("subscribe user_id ~B on ~s failed: ~p",
             [SubscriberId, TargetUser, Error]),
      {error, already_exists};
    {error, _} = Error ->
      ?ERROR("subscribe user_id ~B on ~s failed: ~p",
             [SubscriberId, TargetUser, Error]),
      Error
  end.

%%%_* Internal =================================================================

