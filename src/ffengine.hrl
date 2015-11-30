-ifndef(_FFENGINE_HRL_).
-define(_FFENGINE_HRL_, true).

-include_lib("lager/include/lager.hrl").

-define(DB_CONN_POOL, db_conn_pool).

-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(PUT, <<"PUT">>).
-define(DELETE, <<"DELETE">>).

-define(H_AUTH_TOKEN, <<"x-authentication-token">>).

-define(SQL_TRACE_OPTION, [{sql, true}]).

-define(SQL(Msg, Args),       lager:debug(?SQL_TRACE_OPTION, Msg, Args)).
-define(SQL(Msg),             ?SQL(Msg, [])).
-define(DEBUG(Msg, Args),     lager:debug(Msg, Args)).
-define(DEBUG(Msg),           ?DEBUG(Msg, [])).
-define(INFO(Msg, Args),      lager:info(Msg, Args)).
-define(INFO(Msg),            ?INFO(Msg, [])).
-define(NOTICE(Msg, Args),    lager:notice(Msg, Args)).
-define(NOTICE(Msg),          ?NOTICE(Msg, [])).
-define(WARNING(Msg, Args),   lager:warning(Msg, Args)).
-define(WARNING(Msg),         ?WARNING(Msg, [])).
-define(ERROR(Msg, Args),     lager:error(Msg, Args)).
-define(ERROR(Msg),           ?ERROR(Msg, [])).
-define(CRITICAL(Msg, Args),  lager:critical(Msg, Args),
-define(CRITICAL(Msg),        ?CRITICAL(Msg, [])).
-define(ALERT(Msg, Args),     lager:alert(Msg, Args),
-define(ALERT(Msg),           ?ALERT(Msg, [])).
-define(EMERGENCY(Msg, Args), lager:emergency(Msg, Args),
-define(EMERGENCY(Msg),       ?EMERGENCY(Msg, [])).

-define(SHOULD_LOG_OR_TRACE(Level),
        begin
          {CurrentLevel, Traces} = lager_config:get(loglevel, {?LOG_NONE, []}),
          (lager_util:level_to_num(Level) band CurrentLevel) /= 0 orelse
            Traces /= []
        end
       ).


-define(INSERT_USER,         "insert_user").
-define(INSERT_TOKEN,        "insert_token").
-define(INSERT_CHANNEL,      "insert_channel").
-define(INSERT_USER_CHANNEL, "insert_user_channel").
-define(INSERT_FEED,         "insert_feed").
-define(INSERT_FEED_CHANNEL, "insert_feed_channel").
-define(INSERT_POST,         "insert_post").
-define(INSERT_POST_CHANNEL, "insert_post_channel").
-define(INSERT_POST_ACTIVITY_HIST, "insert_post_activity_hist").
-define(INSERT_COMMENT,      "insert_comment").
-define(INSERT_POST_COMMENTS_CHANNEL, "insert_post_comments_channel").
-define(UPDATE_POST_RATING,  "update_post_rating").

-endif. % _FFENGINE_HRL_

