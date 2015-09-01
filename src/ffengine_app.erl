-module(ffengine_app).
-behaviour(application).

-export([ start/2
        , stop/1
        ]).

-define(C_ACCEPTORS,  100).

%%% Application callbacks ------------------------------------------------------
start(_Type, _Args) ->
  Routes    = routes(),
  Dispatch  = cowboy_router:compile(Routes),
  Port      = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = [{env, [{dispatch, Dispatch}]}],
  {ok, _}   = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
  ffengine_sup:start_link().

stop(_State) ->
  ok.

%%% Internal functions ---------------------------------------------------------
routes() ->
  [{'_', [ {"/v1/ping", ping_handler, []}
         , {"/v1/users/[...]", users_handler, []}
         , {"/v1/sessions", sessions_handler, []}
         , {"/v1/posts/[:username/:permalink]", posts_handler, []}
         , {"/v1/comments/:username/:permalink", comments_handler, []}
         , {"/v1/feeds/:feed", feeds_handler, []}
         ]}
  ].

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port} = application:get_env(http_port),
      Port;
    Port ->
      list_to_integer(Port)
  end.
