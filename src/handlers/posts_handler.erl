-module(posts_handler).

-include("../ffengine.hrl").

-export([ init/3
        , rest_init/2
        , content_types_provided/2
        , content_types_accepted/2
        , allowed_methods/2
        , is_authorized/2
        , handle_get/2
        , handle_post/2
        ]).

-record(state, {user_id}).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _HandlerOpts) ->
  {ok, Req, #state{}}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_post}], Req, State}.

allowed_methods(Req, State) ->
  {[?GET, ?POST], Req, State}.

is_authorized(Req0, State) ->
  {Method, Req} = cowboy_req:method(Req0),
  is_authorized(Method, Req, State).

is_authorized(?GET, Req0, State) ->
  {Username, Req1} = cowboy_req:binding(username, Req0),
  {Permalink, Req2} = cowboy_req:binding(permalink, Req1),
  {Res, Req} = case cowboy_req:header(?H_AUTH_TOKEN, Req2, []) of
                 {[], Req3} ->
                   {posts:is_public(Username, Permalink), Req3};
                 {Token, Req3} ->
                   {posts:can_read(Token, Username, Permalink), Req3}
               end,
  case Res of
    true  -> {true, Req, State};
    false -> {{false, ?H_AUTH_TOKEN}, Req, State}
  end;
is_authorized(?POST, Req0, State) ->
  case cowboy_req:header(?H_AUTH_TOKEN, Req0, []) of
    {[], Req} ->
      {{false, ?H_AUTH_TOKEN}, Req, State};
    {Token, Req} ->
      ?DEBUG("Token: ~p", [Token]),
      case tokens:is_valid(Token) of
        {ok, UserId} -> {true, Req, State#state{user_id = UserId}};
        false        -> {{false, ?H_AUTH_TOKEN}, Req, State}
      end
  end.

handle_get(Req0, State) ->
  {Username, Req1} = cowboy_req:binding(username, Req0),
  {Permalink, Req2} = cowboy_req:binding(permalink, Req1),
  {ok, Req} = case posts:read(Username, Permalink) of
                {ok, Post} ->
                  cowboy_req:reply(200, [], ffengine_json:encode(Post), Req2);
                {error, not_found} ->
                  Json = ffengine_json:encode({error, <<"post not found">>}),
                  cowboy_req:reply(401, [], Json, Req2)
              end,
  {halt, Req, State}.

handle_post(Req0, #state{user_id = UserId} = State) ->
  {ok, Params, Req1} = cowboy_req:body_qs(Req0),
  Ts = ffengine_utils:ts(),
  Permalink = ffengine_utils:ts_to_url(Ts),
  Body = proplists:get_value(<<"body">>, Params),
  {ok, Req} =
    case posts:create(UserId, Permalink, Body, Ts) of
      {ok, Post} ->
        cowboy_req:reply(201, [], ffengine_json:encode(Post), Req1);
      {error, already_exist} ->
        %% TODO: better error message
        Error = ffengine_json:encode({error, <<"post with this permalink already exists">>}),
        cowboy_req:reply(400, [], Error, Req1)
    end,
  {halt, Req, State}.

