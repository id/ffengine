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

-record(state, {user_id, post_id}).

init(_Transport, _Req, _Opts) ->
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
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
  case cowboy_req:header(?H_AUTH_TOKEN, Req2, []) of
    {[], Req} ->
      case posts:is_public(Username, Permalink) of
        {ok, PostId} ->
          {true, Req, State#state{post_id = PostId}};
        false ->
          {{false, ?H_AUTH_TOKEN}, Req, State}
      end;
    {Token, Req} ->
      case tokens:is_valid(Token) of
        {ok, UserId} ->
          case posts:can_read(UserId, Username, Permalink) of
            {ok, PostId} ->
              {true, Req, State#state{user_id = UserId, post_id = PostId}};
            false ->
              {{false, ?H_AUTH_TOKEN}, Req, State}
          end;
        false ->
          {{false, ?H_AUTH_TOKEN}, Req, State}
      end
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

handle_get(Req0, #state{post_id = PostId} = State) ->
  {ok, Req} =
    case posts:read(PostId) of
      {ok, Post} ->
        cowboy_req:reply(200, [], ffengine_json:encode(Post), Req0);
      {error, not_found} ->
        Json = ffengine_json:encode({error, <<"post not found">>}),
        cowboy_req:reply(404, [], Json, Req0)
    end,
  {halt, Req, State}.

handle_post(Req0, #state{user_id = UserId} = State) ->
  {ok, Params, Req1} = cowboy_req:body_qs(Req0),
  Time = ffengine_utils:utc_time(),
  Permalink = ffengine_utils:time_to_url(Time),
  Body = proplists:get_value(<<"body">>, Params),
  {ok, Req} =
    case posts:create(UserId, Permalink, Body, Time) of
      {ok, Post} ->
        cowboy_req:reply(201, [], ffengine_json:encode(Post), Req1);
      {error, already_exists} ->
        Error = ffengine_json:encode({error, <<"post already exists">>}),
        cowboy_req:reply(400, [], Error, Req1);
      {error, _} ->
        Error = ffengine_json:encode({error, <<"cannot create post">>}),
        cowboy_req:reply(400, [], Error, Req1)
    end,
  {halt, Req, State}.

