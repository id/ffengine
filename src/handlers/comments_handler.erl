-module(comments_handler).

-include("../ffengine.hrl").

-export([ init/3
        , rest_init/2
        , content_types_accepted/2
        , allowed_methods/2
        , is_authorized/2
        , handle_post/2
        ]).

-record(state, {user_id, post_id}).

init(_Transport, _Req, _Opts) ->
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _HandlerOpts) ->
  {ok, Req, #state{}}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_post}], Req, State}.

allowed_methods(Req, State) ->
  {[?POST], Req, State}.

is_authorized(Req0, State) ->
  case cowboy_req:header(?H_AUTH_TOKEN, Req0, []) of
    {[], Req} ->
      {{false, ?H_AUTH_TOKEN}, Req, State};
    {Token, Req1} ->
      case tokens:is_valid(Token) of
        {ok, UserId} ->
          {Username, Req2} = cowboy_req:binding(username, Req1),
          {Permalink, Req} = cowboy_req:binding(permalink, Req2),
          case posts:can_read(UserId, Username, Permalink) of
            {ok, PostId} ->
              {true, Req, State#state{user_id = UserId, post_id = PostId}};
            false ->
              {{false, ?H_AUTH_TOKEN}, Req, State}
          end;
        false ->
          {{false, ?H_AUTH_TOKEN}, Req1, State}
      end
  end.

handle_post(Req0, #state{user_id = UserId, post_id = PostId} = State) ->
  {ok, Params, Req1} = cowboy_req:body_qs(Req0),
  Time = ffengine_utils:utc_time(),
  Body = proplists:get_value(<<"body">>, Params),
  {ok, Req} =
    case comments:create(UserId, PostId, Body, Time) of
      {ok, Comment} ->
        cowboy_req:reply(201, [], ffengine_json:encode(Comment), Req1);
      {error, already_exists} ->
        Error = ffengine_json:encode({error, <<"comment already exists">>}),
        cowboy_req:reply(400, [], Error, Req1);
      {error, _} ->
        Error = ffengine_json:encode({error, <<"cannot create comment">>}),
        cowboy_req:reply(400, [], Error, Req1)
    end,
  {halt, Req, State}.

