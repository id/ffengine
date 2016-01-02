-module(users_handler).

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
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #state{}}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_post}],
   Req, State}.

allowed_methods(Req, State) ->
  {[?GET, ?POST], Req, State}.

is_authorized(Req0, State) ->
  {Method, Req} = cowboy_req:method(Req0),
  is_authorized(Method, Req, State).

is_authorized(?GET, Req, State) ->
  {true, Req, State};
is_authorized(?POST, Req0, State) ->
  {Username, Req1} = cowboy_req:binding(username, Req0),
  case Username of
    <<"new">> ->
      {true, Req1, State};
    <<"subscribe">> ->
      case cowboy_req:header(?H_AUTH_TOKEN, Req1, []) of
        {[], Req} ->
          {{false, ?H_AUTH_TOKEN}, Req, State};
        {Token, Req} ->
          case tokens:is_valid(Token) of
            {ok, UserId} ->
              {true, Req, State#state{user_id = UserId}};
            false ->
              {{false, ?H_AUTH_TOKEN}, Req, State}
          end
      end;
    _ ->
      Error = ffengine_json:encode({[{error, <<"bad request">>}]}),
      {ok, Req} = cowboy_req:reply(400, [], Error, Req1),
      {halt, Req, State}
  end.

%% get user info
handle_get(Req0, State) ->
  {Username, Req1} = cowboy_req:binding(username, Req0),
  {ok, Req} =
    case users:read(Username) of
      {ok, User} ->
        cowboy_req:reply(200, [], ffengine_json:encode(User), Req1);
      {error, not_found} ->
        Json = ffengine_json:encode({error, <<"user not found">>}),
        cowboy_req:reply(404, [], Json, Req1)
    end,
  {halt, Req, State}.

%% create new user
handle_post(Req0, State) ->
  {Username, Req1} = cowboy_req:binding(username, Req0),
  {ok, Req} =
    case Username of
      <<"new">>       -> create(Req1, State);
      <<"subscribe">> -> subscribe(Req1, State)
    end,
  {halt, Req, State}.

create(Req0, _State) ->
  {ok, Params, Req1} = cowboy_req:body_qs(Req0),
  Username = proplists:get_value(<<"username">>, Params),
  Pwdhash = erlpass:hash(proplists:get_value(<<"password">>, Params)),
  Email = proplists:get_value(<<"email">>, Params),
  case users:create(Username, Pwdhash, Email) of
    ok ->
      cowboy_req:reply(201, [], <<>>, Req1);
    {error, already_exists} ->
      Error = ffengine_json:encode({error, <<"user already exists">>}),
      cowboy_req:reply(400, [], Error, Req1)
  end.

subscribe(Req0, State) ->
  {ok, Params, Req} = cowboy_req:body_qs(Req0),
  Username = proplists:get_value(<<"username">>, Params),
  case users:subscribe(State#state.user_id, Username) of
    ok ->
      cowboy_req:reply(200, [], <<>>, Req);
    {error, already_exists} ->
      Error = ffengine_json:encode({[{error, <<"already subscribed">>}]}),
      cowboy_req:reply(400, [], Error, Req);
    {error, _Other} ->
      Error = ffengine_json:encode({[{error, <<"cannot subscribe">>}]}),
      cowboy_req:reply(400, [], Error, Req)
  end.
