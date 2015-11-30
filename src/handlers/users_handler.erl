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
  {Method, Req1} = cowboy_req:method(Req0),
  {PathInfo, Req} = cowboy_req:path_info(Req1),
  is_authorized(Method, PathInfo, Req, State).

is_authorized(?POST, [<<"create">>], Req, State) ->
  %% anybody can create a user
  {true, Req, State};
is_authorized(?POST, [<<"subscribe">> | _], Req0, State) ->
  %% only authenticated users can subscribe on other users
  case cowboy_req:header(?H_AUTH_TOKEN, Req0, []) of
    {[], Req} ->
      {{false, ?H_AUTH_TOKEN}, Req, State};
    {Token, Req} ->
      ?DEBUG("Token: ~p", [Token]),
      case tokens:is_valid(Token) of
        {ok, UserId} -> {true, Req, State#state{user_id = UserId}};
        false        -> {{false, ?H_AUTH_TOKEN}, Req, State}
      end
  end;
is_authorized(?POST, _, Req, State) ->
  cowboy_req:reply(404, [], <<>>, Req),
  {halt, Req, State}.

handle_get(Req, State) ->
  %% TODO: return info about user (general info, subscriptions, subscribers, etc.)
  %% check if user is private in is_authorized
  {<<>>, Req, State}.

handle_post(Req0, State) ->
  {PathInfo, Req} = cowboy_req:path_info(Req0),
  ?DEBUG("[~p] PathInfo: ~p", [?MODULE, PathInfo]),
  do_handle_post(PathInfo, Req, State).

do_handle_post([<<"create">>], Req0, State) ->
  {ok, Params, Req1} = cowboy_req:body_qs(Req0),
  Username = proplists:get_value(<<"username">>, Params),
  Pwdhash = erlpass:hash(proplists:get_value(<<"password">>, Params)),
  Email = proplists:get_value(<<"email">>, Params),
  {ok, Req} =
    case users:create(Username, Pwdhash, Email) of
      ok ->
        cowboy_req:reply(201, [], <<>>, Req1);
      {error, already_exists} ->
        Error = ffengine_json:encode({error, <<"user already exists">>}),
        cowboy_req:reply(400, [], Error, Req1)
    end,
  {halt, Req, State};
do_handle_post([<<"subscribe">>, Username], Req0, State) when is_binary(Username) ->
  %% TODO: handle private users
  %% TODO: do not subscribe users on themselves
  {ok, Req} =
    case users:subscribe(State#state.user_id, Username) of
      ok ->
        cowboy_req:reply(200, [], <<>>, Req0);
      {error, already_exists} ->
        Error = ffengine_json:encode({error, <<"already subscribed">>}),
        cowboy_req:reply(400, [], ffengine_json:encode(Error), Req0);
      {error, _Other} ->
        Error = ffengine_json:encode({error, <<"cannot subscribe">>}),
        cowboy_req:reply(400, [], ffengine_json:encode(Error), Req0)
    end,
  {halt, Req, State}.
