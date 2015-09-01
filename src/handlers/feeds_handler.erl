-module(feeds_handler).

-include("../ffengine.hrl").

-export([ init/3
        , rest_init/2
        , content_types_provided/2
        , content_types_accepted/2
        , allowed_methods/2
        , is_authorized/2
        , handle_get/2
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
  {[?GET], Req, State}.

is_authorized(Req0, State) ->
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
  {Feed, Req1} = cowboy_req:binding(feed, Req0),
  {ok, Req} =
    case feeds:read(State#state.user_id, Feed) of
      {ok, Posts} ->
        cowboy_req:reply(200, [], ffengine_json:encode(Posts), Req1);
      {error, not_found} ->
        Json = ffengine_json:encode({error, <<"feed not found">>}),
        cowboy_req:reply(401, [], Json, Req1)
    end,
  {halt, Req, State}.

