-module(comments_handler).

-include("../ffengine.hrl").

-export([ init/3
        , rest_init/2
        , content_types_accepted/2
        , allowed_methods/2
        , is_authorized/2
        , handle_post/2
        ]).

-record(state, {user_id}).

init(_Transport, _Req, _Opts) ->
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
      {Username, Req2} = cowboy_req:binding(username, Req1),
      {Permalink, Req} = cowboy_req:binding(permalink, Req2),
      %% if a user can read a post, they can comment it as well
      case posts:can_read(Token, Username, Permalink) of
        true ->
          {true, Req, State};
        false ->
          {{false, ?H_AUTH_TOKEN}, Req, State}
      end
  end.

handle_post(Req0, State) ->
  {Token, Req1} = cowboy_req:header(?H_AUTH_TOKEN, Req0, []),
  {Username, Req2} = cowboy_req:binding(username, Req1),
  {PostPermalink, Req3} = cowboy_req:binding(permalink, Req2),
  {ok, Params, Req4} = cowboy_req:body_qs(Req3),
  Ts = ffengine_utils:ts(),
  Body = proplists:get_value(<<"body">>, Params),
  ok = comments:create(Token, Username, PostPermalink, Body, Ts),
  {ok, Req} = cowboy_req:reply(201, [], <<>>, Req4),
  {halt, Req, State}.

