-module(sessions_handler).

-include("../ffengine.hrl").

-export([ init/3
        , content_types_provided/2
        , content_types_accepted/2
        , allowed_methods/2
%        , is_authorized/2
        , handle_get/2
        , handle_post/2
        ]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, handle_post}], Req, State}.

allowed_methods(Req, State) ->
  {[?GET, ?POST], Req, State}.

handle_get(Req, State) ->
  {<<>>, Req, State}.

handle_post(Req0, State) ->
  {ok, Params, Req1} = cowboy_req:body_qs(Req0),
  Username = proplists:get_value(<<"username">>, Params),
  Password = proplists:get_value(<<"password">>, Params),
  {ok, Req} =
    case users:read(Username) of
      {ok, User} ->
        case erlpass:match(Password, maps:get(pwdhash, User)) of
          true  ->
            {ok, Token} = tokens:read_default_token(maps:get(user_id, User)),
            Headers = [{<<"authToken">>, Token}],
            cowboy_req:reply(200, Headers, <<>>, Req1);
          false ->
            Json = ffengine_json:encode({error, <<"wrong password">>}),
            cowboy_req:reply(401, [], Json, Req1)
        end;
      {error, not_found} ->
        Json = ffengine_json:encode({error, <<"user not found">>}),
        cowboy_req:reply(401, [], Json, Req1)
    end,
  {halt, Req, State}.

