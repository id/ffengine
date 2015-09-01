-module(ping_handler).

-include("../ffengine.hrl").

-export([ init/3
        , content_types_provided/2
        , handle_request/2
        ]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[{<<"text/plain">>, handle_request}], Req, State}.

handle_request(Req, State) ->
  {<<"pong\n">>, Req, State}.
