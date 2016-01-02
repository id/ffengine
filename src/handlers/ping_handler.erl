-module(ping_handler).

-include("../ffengine.hrl").

-export([ init/3
        , allowed_methods/2
        , content_types_provided/2
        , handle_request/2
        ]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[?GET], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_request}], Req, State}.

handle_request(Req, State) ->
  %% test that the node is running and is connected to the database
  Res = ffengine_db:squery("select 'pong' as reply"),
  {ok, Reply} = ffengine_db:parse_select_res(Res),
  cowboy_req:reply(200, [], ffengine_json:encode(Reply), Req),
  {halt, Req, State}.
