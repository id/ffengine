%% @private
-module(ffengine_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ffengine/src/ffengine.hrl").

-define(URI, "http://localhost:3000").

%%%_* ct callbacks =============================================================

suite() -> [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
  application:start(inets),
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(Case, Config) ->
  ct:pal("=== [~p] starting ~p ===", [?MODULE, Case]),
  try
    ?MODULE:Case({init, Config})
  catch
    error : function_clause ->
      Config
  end.

end_per_testcase(Case, Config) ->
  try
    ?MODULE:Case({'end', Config})
  catch
    error : function_clause ->
      ok
  end,
  ct:pal("=== [~p] ~p end ===", [Case]),
  ok.

all() -> [F || {F, _A} <- module_info(exports),
                  case atom_to_list(F) of
                    "t_" ++ _ -> true;
                    _         -> false
                  end].

%%%_* Test functions ===========================================================

t_ping(Config) when is_list(Config) ->
  {ok, Response} = httpc:request(post, {?URI ++ "/ping", []}, []),
  ct:pal("Response = ~p", [Response]).

