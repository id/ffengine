-module(ffengine_sup).
-behaviour(supervisor).

-include("ffengine.hrl").

-export([ start_link/0
        , init/1
        ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, Db} = application:get_env(db),
  PoolSize = proplists:get_value(conn_pool_size, Db, 4),
  PoolMaxSize = proplists:get_value(conn_pool_max_size, Db, 8),
  PoolArgs = [ {name, {local, ?DB_CONN_POOL}}
             , {worker_module, ffengine_epgsql_worker}
             , {size, PoolSize}
             , {max_overflow, PoolMaxSize}
             ],
  Spec = poolboy:child_spec(?DB_CONN_POOL, PoolArgs, Db),
  {ok, {{one_for_one, 10, 10}, [Spec]}}.
