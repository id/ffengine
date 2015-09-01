%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(ffengine_epgsql_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("ffengine.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conn}).

%%%_ * API ---------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%%_ * gen_server callbacks ----------------------------------------------------
init(Args) ->
  Hostname  = proplists:get_value(hostname, Args),
  Port      = proplists:get_value(port, Args, 5432),
  Username  = proplists:get_value(username, Args),
  Password  = proplists:get_value(password, Args),
  Database  = proplists:get_value(database, Args),
  Ssl       = proplists:get_value(ssl, Args, false),
  SslOpts   = proplists:get_value(ssl_opts, Args, []),
  Timeout   = proplists:get_value(timeout, Args, 10000),
  PgsqlOptions = [ {database, Database}
                 , {timeout, Timeout}
                 , {port, Port}
                 , {ssl, Ssl}
                 , {ssl_opts, SslOpts}
                 ],
  ?DEBUG("Postgres connection args: ~p, ~p, ~p, ~p",
         [Hostname, Username, Password, PgsqlOptions]),
  {ok, Conn} = epgsql:connect(Hostname, Username, Password, PgsqlOptions),
  {ok, #state{conn = Conn}}.

handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
  {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
  {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, #state{conn = Conn}) ->
  ?DEBUG("Terminating with reason ~p", [Reason]),
  ok = epgsql:close(Conn),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

