%%% ============================================================================
%%% @doc
%%% @end
%%% ============================================================================
-module(ffengine_epgsql_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("ffengine.hrl").

%% API
-export([ start_link/1
        , squery/2
        , equery/3
        , execute_batch/2
        , reload/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { conn    :: pid()
               , t_stmts :: ets:tid()
               , args    :: proplists:proplist()
               }).

%%%_ * API ---------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

squery(Pid, Sql) ->
  gen_server:call(Pid, {squery, Sql}).

equery(Pid, StmtName, Params) ->
  gen_server:call(Pid, {equery, StmtName, Params}).

execute_batch(Pid, PreBatch) ->
  gen_server:call(Pid, {execute_batch, PreBatch}).

reload(Pid) ->
  gen_server:call(Pid, reload).

%%%_ * gen_server callbacks ----------------------------------------------------
init(Args) ->
  {ok, Conn} = connect(Args),
  Tid = ets:new(t, [protected]),
  ok = prepare_statements(Conn, Tid),
  {ok, #state{conn = Conn, t_stmts = Tid, args = Args}}.

handle_call(reload, _From, State) ->
  ok = epgsql:close(State#state.conn),
  {ok, Conn} = connect(State#state.args),
  Tid = State#state.t_stmts,
  ets:match_delete(Tid, '_'),
  ok = prepare_statements(Conn, Tid),
  {reply, ok, State#state{conn = Conn}};
handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
  {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
  {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call({execute_batch, Stmts}, _From, State) ->
  Conn = State#state.conn,
  Tid = State#state.t_stmts,
  Batch =
    lists:map(
      fun({Name, Params}) ->
          [{_, S}] = ets:lookup(Tid, Name),
          {S, Params}
      end, Stmts),
  {reply, epgsql:execute_batch(Conn, Batch), State};
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

%%%_ * Internal functions ------------------------------------------------------
connect(Args) ->
  Hostname = proplists:get_value(host, Args),
  Database = proplists:get_value(database, Args),
  Username = proplists:get_value(username, Args),
  ?INFO("~p is connecting to ~s at ~s with user ~s~n",
         [self(), Database, Hostname, Username]),
  epgsql:connect(Args).

prepare_statements(Conn, Tid) ->
  ok = epgsql:update_type_cache(Conn, [ <<"channel_mode">>
                                      , <<"channel_type">>
                                      , <<"post_activity">>]),
  F = fun({Name, Sql, Types}) ->
          {ok, S} = epgsql:parse(Conn, Name, Sql, Types),
          ets:insert(Tid, {Name, S})
      end,
  lists:foreach(F, ffengine_db:statements()).

