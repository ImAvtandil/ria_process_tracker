%%%-------------------------------------------------------------------
%%% @author sanr
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Янв. 2016 16:00
%%%-------------------------------------------------------------------
-module(process_tracker).
-author("sanr").

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([
  start_process/2,
  stop_process/2
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  name,
  supervisor,
  function
}).

-record(process_entry, {
  id,
  ref,
  pid
}).

%%%===================================================================
%%% API
%%%===================================================================

%% process_tracker:start_process(default_tracker, "Start").
start_process(TrackerName, ChildName) ->
  gen_server:call(TrackerName, {make_process, ChildName}).

%% process_tracker:stop_process(default_tracker, "Start").
stop_process(TrackerName, ChildName) ->
  gen_server:cast(TrackerName, {stop_process, ChildName}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
  {Name, _, _} = Params,
  gen_server:start_link({local, Name}, ?MODULE, [Params], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{Name, Supervisor, Function}]) ->
  ets:new(Name, [public, named_table, {keypos, #process_entry.id}]),
  {ok, #state{name = Name, supervisor = Supervisor, function = Function}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({make_process, ChildName}, _From, State) ->
  Supervisor = State#state.supervisor,
  Function = State#state.function,
  Pid = case ets:lookup(State#state.name, ChildName) of
          [] ->
            {ok, Pid_} = Supervisor:Function(),
            Ref = erlang:monitor(process, Pid_),
            ets:insert(State#state.name, #process_entry{id = ChildName, ref = Ref, pid = Pid_}),
            Pid_;
          [#process_entry{pid = Pid_}] ->
            Pid_
        end,

  {reply, {ok, Pid}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({stop_process, Processname}, State) ->
  _ = case ets:lookup(State#state.name, Processname) of
    [] ->ok;
    [#process_entry{pid = Pid}] ->
      ets:delete(State#state.name, Processname),
      gen_server:cast(Pid, stop),
      ok
  end,
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _, process, Dead, _Reason}, #state{} = State) ->
  ets:select_delete(State#state.name, ets:fun2ms(fun(#process_entry{pid = Pid}) when Dead == Pid -> true end)),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
