%%%-------------------------------------------------------------------
%%% @author BakaRay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июль 2016 2:57
%%%-------------------------------------------------------------------
-module(tcp_fsm).
-author("BakaRay").

-behaviour(gen_fsm).

%% API
-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([wait_for_socket/2, wait_for_data/2]).

-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4
  ]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 600000).

-record(state,
{socket, % client socket,
  addr % client ip
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
 % gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
  gen_fsm:start_link(?MODULE, [],[]).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket)->
  gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #state{}} |
  {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, wait_for_socket, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
%-spec(state_name(Event :: term(), State :: #state{}) ->
%  {next_state, NextStateName :: atom(), NextState :: #state{}} |
%  {next_state, NextStateName :: atom(), NextState :: #state{},
%    timeout() | hibernate} |
%  {stop, Reason :: term(), NewState :: #state{}}).
%state_name(_Event, State) ->
%  {next_state, state_name, State}.
wait_for_socket({socket_ready, Socket}, StateData)->% when is_port(Socket)->
  inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),
  {ok, {Address, _Port}}=inet:peername(Socket),
  error_logger:info_msg("socket_ready for IP: ~p and port ~p ~n",[Address, _Port]),
  {next_state, wait_for_data, StateData#state{socket= Socket, addr= Address}, ?TIMEOUT};  % we go to this state after timeout

wait_for_socket(Other, StateData) ->
  error_logger:error_msg("State: wait_for_socket, Unexpected message: ~p~n", [Other]),
  {next_state, wait_for_socket, StateData}.









wait_for_data({data, Bin}, #state{socket=Socket, addr=Address}=StateData)->
  %%just print it
  error_logger:info_msg("Received data ~p~n", [erlang:binary_to_list(Bin)]),
  ok=gen_tcp:send(Socket, Bin),
  error_logger:info_msg("Send data back to client ~p ~n", [Address]),
  inet:setopts(Socket, [binary, {packet, 2}, {nodelay, true}, {active, once}, {keepalive, true}]),
  {next_state, wait_for_data, StateData, ?TIMEOUT};

wait_for_data(timeout, #state{addr=Address}=StateData)->
  error_logger:info_msg("~p CLient Connection timeout - closing.~n", [Address]),
  {stop, normal, StateData};

wait_for_data( Other, StateData)->
  error_logger:error_msg("State: wait_for_data. Unexpected message: ~p~n",[Other]),
  {next_state, wait_for_data,StateData}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
%-spec(state_name(Event :: term(), From :: {pid(), term()},
%    State :: #state{}) ->
%  {next_state, NextStateName :: atom(), NextState :: #state{}} |
%  {next_state, NextStateName :: atom(), NextState :: #state{},
%    timeout() | hibernate} |
%  {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
%  {reply, Reply, NextStateName :: atom(), NextState :: #state{},
%    timeout() | hibernate} |
%  {stop, Reason :: normal | term(), NewState :: #state{}} |
%  {stop, Reason :: normal | term(), Reply :: term(),
%    NewState :: #state{}}).
%state_name(_Event, _From, State) ->
%  Reply = ok,
%  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
%handle_info(_Info, StateName, State) ->
%  {next_state, StateName, State}.
handle_info({tcp, _Socket, Bin}, StateName, StateData)->
  ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, _Socket}, _StateName, #state{addr=Address}=StateData)->
  error_logger:info_msg("~p Client disconnected. ~n", [Address]),
  {stop, normal, StateData};

handle_info(_Info, StateName, StateData)->
  {next_state, StateName, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
