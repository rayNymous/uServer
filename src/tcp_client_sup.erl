%%%-------------------------------------------------------------------
%%% @author BakaRay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июль 2016 1:57
%%%-------------------------------------------------------------------
-module(tcp_client_sup).
-author("BakaRay").

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5).
-define(MAX_TIME,69).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
%-spec(start_link() ->
%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% We need it to start our client
start_child()->
  supervisor:start_child(tcp_client_sup, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
%-spec(init(Args :: term()) ->
%  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
%    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
%    [ChildSpec :: supervisor:child_spec()]
%  }} |
%  ignore |
%  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = ?MAX_RESTART,
  MaxSecondsBetweenRestarts = ?MAX_TIME,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 2000,
  Type = worker,

  AChild = { undefined,
    {tcp_fsm, start_link, []},
    Restart, Shutdown, Type,
    [tcp_fsm]},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
