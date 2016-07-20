%%%-------------------------------------------------------------------
%%% @author BakaRay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июль 2016 2:16
%%%-------------------------------------------------------------------
-module(tcp_server_sup).
-author("BakaRay").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART,5).
-define(MAX_TIME, 60).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link(Port) ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Port) ->
  W=supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]),
  %io:write("returns ~p~n",[W]),
  W.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

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



init([Port]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = ?MAX_RESTART,
  MaxSecondsBetweenRestarts = ?MAX_TIME,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  LRestart = permanent,
  LShutdown = infinity,
  LType = worker,

  ListenerChild = {tcp_listener,
    {tcp_listener, start_link, [Port]},
    LRestart, LShutdown, LType,
    [tcp_listener]},

  CRestart= permanent,
  CShutdown=infinity,
  CType=supervisor,

  ClientChild = {tcp_client_sup,
    {tcp_client_sup, start_link,[]},
    CRestart, CShutdown, CType,
    [tcp_client_sup]
    },

  {ok, {SupFlags, [ListenerChild, ClientChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
