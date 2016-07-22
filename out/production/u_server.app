%%%-------------------------------------------------------------------
%%% @author BakaRay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июль 2016 3:48
%%%-------------------------------------------------------------------
{application, u_server, [
  {description, "Erlang TCP Server"},
  {vsn, "1.0"},
  {modules, [u_server,
    tcp_server_sup,
    tcp_listener,
    tcp_client_sup,
    tcp_fsm]},
  {registered,   []},
  {applications, [kernel, stdlib]},
  {mod, {erltcps, []}},
  {env, []}
]}.