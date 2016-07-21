%%%-------------------------------------------------------------------
%%% @author BakaRay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июль 2016 5:32
%%%-------------------------------------------------------------------
-module(init_tests).
-author("BakaRay").
-include_lib("eunit/include/eunit.hrl").


-define(TestProcessCount,13).


%simple_test() ->
%  io:write("ololol"),
%  ?assert(true).


%init_test()->
%  u_server:start(normal, 3333),
%  {ok, S}= gen_tcp:connect({127,0,0,1},3333, [{packet,2}]),
%  ?assertNotEqual(0,S),
% error_logger:info_msg("Trying to send data ~n"),
%  gen_tcp:send(S, <<"FUCKYOU">>).
%  Exit= u_server:stop(normal),

load_test()->
  u_server:start(normal,3333),
  utils:for(0,?TestProcessCount,
    fun(I)->
      {ok,S}=gen_tcp:connect({127,0,0,1},3333, [{packet,raw}]),
      error_logger:info_msg("Listener ~.10B is up ~n", [I]),
      timer:sleep(100),
      gen_tcp:send(S, list_to_binary("hello")),
      error_logger:info_msg("Data Send to Socket~n")
    end),
  error_logger:info_msg("All Connection checked ~n"),
  u_server:stop(normal).

%stop_test()->
%  Exit= u_server:stop(normal),
% ?assertEqual(Exit,ok).