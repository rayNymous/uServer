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

%simple_test() ->
%  io:write("ololol"),
%  ?assert(true).


init_test()->
  u_server:start(normal, 2222),
  {ok, S}= gen_tcp:connect({127,0,0,1},2222, [{packet,2}]),
  ?assertNotEqual(0,S),
  error_logger:info_msg("Trying to send data ~n"),
  gen_tcp:send(S, <<"FUCKYOU">>),
  Exit= u_server:stop(normal),
 ?assertEqual(Exit,ok).