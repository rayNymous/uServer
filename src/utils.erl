%%%-------------------------------------------------------------------
%%% @author BakaRay
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Июль 2016 18:49
%%%-------------------------------------------------------------------
-module(utils).
-author("BakaRay").

%% API
-export([for/3, map/2]).

for(Max,Max,F)-> [F(Max)];
for(I,Max,F)->[F(I)| for(I+1, Max,F)].

map(_,[])->[];
map(F,[H|T])->[F(H)|map(F,T)].