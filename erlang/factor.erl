%% factorize number
%% factor:num(120).
%% [2,2,2,3,5]
%% factor:num(123312415555).
%% [5,7,233,15121081]

-module(factor).
-export([num/1]).

num(1)-> [1];
num(N)-> num(N,2).

num(1,X)-> [];
num(N,X) when N rem X =:= 0 ->
	[X | num(trunc(N/X),X)];
num(N,X) -> num(N,X+1).
