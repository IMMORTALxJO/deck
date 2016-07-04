-module(bin).
-export([gen/1]).

gen(0) -> [];
gen(N) when N rem 2 =:= 0 -> 
	[0|gen(trunc(N/2) )];
gen(N) when N rem 2 =:= 1 ->
	[1|gen(trunc( (N-1)/2 ))].
