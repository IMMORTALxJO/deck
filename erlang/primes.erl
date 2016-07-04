-module(primes).
-export([gen/2,gen2/2]).

gen(0,E)-> gen(2,E);
gen(S,E) ->
	A=[X||SL<-lists:seq(2,S),EL<-lists:seq(2,E),X<-[SL*EL],X>=S,X=<E],
	lists:seq(S,E)--A.

%% S - start of window
%% E - end of window
%% B - now counter
%% P - possible primes
gen2(S,E)->
	gen2(S,E,2,lists:seq(S,E),trunc(math:sqrt(E)) ).
gen2(S,E,B,P,T) when B > T -> P;
gen2(S,E,B,P,T)->
	TA=[X*B||X<-lists:seq(trunc(S/B),trunc(E/B)),X*B>=S,X*B=<E],
	gen2(S,E,B+1,P--TA,T).
