%% Generate primes lower when input arg
%% primes:gen3(100).
%% [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

-module(primes).
-export([gen/1]).

gen(S)-> gen(lists:seq(2,S),[]).
gen([],P) -> lists:reverse(P);
gen([H|T],P)->	gen( [X||X<-T,X rem H /= 0],[H|P]).
