-module(for).
-export([gen/1,gen/2,map/2]).


%% generator of	number lists
%% for:gen(10).
%% [0,1,2,3,4,5,6,7,8,9,10]
%% for:gen(3,-3).
%% [3,2,1,0,-1,-2,-3]


gen(N)->gen(0,N).

gen(NOW,NOW) -> [NOW];
gen(NOW,TARGET) when NOW<TARGET -> [NOW|gen(NOW+1,TARGET)];
gen(NOW,TARGET) when NOW>TARGET -> [NOW|gen(NOW-1,TARGET)].

%% lists mapper
%% for:map([0,1,2,3],fun(X)->X*2 end).
%% [0,2,4,6]

map([],F) -> [];
map([H|T],F) -> [ F(H) | map(T,F) ].
