-module(for).
-export([gen/1,gen/2,map/2]).

gen(N)->gen(0,N).

gen(NOW,NOW) -> [NOW];
gen(NOW,TARGET) when NOW<TARGET -> [NOW|gen(NOW+1,TARGET)];
gen(NOW,TARGET) when NOW>TARGET -> [NOW|gen(NOW-1,TARGET)].

map([],F) -> [];
map([H|T],F) -> [ F(H) | map(T,F) ].
