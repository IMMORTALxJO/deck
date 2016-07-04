-module(first).
-export([test/1]).

test(0) -> [0];
test(N) -> [N|test(N-1)].

