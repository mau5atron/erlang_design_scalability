-module(ex1).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) when	N < 0 -> io:format("Cannot have negative input.~n");
factorial(N) ->
	N * factorial(N-1).