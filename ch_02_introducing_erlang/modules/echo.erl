-module(echo).
-export([go/0, loop/0]).

go() ->
	Pid = spawn(echo, loop, []),
	Pid ! {self(), yo_whats_up},

	receive
		{Pid, Msg} ->
			io:format("~w~n", [Msg])
	end,
	Pid ! stop.

loop() ->
	receive
		{From, Msg} ->
			From ! {self(), Msg},
			loop();

		stop ->
			ok
	end.