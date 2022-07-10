% Fail Safe!

	% in "Recursion and Pattern Matching" we saw the factorial example, and
	% how passing a negative number to the function causes it to raise an
	% exception. This also happens when factorial is applied to something
	% that isn't a number, in this case atom zero:


		1> ex1:factorial(zero).
		% ** exception error: bad argument in an arithmetic expression
		% in function ex1:factorial/1

	% The alternative to this would be to program defensively, and explicitly
	% identify the case of negative numbers, as well as arguments of any
	% other type, by means of a catch-all clause:

		factorial(0) -> 1;
		factorial(N) when N > 0, is_integer(N) ->
			N * factorial(N-1);
		factorial(_) ->
			{error, bad_argument}.

	% The effect of this would be to require every caller of the function to
	% deal not with proper results (like 120 = factorial(5)) but also
	% improper ones of the format {error,bad_argument}. If we do this,
	% clients of any function need to understand its failure modes and
	% provide ways of dealing with them, mixing correct computation and
	% error-handling code. How do you handle errors or corrupt data when you
	% do not know what these errors or how the data got corrupted?

	% The Erlang design philosophy says "let it fail!" so that a function,
	% process, or other running entity deals only with the correct case and
	% leaves it to other parts of the system (specifically designed to do
	% this) to deal with failure. One way of dealing with failure in
	% sequential code is to use the mechanism for exception handling given by
	% the try-catch construct. Using the definition:

		factorial(0) -> 1;
		factorial(N) when N > 0, is_integer(N) ->
			N * factorial(N-1).

	% we can see the construct

		2> ex1:factorial(zero).
		% ** exception error: no function clause matching ex1:factorial(zero)
		3> try ex1:factorial(zero) catch Type:Error -> {Type, Error} end.
		% {error,function_clause}
		4> try ex1:factorial(-2) catch Type:Error -> {Type, Error} end.
		% {error,function_clause}
		5> try ex1:factorial(-2) catch error:Error2 -> {error, Error2} end.
		% {error,function_clause}
		6> try ex1:factorial(-2) catch error:Error3 -> {error, Error3};
		6> exit:Reason -> {exit, Reason} end.
		% {error,function_clause}

	% The try-catch construct gives the user the opportunity to match on the
	% different kinds of exceptions in the clauses, handling them
	% individually. In this example, we match on an error exception caused by
	% a pattern match failure. There are also exit and throw exceptions, the
	% first being the result of a process calling the exit BIF and the latter
	% the result of a user-generated exception using the throw expression.
