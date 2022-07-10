% Erlang was heavily influenced by other functional programming languages.
% One functional principle is to treat functions as first-class citizens;
% they can be assigned to variables, be part of complex data structures, be
% passed as function arguments, or be returned as the results of function
% calls. We refer to the functional data type as an anonymous function, or
% fun for short. Erlang also provides constructs that allow you to define
% lists by "generate and test," using the analogue of comprehensions in set
% theory. Let's first start with anonymous functions: functions that are
% not named and not defined in an Erlang module.

% -------------------------------------------------------------------------

% Fun with Anonymous functions

% Functions that takes funs as arguments are called higher-order functions.
% An example of such a function is filter, where a predicate is represented
% by a fun that returns true or false, applied to the elements of a list.
% filter returns a list made up of those elements that have the required
% property; namely, those for which the fun returns true. We often use the
% term "predicate" to refer to a fun that, based on certain conditions
% defined in the function, returns the atoms true or false. Here is an
% example of how filter/2 could be implemented.

-module(ex3).
-export([filter/2, is_even/1]).

filter(P, []) -> [];
filter(P, [X|Xs]) ->
	case P(X) of
		true ->
			[X|filter(P, Xs)];
		_ ->
			filter(P, Xs)
	end.

is_even(X) ->
	X rem 2 == 0.

% To use filter, you need to pass it a function and a list. One way to pass
% the function is to use a fun expression, which is a way od defining an
% anonymous function. In shell command 2, show next, you can see an example
% of an anonymous function that tests for its argument being an even
% number:

2> ex3:filter(fun(X) -> X rem 2 == 0 end, [1,2,3,4]).
% [2,4]
3> ex3:filter(fun ex3:is_even/1, [1,2,3,4]).
% [2,4]

% A fun does not have to be anonymous, and could instead refer to a local
% or global function definition. In shell command 3, we described the
% function by fun ex3:is_even/1; i.e. by giving its module, name, and
% arity. Anonymous functions can also be spawned as the body of a process
% and passes in messages between processes; we look at processes in general
% after the next topic.

% If you're using Erlang/OTP 17.0 or newer. there's another way a fun does
% not have to be anonymous: it can be given a name. This feature is
% especially handy in the shell as it allows for easy definition of
% recursive anonymous functions. For example, we can implement the
% equivalent of ex3:filter/2 in the shell like this:

4> F = fun Filter(_, []) -> [];
4> Filter(P, [X|Xs]) -> case P(X) of true -> [X|Filter(P, Xs)];
4> false -> Filter(P, Xs) end end.
% #Fun<erl_eval.36.90072148>
5> Filter(fun(X) -> X rem 2 == 0 end, [1,2,3,4]).
% * 1: variable 'Filter' is unbound
6> F(fun(X) -> X rem 2 == 0 end, [1,2,3,4]).
% [2,4]

% We name our recursive function Filter by putting that name just after the
% fun keyword. Note that the name has to appear in both function clauses:
% the one of the two lines, which handles the empty list case, and the one
% defined on the next two lines, which handles the case when the list
% isn't empty. You can see two places in the body of the second clause
% where we recursively call Filter to handle remaining elements in the
% list. But even though the function has the name Filter, we still assign
% it to shell variable F because the name Filter is local to the function
% itself, and thus can't be used outside the body to invoke it, as our
% attempt to call it on line 5 shows. On line 6, we invoke the named fun
% via F and it works as expected. And because shell variables and function
% names are in different scopes, we could have used the shell variable name
% Filter rather than F, thus naming the function the same way in both
% scopes.

% -------------------------------------------------------------------------

% List Comprehensions: Generate and Test

	% Many of the examples we have looked at so far deal with the
	% manipulation of lists. We've used recursive functions on them, as well
	% as higher-order functions. Another approach is to use list
	% comprehensions, expressions that generate elements and apply tests (or
	% filters) to them. The format is like this:

		% [Expression || Generators, Tests, Generators, Tests]

	% where each Generator has the format

		% X <- [2, 3, 5, 7, 11]

	% The effect of this is to successfully bind the variable X to the values
	% 2, 3, 5, 7, 11. In other words, it generates the elements from the
	% list: The symbol <- is meant to suggest the "element of" symbol for
	% sets, ∈. In this example, X is called a bound variable. We've shown
	% only one bound variable here, but a list of comprehension can be built
	% from multiple bound variables and generators; we show some examples
	% later in this section.

	% The Tests are Boolean expressions, which are evaluated for each
	% combination of values of the bound variables. If all the Tests in the
	% group return true, then the Expression is generated from the current
	% values of the bound variables. The use of Tests in a list comprehension
	% is optional. The list comprehension construct as a whole generates a
	% list of results, one for each combination of values of the bound
	% variables that passes all the tests.

	% As a first example, we could rewrite the function filter/2 as a list
	% comprehension:
	
		% filter(P, Xs) -> [ X || X<-Xs, P(X)].

	% In the lsit comprehension, the first x is the expression, X<-Xs is the
	% generator, and P(X) is the test. Each value from the generator is
	% tested with the test, and the expression comprises only those values
	% for which the test returns true. Values for which the test returns
	% false are simply dropped. We can use list comprehensions directly in
	% our programs, as in the previous filter/2 example, or in the erlang
	% shell.

		1> [Element || Element <- [1,2,3,4], Element rem 2 == 0].
		% [2,4]
		2> [Element || Element <- [1,2,3,4], ex3:is_even(Element)].
		% [2,4]
		3> [Element || Element <- lists:seq(1,4), Element rem 2 == 0].
		% [2,4]
		4> [io:format("~p~n",[Element]) || Element <- [one, two, three]].
		% one
		% two
		% three
		% [ok,ok,ok]

	% Note how, in shell command 4, we are using list comprehensions to
	% create side effects. The expression still returns a list [ok, ok, ok]
	% containing the return values of executing the io:format/2 expression on
	% the elements.

	% The next set of examples show the effect of multiple generators and
	% interleaved generators and tests. In the first, for each value of X,
	% the values bound to Y run through 3, 4, and 5. In the second example,
	% the values of Y depend on the value chosen for x (showing that the
	% expression evaluates X before Y). The remaing two examples apply tests
	% to both of the bound variables:

		5> [ {X,Y} || X <- [1,2], Y <- [3,4,5] ].
		% [{1,3},{1,4},{1,5},{2,3},{2,4},{2,5}]
		6> [ {X,Y} || X <- [1,2], Y <- [X+3,X+4,X+5] ].
		% [{1,4},{1,5},{1,6},{2,5},{2,6},{2,7}]
		7> [ {X,Y} || X <- [1,2,3], X rem 2 /= 0, Y <- [X+3,X+4,X+5], (X+Y) rem 2 == 0 ].
		% [{1,5},{3,7}]
		8> [ {X,Y} || X <-[1,2,3], X rem 2 /= 0, Y <- [X+3,X+4,X+5], (X+Y) rem 2 /= 0 ].
		% [{1,4},{1,6},{3,6},{3,8}]
