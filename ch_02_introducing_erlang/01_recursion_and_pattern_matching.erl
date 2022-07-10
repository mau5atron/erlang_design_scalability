% Recursion and Pattern Matching

	% Recursion is the way Erlang programmers get iterative or repetitive
	% behavior in their programs. It is also what keeps processes alive in
	% between bursts of activity.

	% This first example shows how to compute the factorial of a postive
	% number:

-module(ex1).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) when N > 0 ->
	N * factorial(N-1).

% We call the function factorial and indicate that it takes a single
% argument (factorial/1). The trailing /1 is the arity of the function,
% referring to the number of arguments the function takes.

% If the argument we pass to the function is the integer 0, we match the
% first clause, returning 1. Any integer greater than 0 is bound to the
% variable N, returning the product of N and factorial(N-1). The iteration
% will continue until we pattern match on the function clause the serves as
% the base case. The base case is the clause where recursion stops. If we
% call factorial/1 with a negative integer, the call fails as no clauses
% match. But we don't bother dealing with the problems caused by a caller
% passing a noninteger argument; this is an Erlang principle we discuss
% later.

% -------------------------------------------------------------------------

% Recursion is not just for computing simple values; we can write
% imperitive programs using the same style. The following is a program to
% print every element of a list, separate by tabs. As with the preious
% example, the function is presented in two clauses, where each clause has
% a head and a body, separated by the arrow (->). In the head we see the
% function applied to a pattern, and when a function is applied to an
% argument, the first clause whose pattern matches the argument is used. In
% this example the [] matches an empty list, whereas [X|Xs] matches a
% nonempty list. The [X|Xs] syntax assigns the first element of the list,
% or head, to x and the remainder of the list, or tail, to Xs (if you have
% not yet noted it, Erlang variables such as X, Xs, and N all start with
% uppercase letters).

-module(ex2).
-export([print_all/1]).

print_all([]) -> io:format("~n");
print_all([X|Xs]) ->
	io:format("~p\t", [X]),
	print_all(Xs).

% The effect is to print each item from the list, in the order that it
% appears in the list, with a tab (\t) after each item. Thanks to the base
% case, which runs when the list is empty (when it matches []), a newline 
% (~n) is printed at the end. Unlike in the ex1:factorial/1 example shown
% earlier, the pattern of recursion in this example is tail recursive. It
% is used in Erlang programs to give looping behavior. A function is said
% to be tail recursive if the only recursive calls to the function occur as
% the last expression to be executed in the function clause. We can think
% of this final call as a "jump" back to the start of the function, now
% called with a different parameter. Tail-recursive functions allow
% last-call optimization, ensuring stack frames are not added in each
% iteration. This allows functions to execute in constant memory space and
% removes the risk of a stack overflow, which in Erlang manifests itself
% through the virtual machine running out of memory.

% If you come from an imperitive programming background, writing the
% function slightly differently to use a case expression rather than
% separate clauses may make tail recursion easier to understand:

all_print(Ys) ->
	case Ys of
		[] ->
			io:format("~n");
		[X|Xs] ->
			io:format("~p\t", [X]),
			all_print(Xs)
	end.

% When you test either of these print functions, note the ok printed out
% after the newline. Every Erlang function has to return a value. This
% value is whatever the last executed expression returns. In our case, the
% last executed expression is io:format("~n"). The newline appears as a
% side effect of the function, while the ok is the return value printed by
% the shell.

% -------------------------------------------------------------------------

% The arguments in our exmple play the role of mutable variables, whose
% values change between calls. Erlang variables are single assignment, so
% once you've bound a value to a variable, you can no longer change that
% variable. In a recursive function variables of the same name, including
% function arguments, are considered fresh in every function iteration. We
% can see the behavior of single assignment of variables here:

1> A = 3.
% 3
2> A = 2 + 1.
% 3
3> A = 3 + 1.
% ** exception error: no match of right hand side value 4

% In shell command 1 we assign an unbound variable. In shell command 2 we
% pattern match an assigned variable to its value. Pattern matching fails
% in shell command 3 because the value of the right hand side differs from
% the current value of A.

% -------------------------------------------------------------------------

% Erlang also allows pattern matching over binary data, where we match on a
% bit level. This is incredibly powerful and efficient construct for
% decoding frames and dealing with network protocol stacks. How about
% decoding an IPv4 packet in a few lines of code?

-define(IP_VERSION, 4).
-define(IP_MIN_HDR_LEN, 5).

handle(Dgram) ->
	DgramSize = byte_size(Dgram),
	<<?IP_VERSION:4, HLen:4, SrvcType:8, TotLen:16, ID:16, ...,
	Flgs:3, FragOff:13, TTL:8, Proto:8, HdrChkSum:16, ...,
	SrcIP:32, DestIP:32, Body/binary>> = Dgram,
	if
		(HLen >= 5) and (4*HLen =< DgramSize) ->
			OptsLen = 4 * (HLen - ?IP_MIN_HDR_LEN),
			<<Opts:OptsLen/binary, Data/binary>> = Body,
			...
	end.


% We first determine the size (number of bytes) of Dgram, a variable
% holding an IPv4 packet as binary data previously received from a network
% socket. Next, we use pattern matching against Dgram to extract its
% fields; the left-hand side of the pattern matching assignment defines an
% Erlang binary, delimited by << and >> and containing a number of fields.

% The numbers following most of the fields specify the number of bits (or
% bytes for binaries) each field occupies. For example, Flgs:3 defaults to
% an integer that matches 3 bits, the value of which it binds to the
% variable Flgs. At the point of the pattern match we don't yet know the
% size of the final field, Body, so we specify it as a binary of unknown
% length in bytes that we bind to the variable Data. If the pattern match
% succeeds, it extracts, in just a single statement, all the named fields
% from the Dgram packet. Finally, we check the value of the extracted Hlen
% field in an if clause, and if it succeeds, we perform a pattern matching
% assignment against Body to extract Opts as a binary of OptsLen bytes and
% Data as a binary consisting of all the rest of the data in Body. Note how
% OptsLen is calculated dynamically. If you've ever written code using an
% imperative language such as Java or C to extract fields from a network
% packet, you can see how much easier pattern matching makes the task.