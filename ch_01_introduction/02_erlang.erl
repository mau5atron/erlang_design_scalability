% Erlang

	% The first building block is Erlang itself, which includes the semantics
	% of the language and its underlying virtual machine. Key language
	% features such as lightweight processes, lack of shared memory, and
	% asynchronous message passing will bring you a step closed to your goal.
	% Just as important are links and monitors between processes, and
	% dedicated channels for the propagation of the error signals. The
	% monitors and error reporting allow you to build, with relative ease,
	% complex supervision hierarchies with built-in fault recovery. Because
	% message passing and error propagation are asynchronous, the semantics
	% and logic of a system that was developed to run in a single Erlang node
	% can be easily distributed without having to change any of the code
	% base.