% Processes and Message Passing

	% Concurrency is at the heart of the Erlang programming model. Processes
	% are lightweight, meaning that creating them invovles negligible time
	% and memory overhead. Processes do not share memory, and instead
	% communicate with each other through message passing. Messages are
	% copied from the stack of the sending process to the heap of the
	% receiving one. As processes execute concurrently in separate memory
	% spaces, these memory spaces can be garbage collected separately, giving
	% Erlang programs very predictable soft real-time properties, even under
	% sustained heavy loads. Millions of processes can run concurrently
	% within the same VM, each handling a standalone task. Processes fail
	% when exceptions occur, but because there is no shared memory, failure
	% can often be isolated as the process were working on standalone tasks.
	% this allows other processes working on unrelated or unaffected tasks to
	% continue executing and the program as a whole to recover on its own.

	% So, how does it all work? Processes are created via the spawn(Mod,
	% Func, Args) BIF or one of its variants. The result of a spawn call is a
	% process identifier, normally referred to as a pid. Pids are used for
	% sending messages, and can themselves be part of the message, allowing
	% other processes to communicate back.

	% The following example of an "echo" process shows these basics. The
	% first action of the go/0 function is to spawn a process executing
	% loop/0, after which it communicates with that process by sending and
	% receiving messages. The loop/0 function receives messages and,
	% depending on their format, either replies to them (and loops) or
	% terminates. To get this looping behavior, the function is tail
	% recursive, ensuring it executes in constant memory space.

	% We know the pid of the process executing loop/0 from the spawn, but
	% when we send it a message, how can it communicate back to us? We'll
	% have to send it our pid, which we find using the self() BIF:

		-module(echo).
		-export([go/0, loop/0]).

		go() ->
			Pid = spawn(echo, loop, []),
			Pid ! {self(), hello},

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

	% In this echo example, the go/0 function first spawns a new process
	% executing the echo:loop/0 function, storing the resulting pid in the
	% variable Pid. It then sends the to the Pid process a message containing
	% the pid of the sender, retrieved using the self() BIF, along with the
	% atom hello. After that, go/0 waits to receive a message in the form of
	% a pair whose first element matches the pid of the loop process; when
	% such a message arrives, go/0 prints out the second element of the
	% message, exits the receive expression, and finishes by sending the
	% message stop to Pid.

	% The echo:loop/0 function first waits for a message. If it receives a
	% pair containing a pid From and a message, it sends a message containing
	% its own pid along with the received Msg back to the From process and
	% then calls itself recursively. If it instead receives the atom stop,
	% loop/0 returns ok. When loop/0 stops, the process that go/0 originally
	% spawned to run loop/0 terminate as well, as there is no more code to
	% execute.

	% Note how, when we run this program, the go/0 call returns stop. Every
	% function returns a value, that of the last expression it evaluated.
	% Here, the last expression is Pid ! go, which returns the message we
	% just sent to Pid:

		1> c(echo).
		% {ok, echo}
		2> echo:go().
		% hello
		% stop

% -------------------------------------------------------------------------

% BOUND VARIABLES IN PATTERNS

	% Pattern matching is different in Erlang than in other languages with
	% pattern matching because variables occurring in patterns can be already
	% bound. In the go function in the echo example, the variable Pid is
	% already bound to the pid of the process just spawned, so the receive
	% expression will accept only those messages in which the first component
	% is that particular pid; in the scenario here, it will be a message from
	% that pid, in fact.

	% If a message is received with a different component, then the pattern
	% match in the receive will not be successful, and the receive will block
	% until a message is received from process Pid.

	% ---

	% Erlang message passing is asynchronous: the expression that sends a
	% message to a process returns immediately and always appears to be
	% successful, even when the receiving process doesn't exist. If the
	% process exists, the messages are placed in the mailbox of the receiving
	% process in the same order in which they are received. they are
	% porcessed using the receive expression, which pattern matches on the
	% messages in sequential order. Message reception is selective, meaning
	% that messages are not necessarily processed in the order in which they
	% arrive, but rather the order in which they are matched. Each receive
	% clause selects the message it wants to read from the mailbox using
	% pattern matching.
	
	% Suppose that the mailbox for the loop process has received the messages
	% foo, stop, and {Pid, hello} in that order. The receive expression will
	% try to match the first messsage (here, foo) against each other of the
	% patterns in turn; this fails, leaving the message in the mailbox. It
	% then tries to do the same with the second, with the result that the
	% process terminates, as there is no more code to execute.

	% These semantics mean that we can process messages in whatever order we
	% choose, irrespective of when they arrive. Code like this:

		receive
			message1 -> %...
		end
		receive
			message2 -> %...
		end

	% Will process the atoms message1 and then message2. Without this
	% feature, we'd have to aniticipate all the different orders in which
	% messages can arrive and handle each of those, greatly increasing the
	% complexity of our programs. With selective receive, all we do is leave
	% them in the mailbox for later retrieval.

% -------------------------------------------------------------------------

% MULTICORE, SCHEDULERS, AND REDUCTIONS

	% The biggest challenges in scaling systems on multicore architectures
	% are sequential code and serialization of operations. These could be in
	% your program, in libraries you use, in the underlying virtual machine,
	% or all of the above. Memory lock contention is often the major
	% bottleneck, caused when threas try to acquire a lock allowing theme to
	% access and manipulate shared memory. Erlang processes do not share
	% memory, removing one of the major obstacles and making it the ideal
	% language to fully utilize many-core computers. Program in Erlang as you
	% would have done on a single-core architecture, ensuring you have a
	% process for each truly concurrent activity, and your system will scale
	% as you add more cores. You will be limited only by your sequential code
	% and bottlenecks in the BEAM virtual machine --- bottlenecks that
	% release after release, are continually optimized or removed.

	% For every core, the BEAM virtual machine starts a thread that runs a
	% scheduler. Each scheduler is responsible for a group of processes, and
	% at any one time, a process from each scheduler executes in parallel on
	% each core. Processes that are not suspended and are ready to execute
	% are placed in the scheduler's run queue. The virtual machine also
	% starts a separate thread pool used for drivers and file I/O that can
	% operate without blocking any scheduler threads. At startup, you can
	% limit the number of threads and schedulersm and specify whether you
	% want schedulers to be bound to a core or be allowed to migrate from one
	% core to another. Schedulers are not bound to cores by default because
	% such binding can backfire, slowing down the system on certain
	% architectures. However, it can result in speedups in other situations.
	% Benchmark your system with both approaches. We cover how to set startup
	% flags and parameters in "Arguments and Flags" and benchmarking in Ch
	% 15.

	% If the system is running under a full load, the schedulers try to
	% guarantee soft real-time properties by retaining an even balance of CPU
	% time across all processes. What the BEAM virtual machine tries to do is
	% avoid cases where processes in a run queue with 10 processes get twice
	% as many CPU time as those in a run queue with 20 processes. This is
	% achieved by allowed processes to migrate between run queues, evening
	% out their sizes across the schedulers. But if the system isn't fully
	% loaded, the virtual machine migrates processes so they occupy fewer
	% cores, and then pauses the unused scheduler threads. This allows cores
	% to be shut down and put in energy saving mode, and later awakened when
	% the load of the virtual machine increases.

	% Schedulers decide when to preempt processes based on approximation of
	% the workload they have executed. This approximation is called the
	% reduction count. When a process is preempted, it stops running and is
	% placed at the end of the run queue, allowing the process first in line
	% to execute. Function calls and BIFs are assigned a value of one or more
	% reductions, with the theory that expensive calls have a higher
	% reduction count than cheaper ones. Each process is allowed to execute a
	% predefined number of reductions before being preempted, allowing the
	% process at the head of the run queue to execute. The number of
	% reductions each process is allowed to execute before being suspended
	% and the reduction count of each instruction are purposely not
	% documented to discourage premature optimization, because the redection
	% count and the total number of reductions the scheduler allows a process
	% to execute may change from one release and hardware architecture to
	% another.

	% Scheduler balance, reductions, and the per-process garbage collector
	% give the BEAM virtual machine predictable, soft real-time properties,
	% even during times of peak and extended load, by maximizing fairness and
	% ensuring there is no process starvation. Other programming languages
	% and frameworks not running on BEAM don't provide preemptive
	% multitasking. Application activities are not allowed to block,
	% preventing the event loop from running frequently and dispatching
	% events to their intended targets. If an application blocks, it blocks
	% every part of the application, whereas in Erlang, the only way to block
	% a scheduler (and all the processes in its run queue) is to drop into C
	% code and either ignorantly or purposefully implement a misbehaving
	% native implemented function (NIF) or driver. Lack of preemptive
	% multitasking will therefore affect the soft real-time properties of a
	% system, as it will either rely on the process to cooperatively preempt
	% itself, or base preemption on specific operations instead of the number
	% and cost of the operations themselves. Having said this, don't even get
	% us started with "stop the world" garbage collectors in shared memory
	% architectures, which force all threads to synchronize in order to
	% determine which objects are still being use and which ones can be
	% freed. No one named, no one shamed.

% -------------------------------------------------------------------------