% Links and Monitors for Supervision

	% A typical Erlang system has lots of (possibly dependent) processes
	% running at the same time. How do these dependencies work with the "let
	% it fail" philosophy. Suppose process A interacts with process B and C;
	% these processes are dependent on each other, so if A fails, they can no
	% longer function properly. A's failure needs to be detected, after which
	% B and C need to be terminated before A's failure needs to be detected,
	% after which B and C need to be terminated before restarting them all.
	% In this section we describe the mechanisms that support this approach,
	% namely process linking, exit signals, and monitoring. These simple
	% constructs enable us to build libraries with complex supervision
	% strategies, allowing us to manage processes that my be subjected to
	% failure at any time.

% -------------------------------------------------------------------------

% Links

	% Calling link(Pid) in a process A creates a bidirectional link between
	% process A and Pid. calling spawn_link/3 has the same effect as calling
	% spawn/3 followed by link/1, except that it is executed atomically,
	% eliminating the race condition where a process terminates between the
	% spawn and the link. A link from the calling process to Pid is removed
	% by calling unlink(Pid).

	% The key insight here is that the mechanism needs to be orthogonal to
	% Erlang message passing, but effectuated with it. If two Erlang
	% processes are linked, when either of them terminates, an exit signal is
	% sent to the other, which will then itself termiante. The terminated
	% process will in turn send the exit signal to all the processes in its
	% linked set, propagating it through the system. The power of the
	% mechanism comes from the ways that this default behavior can be
	% modified, giving the designer fine control over the termination of the
	% processes within a system. We now look at this in more detail.

	% One pattern for using links is as follows: a server that controls
	% access to resources links to a client while that client has access to a
	% particular resource. If the client terminates, the server will be
	% informed so it can reallocate the resource (or just terminate). If, on
	% the other hand, the client hands back the resource, the server may
	% unlink from the client.

	% Remember, though, that the links are bidirectional, so if the server
	% dies for some reason while the client and server are linked, this will
	% by default kill the client too, which you may not want to happen. If
	% that's the case, use a monitor instead of a link, as we explain in
	% "Monitors"

	% Exit signals can be trapped by calling the process_flag(trap_exit,
	% true) function. This converts exit signals into messages of the form 
	% {'EXIT', Pid, Reason}, where Pid is the process identifier of the
	% process that has died and Reason is the reason it has terminated. These
	% messages are stored in the recipient's mailbox and processed in the
	% same way as all other messages. When a process is trapping exits, the
	% exit signal is not propagated to any of the processes in its link set.

	% Why does a process exit? This can happen for two reasons. If a process
	% has no more code to execute, it terminates normally. The Reason
	% propagated will be the atom normal. Abnormal termination is initiated
	% in case of a runtime error, receiving an exit signal when not trapping
	% exits, or by calling the exit BIFs. Called with a single argument, exit
	% (Reason) will terminate the calling process with reason Reason, which
	% will be propagated in the exit signal to any other processes to which
	% the exiting one is linked. When the exit BIF is called with two
	% arguments, exit(Pid, Reason), it sends an exit signal with reason
	% Reason to the process Pid. This will have the same effect as if the
	% calling process had terminated with reason Reason.

	% As we said at the start of this section, users can control the way in
	% which termination is propagated through a system. The options are
	% summarized in Table 2-1 and vary depending on if the trap_exit process
	% flag is set.

		% Propagation Semantics

			% Reason
				% normal
			% Trapping exits
				% Receives {'EXIT', Pid, normal}
			% Not trapping exits
				% Nothing happens

			% --------------------------------

			% Reason
				% kill
			% Trapping exits
				% Terminates with reason killed
			% Not trapping exits
				% Terminates with reason killed

			% --------------------------------

			% Reason
				% other
			% Trapping exits
				% Receives {'EXIT', Pid, Other}
			% Not trapping exits
				% Terminates with reaeson Other

	% As the second column of the table shows, a process that is trapping
	% exits will receive an 'EXIT' message when a linked process terminates,
	% whether the termination is normal or abnormal. The kill reason allows
	% one process to force another to exit along with it. This means that
	% there's a mechanism for killing any process, even those that trap
	% exits; note that its reason for termination is killed and not kill,
	% ensuring that the unconditional termination does not itself propagate.
	% If a process is not trapping exits, nothing happens if a process in its
	% link set terminates abnormally. Abnormal termination, however, results
	% in the process terminating.

% -------------------------------------------------------------------------

% Monitors

	% Monitors provide an alternative, unidirectional mechanism for processes
	% to observe the termination of other processes. Monitors differ from
	% links in the followin ways:

		% A monitor is set up when process A calls erlang:monitor(process, B),
		% where the atom process indicates we're monitoring a process and B is
		% specified by a pid or registered name. This causes A to monitor B.

		% Monitors have an identity given by an Erlang reference, which is a
		% unique value returned by the call to erlang:monitor/2. Multiple
		% monitors of B by A can be set up, each identified by a different
		% reference.

		% A monitor is unidirectional rather than bidirectional: if process A
		% monitors process B, this does not mean that B monitors A.

		% When a monitored process terminates, a message of the form {'DOWN',
		% Reference, process, Pid, Reason} is sent to the monitoring process.
		% This contains not only the Pid and Reason for the termination, but
		% also the Reference of the monitor and the atom process.