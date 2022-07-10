% Tools and Libraries

	% The second building block, which came about before open source became
	% the widespread norm for software projects, includes applications that
	% ship as part of the standard Erlang/OTP distribution. You can view each
	% application as a way of packaging resources in OTP, where applications
	% include tools, libraries, interfaces toward other languages and
	% programming environments, databases and database drivers, standard
	% components, and protocol stacks. The OTP documentation does a fine
	% job of separating them into the following subsets:

		% The basic applications include the following:

				% The Erlang Runtime System (erts)

				% The kernel

				% The standard libraries (stdlib)

				% The system architecture support libraries (sasl)
			
			% They provide the tools and basic building blocks needed to
			% architect, create, start, and upgrade your system. We cover the
			% basic applications in detail throughout this book. Together with
			% the compiler, these are the minimal subset of applications
			% necesssary in any system written in Erlang/OTP to do anything
			% meaningful.
% -------------------------------------------------------------------------
		% The database applications incldue mnesia, Erlang's distributed
		% database, and odbc, an interface used to communicate with relational
		% SQL databases. Mnesia is a popular choice because it is fast, runs
		% and stores its data in the same memory space as your applications,
		% and is easy to use, as it is accessed through an Erlang API.
% -------------------------------------------------------------------------
		% The operations and maintenance applications include os_mon, an
		% application that allows you to monitor the underlying operating
		% system; snmp, a Simple Network Management Protocol agent and client;
		% and otp_mibs, management information bases that allow you to manage
		% Erlang systems using SNMP.
% -------------------------------------------------------------------------
		% The collection of interface and communication applications provide
		% protocol stacks and interfaces to work with other programming
		% langauges, including an ASN.1 (asn1) compiler and runtime support,
		% direct hooks into C (ei and erl_interface) and Java (jinterface)
		% programs, along with an XML parser (xmerl). 
% -------------------------------------------------------------------------
		% There are security applications for SSL/TLS, SSH, cryptography, and
		% public key infrastructure. Graphics packages include a port of
		% wxWidgets (wx), together with an easy-to-use interface. The eldap
		% application provides a client interface toward the Lightweight
		% Directory Access Protocol (LDAP). 
% -------------------------------------------------------------------------
		% And for telecom aficionados, there
		% is a Diameter stack (as defined in RFC 6733), used for policy control
		% and authorization, alongside authentication and accounting. Dig even
		% deeper and you will find the Megaco stack. Megaco/H.248 is a protocol
		% for controlling elements of a physically decomposed multimedia
		% gateway, separating the media conversion from the call control. If
		% you ever used a smartphone, you have very likely indirectly taken the
		% Erlang diameter and megaco applications for a spin.
% -------------------------------------------------------------------------
		% The collection of tools applications facilitate the development,
		% deployment, and management of your Erlang system. We cover only the
		% most relevant ones in this book, but outline them all here so you are
		% aware of their existence:

			% The debugger is a graphical tool that allows you ot step through
			% your code while influencing the state of the functions.

			% The observer integrates the application monitor and the process
			% manager, alongside basic tools to monitor your Erlang systems as
			% they are being developed and in production.

			% The dialyzer is a static analysis tool that finds type
			% discrepencies, dead code, and other issues.

			% The event tracer (et) uses ports to collect trace events in
			% distributed environments, and percept allows you to locate
			% bottlenecks in your system by tracing and visualizing
			% concurrency-related activities.

			% Erlang Syntax Tools (syntax_tools) contains modules for handling
			% Erlang syntax trees in a way that is compatible with other
			% language-related tools. It also includes a module merger allowing
			% you to merge Erlang modules, together with a renamer, solving the
			% issue of clashes in a nonhierarchical module space.

			% The parsetools application contains the parse generator (yecc) and
			% a lexical analyzer generator for Erlang (leex).

			% Reltool is a release management tool that provides a graphical
			% front end together with back-end hooks that can be used by more
			% generic build systems.

			% Runtime_tools is a collection of utilities including DTrace and
			% SystemTap probes, and dbgm, a user-friendly wrapper around the
			% trace built-in functions (BIFs).

			% Finally, the tools application is a collection of profilers, code
			% coverage tools, and module cross-reference analysis tools, as well
			% as the Erlang mode for the emacs editor.

% -------------------------------------------------------------------------

		% The test applications provide tools for unit testing (eunit), system
		% testing, and black-box testing. The Test Server (packaged in the
		% test_server application) is a framework that can be used as the
		% engine fo a higher-level test tool application. Chances are that you
		% will not be using it, because OTP provides one of these higher-level
		% test tools in the form of common_test, an application suited for
		% black-box testing. Common_test supports automated execution of
		% Erlang-based test cases toward most target systems irrespective of
		% programming language.

% -------------------------------------------------------------------------

		% We need to mention the Object Request Brokers (ORBs) and interface
		% definition language (IDL) applications for nostalgic reasons,
		% reminding one of the coauthors of his past sins. They include a
		% broker called orber, and IDL compiler called ic, and a few other
		% CORBA Common Object Services no longer used by anyone.


