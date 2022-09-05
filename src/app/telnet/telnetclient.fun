(*

	 FoxNet: The Fox Project's Communication Protocol Implementation Effort
	 Sidd Puri (puri@cs.cmu.edu)
	 Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	 Ken Cline (Kenneth.Cline@cs.cmu.edu)
	 Fox Project
	 School of Computer Science
	 Carnegie Mellon University
	 Pittsburgh, Pa 15213-3891

		i.	Abstract

        telnetclient.fun: telnet client program based on the telnet functor.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Telnet_Client
	2.	internal values stdin, stdout
	3.	internal function member
	4.	internal function setup_termios
	5.	internal function put_stdout
	6.	internal function send_window_size
	7.	datatype message
	8.	internal function connection_handler
	9.	internal function send_terminal_type
	10.	internal function send_terminal_speed
	11.	internal function send_x_display
	12.	internal function send_x_display
	13.	internal function handle_value
	14.	internal function data_handler
	15.	internal function status_handler
	16.	function launch
	17.	function telnet

		iii.	RCS Log
	
$Log: telnetclient.fun,v $
Revision 1.1  1996/07/05  17:45:50  esb
Initial revision


		1.	functor Telnet_Client
*)

functor Telnet_Client (structure Term: TERMINAL
		       structure Pty: TELNET_PTY
		       structure Telnet: TELNET
		       val setup: Telnet.Lower.Setup.T
		       val lookup: string * int -> Telnet.Address.T
		       val suspend_character: char (* normally #"\029", ^] *)
                       val connection_closing: Telnet.Lower.Status.T -> bool
		       structure B: FOX_BASIS
		       val debug_level: int ref option) =
 struct

  local

   structure Option = Telnet.Option

   structure TTY = Posix.TTY

   structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "telnetclient.fun"
			   val makestring = fn _ => NONE)

(*
		2.	internal values stdin, stdout
*)

   val stdin  = Posix.FileSys.stdin
   val stdout = Posix.FileSys.stdout

   val buffer_size = 10000

(*
		3.	internal function member
*)

   fun member (_, []) = false
     | member (item, first :: rest) = item = first orelse member (item, rest)

(*
		4.	internal function setup_termios

	The values for the flags are taken from /usr/include/termio.h.
*)

   val onlcr_flag = Term.O (TTY.O.wordTo 0wx2)

   fun setup_termios () =
        (Term.clear (stdin, Term.L TTY.L.icanon); (* cbreak mode *)
         Term.clear (stdin, Term.I TTY.I.icrnl); (* do not convert CR to NL *)
         Term.clear (stdin, Term.L TTY.L.isig); (* disable INTR, QUIT, SUSP *)
         Term.clear (stdin, onlcr_flag); (* disable mapping NL to CR-NL *)
	 Term.setcc (stdin, [(TTY.V.min, chr 1), (TTY.V.time, chr 0)]))

(*
		5.	internal function put_stdout

	Convert \010 to \011 since that insures that a true line feed
	(as opposed to a CR-LF) is executed; quoting RFC 854, p. 10:

		Moves the printer to the next print line, keeping
		the same horizontal position.
*)

   fun put_stdout str =
        (Posix.IO.writeVec (stdout, {buf = str, i = 0, sz = NONE});
	 ())

(*
		8	internal value window_change_signal
*)

   exception Window_Change_Signal_Not_Found

   val window_change_signal = case Signals.fromString "WINCH" of
                                 SOME s => s
			       | NONE => raise Window_Change_Signal_Not_Found

(*
		6.	internal function send_window_size
*)

   fun send_window_size send =
        let val (rows, cols) = Pty.get_winsize stdout
        in Trace.debug_print (fn _ =>
			     "[Sending window size (" ^ makestring rows ^
			     ", " ^ makestring cols ^ ")]\n");
	  send (Option.Window_Size_Is {columns = cols, rows = rows})
        end

(*
		7.	datatype message
*)

   datatype message = Window_Change | Suppress_Output | Interrupt | Done

(*
		8.	internal function connection_handler
*)

   fun connection_handler message_pipe (Telnet.C {send, abort, extension}) =
        let val Telnet.TCE {flush, send_value, ...} = extension
	    fun window_change_handler (s, n, k) =
                 (B.Pipe.enqueue (message_pipe, Window_Change);
		  k)
            val new_handler = Signals.HANDLER window_change_handler
	    val old_handlers = [(window_change_signal,
				 Signals.setHandler (window_change_signal,
						     new_handler))]
	    fun has_suspend [] = false
	      | has_suspend (first :: rest) =
	         if first = suspend_character then true else has_suspend rest
	    fun forwarding_loop () =
	         if Pty.select (stdin, 0) then
		  let val input = Posix.IO.readVec (stdin, buffer_size)
		  in if has_suspend (explode input) then
		      let val termios = TTY.tcgetattr stdin
		          val process = Posix.ProcEnv.getpid ()
		      in Trace.local_print "Suspending telnet";
		         Posix.Process.kill (Posix.Process.K_PROC process,
					     Posix.Signal.tstp);
			 TTY.tcsetattr (stdin, TTY.TC.sanow, termios);
			 forwarding_loop ()
		      end
		     else
		      (case ((send input; NONE) handle x => SOME x) of
			  NONE => forwarding_loop ()
			| SOME x =>
			   Trace.print_handled (x, SOME "send (telnet exit)"))
		  end
		 else
		  case B.Pipe.dequeue_timeout (message_pipe, 1) of
		     SOME Window_Change =>
		      (send_window_size send_value;
		       flush ();
		       forwarding_loop ())
		   | SOME Done =>
		      put_stdout "\nConnection closed.  Exiting.\n"
		   | SOME Suppress_Output =>
		      Trace.local_print "[suppressing output not implemented]"
		   | SOME Interrupt =>
		      Posix.Process.kill (Posix.Process.K_PROC
					   (Posix.ProcEnv.getpid ()),
					  Posix.Signal.int)
		   | NONE =>
		      forwarding_loop ()
             val original_attributes = TTY.tcgetattr stdin
	in Trace.trace_constant_string "[Got a connection]";
	   setup_termios ();
	   forwarding_loop ();
	   TTY.tcsetattr (stdin, TTY.TC.sanow, original_attributes);
	   map Signals.setHandler old_handlers;
	   ()
        end

(*
		9.	internal function send_terminal_type
*)

   fun send_terminal_type send =
        let val term = case Posix.ProcEnv.getenv "TERM" of
	                 SOME t => B.V.String.to_upper t
		       | NONE => "NETWORK-VIRTUAL-TERMINAL"
        in Trace.debug_print (fn _ => "[Sending terminal type " ^ term ^ "]");
	  send (Option.Terminal_Type_Is term)
        end

(*
		10.	internal function send_terminal_speed
*)

   fun send_terminal_speed send =
        let val send_speed = Term.get_output_speed stdin
	   val receive_speed = Term.get_input_speed stdout
	   val value = Option.Terminal_Speed_Is {send = send_speed,
						  receive = receive_speed}
        in Trace.debug_print (fn _ => "[Sending terminal speed " ^
			     makestring send_speed ^ ", " ^
			     makestring receive_speed ^
			     "]");
	   send value
        end

(*
		11.	internal function send_x_display
*)

   fun send_x_display send =
        case Posix.ProcEnv.getenv "DISPLAY" of
	   SOME string =>
	    (case B.V.String.index (":", string, 0) of
	        SOME i =>
		  let val length = B.V.String.length string
		      val host = B.V.String.substring (string, 0, i)
		      val rest = B.V.String.substring (string, i + 1,
						       length - i - 1)
		      val rlength = B.V.String.length rest
		      val (text_display, screen) =
		            case B.V.String.index (".", rest, 0) of
			       SOME j =>
			        (B.V.String.substring (rest, 0, j),
				 Int.fromString (B.V.String.substring
						 (rest, j + 1,
						  rlength - j - 1)))
			     | NONE => (rest, NONE)
		      val display =
		           case Int.fromString text_display of
			      NONE =>
			       (Trace.local_print ("display number not found: "
						   ^ string);
			        0)
			    | SOME i => i
		      val value = Option.X_Display_Is {host = host,
							display = display,
							screen = screen}
		  in Trace.debug_print (fn _ => "[Sending display " ^
					host ^ ":" ^ makestring display ^
					(case screen of
					    NONE => ""
					  | SOME s => "." ^ Int.toString s)
				        ^ "]");
		     send value
		 end
	      | NONE =>
		 Trace.local_print ("unable to parse display " ^ string))
         | NONE =>
	    Trace.local_print ("no display environment variable")

(*
		12.	internal function send_x_display
*)

   fun send_environment (send, constructor, list) =
        let fun build_single Option.All_Vars =
  (* all well-known variables: USER, JOB, ACCOUNT, PRINTER,
     SYSTEMTYPE, DISPLAY *)
	        build_single Option.User_Name @ build_single Option.Job @ 
	        build_single Option.Account @ build_single Option.Printer @ 
	        build_single Option.System_Type @ build_single Option.Display
	     | build_single Option.User_Name =
		[(Option.User_Name, Posix.ProcEnv.getenv "USER")]
	     | build_single Option.Job = []
	     | build_single Option.Account = []
	     | build_single Option.Printer =
		[(Option.Printer, Posix.ProcEnv.getenv "PRINTER")]
	     | build_single Option.System_Type = []
	     | build_single Option.Display =
		[(Option.Display, Posix.ProcEnv.getenv "DISPLAY")]
	     | build_single (Option.User string) =
		[(Option.User string, Posix.ProcEnv.getenv string)]
	    fun build_all () = build_single Option.All_Vars
	    val env = case list of
	                 [] => build_all ()
		       | _ =>
			  B.V.List.fold op@ (B.V.List.map build_single list) []
        in Trace.debug_print (fn _ => "[Sending environment " ^
			      Option.makestring_environment_pairs env ^ "]");
	   send (constructor env)
        end

(*
		13.	internal function handle_value
*)

   fun handle_value (send_value, Option.Request_Terminal_Type) =
        send_terminal_type send_value
     | handle_value (send_value, Option.Request_Terminal_Speed) =
        send_terminal_speed send_value
     | handle_value (send_value, Option.Request_X_Display) =
        ((send_x_display send_value)
	 handle x => Trace.print_handled (x, SOME "send_x_display"))
     | handle_value (send_value, Option.Request_Environment list) =
        send_environment (send_value, Option.Environment_Is, list)
     | handle_value (send_value, Option.Old_Request_Environment list) =
        send_environment (send_value, Option.Old_Environment_Is, list)
     | handle_value (send_value, Option.Wrong_Request_Environment list) =
        send_environment (send_value, Option.Wrong_Environment_Is, list)
     | handle_value (send_value, value) =
        Trace.local_print ("ignoring option value " ^
			   Option.makestring_option_value value)

(*
		14.	internal function data_handler
*)

   fun data_handler (_, string) = put_stdout string

(*
		15.	internal function status_handler
*)

   fun status_handler pipe (_, Telnet.Telnet_Status.Lower_Status status) =
        (Trace.trace_print (fn _ =>
			    "[" ^ Telnet.Lower.Status.makestring status ^ "]");
	 if connection_closing status then
	  B.Pipe.enqueue (pipe, Done)
	 else ())			(* ignore *)
     | status_handler _ (conn, Telnet.Telnet_Status.Remote_Request request) =
	let val {option, accept} = request
	in case option of
	      Option.Suppress_Go_Ahead => accept (Telnet.Telnet_Status.Yes)
	    | Option.Echo =>
	       (Term.clear (stdin, Term.L TTY.L.echo);
		accept (Telnet.Telnet_Status.Yes))
	    | _ => accept (Telnet.Telnet_Status.No)
	end
     | status_handler _ (conn, Telnet.Telnet_Status.Local_Request request) =
	let val {option, accept} = request
	    val Telnet.C {extension, ...} = conn
	    val Telnet.TCE {send_value, ...} = extension
	in case option of
	      Option.Suppress_Go_Ahead => accept (Telnet.Telnet_Status.Yes)
	    | Option.Window_Size =>
	       (accept (Telnet.Telnet_Status.Yes);
		B.Scheduler.suspend (fn s => B.Scheduler.resume (s, ()));
		send_window_size send_value)
	    | Option.Terminal_Speed => accept (Telnet.Telnet_Status.Yes)
	    | Option.Terminal_Type => accept (Telnet.Telnet_Status.Yes)
	    | Option.X_Display_Location => accept (Telnet.Telnet_Status.Yes)
	    | Option.Environment => accept (Telnet.Telnet_Status.Yes)
	    | Option.Old_Environment => accept (Telnet.Telnet_Status.Yes)
(* not accepted, but if we did accept it we would have to turn on echoes.
	    | Option.Echo =>
	       (Term.set (stdin, Term.L TTY.L.echo);
		accept (Telnet.Telnet_Status.Yes))
*)
	    | _ => accept (Telnet.Telnet_Status.No)
	end
     | status_handler _ (conn, Telnet.Telnet_Status.Remote_Response response) =
	let val {option, now_valid} = response
	in case option of
	      Option.Echo => Term.clear (stdin, Term.L TTY.L.echo)
	    | _ => ()
	end
     | status_handler _ (conn, Telnet.Telnet_Status.Local_Response response) =
	let val {option, now_valid} = response
	in case option of
	      Option.Echo => Term.set (stdin, Term.L TTY.L.echo)
	    | _ => ()
	end
     | status_handler _ (conn, Telnet.Telnet_Status.Remote_Disable option) =
	(case option of
	    Option.Echo => Term.set (stdin, Term.L TTY.L.echo)
	  | _ => ())
     | status_handler _ (conn, Telnet.Telnet_Status.Local_Disable option) =
	(case option of
	    Option.Echo => Term.clear (stdin, Term.L TTY.L.echo)
	  | _ => ())
     | status_handler _ (conn, Telnet.Telnet_Status.Value value) =
	let val Telnet.C {extension, ...} = conn
	    val Telnet.TCE {send_value, ...} = extension
	in handle_value (send_value, value)
	end
     | status_handler _ (_, Telnet.Telnet_Status.Synch) = ()
     | status_handler pipe (_, Telnet.Telnet_Status.Interrupt_Process) =
	B.Pipe.enqueue (pipe, Interrupt)
     | status_handler pipe (_, Telnet.Telnet_Status.Abort_Output) =
	B.Pipe.enqueue (pipe, Suppress_Output)
     | status_handler _ (_, Telnet.Telnet_Status.Go_Ahead) = ()
     | status_handler _ (_, Telnet.Telnet_Status.Erase_Char) =
	put_stdout ("\008 \008")	(* erase one char position *)
     | status_handler _ (_, Telnet.Telnet_Status.Erase_Line) =
        Trace.local_print "Telnet Erase-Line not handled"

(*
		16.	function launch
*)

  in
   fun launch (remote_host, port) =
        let fun handler_fun key =
	         let val message_pipe: message B.Pipe.T = B.Pipe.new ()
	         in {connection_handler = connection_handler message_pipe,
		     data_handler = data_handler,
		     status_handler = status_handler message_pipe}
		 end
	    val peer = lookup (remote_host, port)
	    val rlocal = Telnet.Telnet_Setup.Request_Local
	    val remote = Telnet.Telnet_Setup.Request_Remote
	    val start_options = [rlocal Telnet.Option.Suppress_Go_Ahead,
				 remote Telnet.Option.Suppress_Go_Ahead,
				 rlocal Telnet.Option.Window_Size,
				 rlocal Telnet.Option.Terminal_Speed,
				 rlocal Telnet.Option.Terminal_Type,
				 remote Telnet.Option.Echo,
				 rlocal Telnet.Option.X_Display_Location,
				 rlocal Telnet.Option.Environment]
	    fun session_fun (Telnet.S {connect, ... }) =
	         connect (peer, Telnet.H handler_fun)
         in Telnet.session ((start_options, setup), session_fun)
         end

(*
		17.	function telnet
*)

   fun telnet remote_host = launch (remote_host, 23)

  end

end (* TelnetClient *)

