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

	A Telnet server based on the Telnet functor, to be used in
	place of telnetd.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Telnet_Server
	2.	constants
	3.	internal function make_arg_list
	4.	internal function spawn
	5.	internal type status
	6.	internal function value_handler
	7.	internal function status_handler
	8.	internal function conn_handler
	9.	requested options
	10.	values standard_telnet_port, inetd_handler, inetd_pair
	11.	function daemon

		iii.	RCS Log
	
$Log: telnetserver.fun,v $
Revision 1.2  1996/09/17  16:00:16  cline
ignore close bug in OSF1 sml

Revision 1.1  1996/07/05  17:46:20  esb
Initial revision


		1.	functor Telnet_Server
*)

functor Telnet_Server (structure Term: TERMINAL
		       structure Pty: TELNET_PTY
		       structure Telnet: TELNET
	 (* e.g. at CMU, login_path = "/usr/local/bin/login" *)
		       val login_path: string
	 (* background_mode means never exit, even if you get an input *)
		       val background_mode: bool
		       val setup: Telnet.Lower.Setup.T
		       val lower_pattern: Telnet.Pattern.T
                       val connection_closing: Telnet.Lower.Status.T -> bool
		       structure B: FOX_BASIS
		       val debug_level: int ref option) =
 struct

  local
   structure Option = Telnet.Option

   structure TTY = Posix.TTY

   exception Telnet_Server_Implementation_Error

   structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "telnetserver.fun"
			   val makestring = fn _ => NONE)

(*
		2.	constants
*)

   local
    val iflag = TTY.I.flags [TTY.I.brkint, TTY.I.icrnl, TTY.I.ixon]
    val oflag = TTY.O.flags [TTY.O.opost]
    val cflag = TTY.C.flags [TTY.C.cread, TTY.C.cs8, TTY.C.hupcl]
    val lflag = TTY.L.flags [TTY.L.isig]
    val cc = 
         TTY.V.cc [(TTY.V.eof, #"\^D"), (TTY.V.erase, #"\^H"),
		   (TTY.V.intr, #"\^C"), (TTY.V.kill, #"\^U"),
		   (TTY.V.quit, #"\028"), (* "\^\" *)
		   (TTY.V.susp, #"\^Z"), (TTY.V.start, #"\^Q"),
		   (TTY.V.stop, #"\^S"),
		   (TTY.V.time, B.V.Char.chr 0), (TTY.V.min, B.V.Char.chr 1)]

   in
    val init_termios =
          TTY.termios {iflag = iflag, oflag = oflag,
		       cflag = cflag, lflag = lflag, cc = cc,
		       ispeed = TTY.b9600, ospeed = TTY.b9600}
   end

   val wait_opts = 1
   val buffer_size = 10000
  (* The value for onlcr is taken from /usr/include/termio.h. *)
   val onlcr_flag = Term.O (TTY.O.wordTo 0wx00000002)
   val init_winsize = (25, 80)

(*
		3.	internal function make_arg_list

	Build the Unix argument list for the login program, including
	the user name if it's part of the environment.
*)

   fun make_arg_list [] = [login_path]
     | make_arg_list (first :: rest) =
        if B.V.String.caseless_equal (B.V.String.substring (first, 0, 5),
				      "user=") then
	 let val length = B.V.String.length first - 5
	     val user = B.V.String.substring (first, 5, length)
	 in if user <> "" then [login_path, user]
	    else make_arg_list rest
	 end
	else make_arg_list rest

(*
		4.	internal function spawn

	Spawn is used to spawn the login process.
*)

   val warning = "Server Warning: Connection not encrypted! " ^
		 "Communication may be eavesdropped.\n"

   fun spawn (environment, master, slave) =
        (Posix.IO.writeVec (slave, {buf = warning , i = 0, sz = NONE}); 
         case Posix.Process.fork () of
            SOME pid => pid		(* parent *)
	  | NONE =>			(* child *)
	     (Posix.IO.close master;
	      Posix.IO.dup2 {old = slave, new = Posix.FileSys.stdin};
	      Posix.IO.dup2 {old = slave, new = Posix.FileSys.stdout};
	      Posix.IO.dup2 {old = slave, new = Posix.FileSys.stderr};
	      Pty.new_session slave;
	      Posix.IO.close slave;
	      Posix.Process.exece (login_path, make_arg_list environment,
				   environment)))

(*
		5.	internal type status
*)

   datatype status = Done | Interrupt | Kill_Output

(*
		6.	internal function value_handler
*)

   fun value_handler (Option.Window_Size_Is {columns, rows}, pipe, master) =
        Pty.set_winsize (master, (rows, columns))
     | value_handler (Option.Request_Terminal_Speed, pipe, _) = ()
     | value_handler (Option.Terminal_Speed_Is {send, receive}, pipe, _) = ()
     | value_handler (Option.Request_Terminal_Type, pipe, _) = ()
     | value_handler (Option.Terminal_Type_Is term, pipe, _) =
(* for some unexplained reason, Unix telnet upcases the terminal name, and
   Unix telnetd downcases it.  Since things won't work unless we do this,
   we go ahead and downcase the terminal name. *)
	(Trace.trace_print (fn _ => "[Setting term type to " ^
			    B.V.String.to_lower term ^ "]");
	 B.Pipe.enqueue (pipe, [(Option.User "TERM",
				 SOME (B.V.String.to_lower term))]))
     | value_handler (Option.Disable_Flow_Control, pipe, _) = ()
     | value_handler (Option.Enable_Flow_Control, pipe, _) = ()
     | value_handler (Option.Flow_Control_Any, pipe, _) = ()
     | value_handler (Option.Flow_Control_Xon, pipe, _) = ()
     | value_handler (Option.Request_X_Display, pipe, _) = ()
     | value_handler (Option.X_Display_Is {host, display, screen}, pipe, _) =
	()
     | value_handler (Option.Request_Environment list, pipe, _) = ()
     | value_handler (Option.Environment_Is list, pipe, _) =
	B.Pipe.enqueue (pipe, list)
     | value_handler (Option.Environment_Update list, pipe, _) =
	B.Pipe.enqueue (pipe, list)
     | value_handler (Option.Old_Request_Environment list, pipe, _) = ()
     | value_handler (Option.Old_Environment_Is list, pipe, _) =
	B.Pipe.enqueue (pipe, list)
     | value_handler (Option.Old_Environment_Update list, pipe, _) =
	B.Pipe.enqueue (pipe, list)
     | value_handler (Option.Wrong_Request_Environment list, pipe, _) = ()
     | value_handler (Option.Wrong_Environment_Is list, pipe, _) =
	B.Pipe.enqueue (pipe, list)
     | value_handler (Option.Wrong_Environment_Update list, pipe, _) =
	B.Pipe.enqueue (pipe, list)
(*
		7.	internal function status_handler
*)

   val environment_request = Option.Request_Environment [Option.All_Vars]
   val old_environment_request =
        Option.Old_Request_Environment [Option.All_Vars]

   fun status_handler (status_pipe, env_pipe, master)
                      (Telnet.C {extension, ... }, status) =
        (Trace.trace_print (fn _ => "[Received " ^
			    Telnet.Telnet_Status.makestring status ^ "]");
         case status of
	    Telnet.Telnet_Status.Lower_Status lower =>
	     if connection_closing lower then
	      B.Pipe.enqueue (status_pipe, Done)
	     else ()
	  | Telnet.Telnet_Status.Remote_Request request =>
	     let val {option, accept} = request
	         val Telnet.TCE {send_value, active, pending, ...} = extension
		 fun safe_accept value = ((accept value) handle _ => ())
	     in case option of
	           Option.Terminal_Type =>
		    (safe_accept (Telnet.Telnet_Status.Yes);
		     send_value Option.Request_Terminal_Type)
		 | Option.Window_Size =>
		    safe_accept (Telnet.Telnet_Status.Yes)
		 | Option.Suppress_Go_Ahead =>
		    safe_accept (Telnet.Telnet_Status.Yes)
		 | Option.X_Display_Location =>
		    (safe_accept (Telnet.Telnet_Status.Yes);
		     send_value Option.Request_X_Display)
		 | Option.Environment =>
		    (safe_accept (Telnet.Telnet_Status.Yes);
		     send_value environment_request)
		 | Option.Old_Environment =>
		    (if pending {option = Option.Environment,
				 remote = true} then
		      (* delay, to allow other option to become established. *)
		      B.Scheduler.sleep 10
		     else ();
		     if active {option = Option.Environment,
				remote = true} then
		      safe_accept (Telnet.Telnet_Status.No)
		     else
		      (safe_accept (Telnet.Telnet_Status.Yes);
		       send_value old_environment_request))
		 | _ => safe_accept (Telnet.Telnet_Status.No)
	      end
	  | Telnet.Telnet_Status.Local_Request request =>
	     let val {option, accept} = request
	         val Telnet.TCE {send_value, ...} = extension
		 fun safe_accept value = ((accept value) handle _ => ())
	     in case option of
		   Option.Suppress_Go_Ahead =>
		    safe_accept (Telnet.Telnet_Status.Yes)
		 | Option.Echo =>
		    safe_accept (Telnet.Telnet_Status.Yes)
		 | _ => safe_accept (Telnet.Telnet_Status.No)
	     end
	  | Telnet.Telnet_Status.Remote_Response response =>
	     let val {option, now_valid} = response
	         val Telnet.TCE {send_value, request_remote, ...} = extension
	     in if now_valid = Telnet.Telnet_Status.On then
	         case option of
		    Option.Terminal_Type =>
		     send_value Option.Request_Terminal_Type
		  | Option.Environment =>
		     send_value environment_request
		  | Option.Old_Environment =>
		     send_value environment_request
		  | Option.X_Display_Location =>
		     send_value Option.Request_X_Display
		  | _ => ()
		else	(* if Environment refused, try Old_Environment *)
		 case option of
		    Option.Environment =>
		     request_remote Option.Old_Environment
		  | _ => ()
	     end
	  | Telnet.Telnet_Status.Local_Response response => ()
	  | Telnet.Telnet_Status.Remote_Disable option => ()
	  | Telnet.Telnet_Status.Local_Disable option => ()
	  | Telnet.Telnet_Status.Value value =>
	     value_handler (value, env_pipe, master)
	  | Telnet.Telnet_Status.Synch => ()
	  | Telnet.Telnet_Status.Interrupt_Process =>
	       B.Pipe.enqueue (status_pipe, Interrupt)
	  | Telnet.Telnet_Status.Abort_Output =>
	       B.Pipe.enqueue (status_pipe, Kill_Output)
	  | Telnet.Telnet_Status.Go_Ahead => ()
	  | Telnet.Telnet_Status.Erase_Char => ()
	  | Telnet.Telnet_Status.Erase_Line => ())

(*
		8.	internal function conn_handler
*)

  fun conn_handler key =
       let val (master, slave) = Pty.openpty (init_termios, init_winsize)
	   fun output str =
	        (Posix.IO.writeVec (master, {buf = str, i = 0, sz = NONE});
		 ())
	   fun data_handler (_, data) = output data
	   fun get_user [] = NONE
	     | get_user ((Option.User_Name, value) :: rest) = SOME value
	     | get_user ((Option.User variable, value) :: rest) =
	        if B.V.String.caseless_equal (variable, "user") then SOME value
		else get_user rest
	     | get_user (first :: rest) = get_user rest
	   fun process_string NONE = ""
	     | process_string (SOME s) = s
	   fun process_environment [] = []
	     | process_environment ((Option.All_Vars, value) :: rest) =
	        Trace.print_raise (Telnet_Server_Implementation_Error,
				   SOME "process_environment")
	     | process_environment ((Option.User_Name, value) :: rest) =
	        ("USER=" ^ process_string value) :: process_environment rest
	     | process_environment ((Option.Job, value) :: rest) =
	        ("JOB=" ^ process_string value) :: process_environment rest
	     | process_environment ((Option.Account, value) :: rest) =
	        ("ACCT=" ^ process_string value) :: process_environment rest
	     | process_environment ((Option.Printer, value) :: rest) =
	        ("PRINTER=" ^ process_string value) :: process_environment rest
	     | process_environment ((Option.System_Type, value) :: rest) =
	        ("SYSTEMTYPE=" ^ process_string value)
		:: process_environment rest
	     | process_environment ((Option.Display, value) :: rest) =
	        ("DISPLAY=" ^ process_string value) :: process_environment rest
	     | process_environment ((Option.User var, value) :: rest) =
	        (var ^ "=" ^ process_string value) :: process_environment rest
	   val env_pipe = B.Pipe.new ()
	   fun get_env env =
	        let val timeout = case env of [] => 3000 | _ => 300
	        in case B.Pipe.dequeue_timeout (env_pipe, timeout) of
		      NONE =>
		       (process_environment env, get_user env)
		    | SOME new_environment =>
		       get_env (new_environment @ env)
		end
	   val status_pipe = B.Pipe.new ()
	   fun main_loop (send, env) =
	        let val child = spawn (env, master, slave)
		    val kill_pid = Posix.Process.K_PROC child
		    val wait_pid = Posix.Process.W_CHILD child
		    val wait_nh = Posix.Process.waitpid_nh
		    fun loop () =
		         if wait_nh (wait_pid, []) <> NONE then
			  Trace.trace_constant_string "[Child died]\n"
			 else if Pty.select (master, 0) then
			  (send (Posix.IO.readVec (master, buffer_size));
			   loop ())
			 else
			  case B.Pipe.dequeue_timeout (status_pipe, 0) of
			     SOME Done =>
			      (Trace.trace_constant_string "[Killing child]\n";
			       Posix.Process.kill (kill_pid,
						   Posix.Signal.kill))
			   | SOME Interrupt =>
			      (Trace.trace_constant_string
			               "[sending interrupt]\n";
			       Posix.Process.kill (kill_pid,
						   Posix.Signal.int))
			   | SOME Kill_Output =>
			      let fun last_loop last =
			               if Pty.select (master, 10) then
					last_loop (Posix.IO.readVec
						   (master, buffer_size))
				       else last
			      in send "[Aborting Output]\n";
			         send (last_loop "");
			         loop ()
			      end
			   | _ => loop ()
		in loop ()
		end
	   fun connection_handler (Telnet.C {send, extension, ...}) =
	        (Trace.trace_constant_string "[Got a connection]\n";
		 main_loop (send, #1 (get_env []));
		 Posix.IO.close slave
		 (* Ignore an apparent bug in OSF1 3.2 which raises SysErr *)
		 handle _ => ();
		 Posix.IO.close master)
       in Term.set (slave, onlcr_flag);
          {connection_handler = connection_handler,
	   data_handler       = data_handler,
	   status_handler     = status_handler (status_pipe, env_pipe, master)}
       end

(*
		9.	requested options
*)

  val rlocal = Telnet.Telnet_Setup.Request_Local
  val remote = Telnet.Telnet_Setup.Request_Remote
  val start_options = [rlocal Telnet.Option.Suppress_Go_Ahead,
		       remote Telnet.Option.Suppress_Go_Ahead,
		       remote Telnet.Option.Window_Size,
		       remote Telnet.Option.Terminal_Type,
		       rlocal Telnet.Option.Echo,
		       remote Telnet.Option.X_Display_Location,
		       remote Telnet.Option.Environment]

(*
		10.	values standard_telnet_port, inetd_handler, inetd_pair
*)

  in (* local *)

   val standard_telnet_port = Word16.fromInt 23
   val inetd_handler =
	 Telnet.inetd_handler start_options (Telnet.H conn_handler)

   val inetd_pair = (standard_telnet_port, inetd_handler)

(*
		11.	function daemon
*)

   fun daemon () =
        let val rlocal = Telnet.Telnet_Setup.Request_Local
	    val remote = Telnet.Telnet_Setup.Request_Remote
	    val start_options = [rlocal Telnet.Option.Suppress_Go_Ahead,
				 remote Telnet.Option.Suppress_Go_Ahead,
				 remote Telnet.Option.Window_Size,
				 remote Telnet.Option.Terminal_Type,
				 rlocal Telnet.Option.Echo,
				 remote Telnet.Option.X_Display_Location,
				 remote Telnet.Option.Environment]
	    fun session_fun (Telnet.S {listen, ...}) =
	         let val count = Telnet.Count.Unlimited
		     val handler = Telnet.H conn_handler
		     val Telnet.L {stop, ...} = listen (lower_pattern,
							handler, count)
		     fun loop () =
		          (B.Scheduler.sleep 1000;
			   if Pty.select (Posix.FileSys.stdin, 0) then ()
			   else loop ())
		 in if background_mode then B.Scheduler.suspend (fn _ => ())
		    else loop ();
		    Trace.debug_constant_string "calling stop";
		    stop ();
		    Trace.debug_constant_string "stopped"
		 end
	in Telnet.session ((start_options, setup), session_fun)
	end

  end (* local *)

 end (* Telnet_Server *)
