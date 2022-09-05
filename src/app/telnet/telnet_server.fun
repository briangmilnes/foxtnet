(*---------------------------------------------------------------------------*>
 *
 *  telnet_server.fun
 *  1 / 18 / 96
 *  Sidd Puri
 *
 *  A Telnet server based on the Telnet functor, to be used in place of
 *  telnetd.
 *
<*---------------------------------------------------------------------------*)

(* Telnet server functor ----------------------------------------------------*)

functor TelnetServer (

  structure B      : FOX_BASIS
  structure Term   : TERM
  structure Telnet : TELNET

  sharing Telnet.Lower = Tcp

  val debug : bool

) = struct

  structure Setup  = Telnet.TelnetSetup
  structure Opt    = Telnet.TelnetOpt
  structure IO     = Telnet.TelnetIO
  structure Status = Telnet.TelnetStatus

  structure TTY = Posix.TTY

  fun dprint (str : string) = if debug then print str else ()

  (* Constants --------------------------------------------------------------*)

  val IS   = 0
  val SEND = 1

  val init_termios = TTY.termios {
    iflag = let open TTY.I in flags [ brkint, icrnl, ixon ] end,
    oflag = let open TTY.O in flags [ opost               ] end,
    cflag = let open TTY.C in flags [ cread, cs8, hupcl   ] end,
    lflag = let open TTY.L in flags [ isig                ] end,
    cc    = let open TTY.V in cc [
      (eof, #"\^D"), (erase, #"\^H"), (intr, #"\^C"), (kill, #"\^U"),
      (quit, #"\^\"), (susp, #"\^Z"), (start, #"\^Q"), (stop, #"\^S"),
      (time, chr 0), (min, chr 1)
    ] end,
    ispeed = TTY.b9600,
    ospeed = TTY.b9600
  }

  val wait_opts = 1
  val buflen = 1024
  val onlcr = SysWord.fromInt 2
  val init_winsize = (25, 80)

  (* Spawn login process ----------------------------------------------------*)

  fun spawn program environ (master, slave) =
    case Posix.Process.fork () of
      SOME pid => pid
    | NONE => (
	Posix.IO.close master;
	Posix.IO.dup2 { old = slave, new = Posix.FileSys.stdin  };
	Posix.IO.dup2 { old = slave, new = Posix.FileSys.stdout };
	Posix.IO.dup2 { old = slave, new = Posix.FileSys.stderr };
	new_session slave;
	Posix.IO.close slave;

	Posix.Process.exece (program, [program], environ)
      )

  (* Sigal and options handling setup ---------------------------------------*)

  exception NoSignal

  datatype message = GotEnv of string * string | DoneEnv | Done

  (* Start a new connection -------------------------------------------------*)

  fun make_connection program key = let
    val (master, slave) = openpty (init_termios, init_winsize)
    val _ = Term.set (slave, Term.Ow onlcr)
    fun output str = (
      Posix.IO.writeVec (master, { buf = str, i = 0, sz = NONE });
    ())

    val message_pipe : message B.Pipe.T = B.Pipe.new ()

    fun connection_handler (
      Telnet.C { send, extension = Telnet.TCE { control_options, ... }, ... }
    ) = let
      fun finish_env_negotiation () = let
	fun loop env 0 = SOME env
	  | loop env n = (case B.Pipe.dequeue_timeout (message_pipe, 10) of
	      SOME (GotEnv new) => loop (new :: env) n
	    | SOME DoneEnv => loop env (n - 1)
	    | SOME Done =>
		(dprint "[ Options negotiation never terminated ]\n"; NONE)
	    | NONE => loop env n
	    )
      in loop [] wait_opts end

      fun main_loop env = let
	fun remove var (str, l) =
	  if hd (String.fields (fn c => c = #"=") str) = var then l
	  else str :: l
	fun update ((var, value), environ) =
	  (var ^ "=" ^ value) :: foldr (remove var) [] environ
	val environ = foldr update (Posix.ProcEnv.environ ()) env
	val environ = map (fn (var, value) => var ^ "=" ^ value) env
	val child = spawn program environ (master, slave)
	fun loop () =
	  if Posix.Process.waitpid_nh (Posix.Process.W_CHILD child, []) <> NONE
	    then dprint "[ Child died ]\n"
	  else if poll (master, 0)
	    then
	      (send [IO.Data (Posix.IO.readVec (master, buflen))]; loop ())
	  else case B.Pipe.dequeue_timeout (message_pipe, 10) of
	    SOME Done => (
	      dprint "[ Killing child ]\n";
	      Posix.Process.kill
	        (Posix.Process.K_PROC child, Posix.Signal.kill)
	    )
	  | _ => loop ()
      in loop () end
    in
      dprint "[ Got a connection ]\n";
      case finish_env_negotiation () of
	SOME env => main_loop env
      | NONE => ();
      Posix.IO.close slave;
      Posix.IO.close master
    end

    fun data_handler (con, data) = output (IO.makestring data)

    fun status_handler (
      Telnet.C { extension = Telnet.TCE { control_options, ...}, ... },
      st
    ) = let open Opt in
      dprint ("[ " ^ Status.makestring st ^ " ]\n");
      case st of
        Status.Option_Status (Option (Activate,   Terminal_Type, Remote)) =>
	  control_options (Subneg (Terminal_Type, [SEND]))
      | Status.Option_Status (Option (Deactivate, Terminal_Type, Remote)) => 
	  B.Pipe.enqueue (message_pipe, DoneEnv)
      | Status.Option_Status (Subneg (Terminal_Type, 0 (* IS *) :: term)) =>
	  let
	    val term = implode (map (Char.toLower o chr) term)
	  in
	    dprint ("[ Setting term type to " ^ term ^ " ]\n");
	    B.Pipe.enqueue (message_pipe, GotEnv ("TERM", term));
	    B.Pipe.enqueue (message_pipe, DoneEnv)
	  end
      | Status.Option_Status
	  (Subneg (Window_Size, [cols1, cols0, rows1, rows0])) => let
	    val (rows, cols) = (rows1 * 256 + rows0, cols1 * 256 + cols0)
	  in
	    set_winsize (master, (rows, cols))
	  end
      | Status.Lower_Status Tcp.Tcp_Status.Connection_Closing =>
	  B.Pipe.enqueue (message_pipe, Done)
      | _ => ()
    end

  in {
    connection_handler = connection_handler,
    data_handler       = data_handler,
    status_handler     = status_handler
  } end

  (* Launch function --------------------------------------------------------*)

  fun launch port program setup pattern count = let
    val interface      = "se0"
    val local_hostname = Hostname.hostname interface ^ ".foxnet"
    val local_id       = Test_Addresses.get_ip local_hostname
    val lower_setup    = Ip.Network_Setup.Setup [{
			   local_id = local_id, interface = "\000",
			   gateways = [], mask = NONE, mtu = NONE
			 }]

    fun session_fun (Telnet.S { listen, ... }) = let
      val Telnet.L { stop, ... } =
	listen (pattern, Telnet.H (make_connection program), count)
      val block_pipe : unit B.Pipe.T = B.Pipe.new ()
      fun loop () = (
	B.Pipe.dequeue_timeout (block_pipe, 10);
	if not (poll (Posix.FileSys.stdin, 0)) then loop () else ()
      )
    in
      loop ()
    end
  in
    Telnet.session ((setup, lower_setup), session_fun)
  end

end (* TelnetServer *)

(* Launch function ----------------------------------------------------------*)

structure TelnetServer = TelnetServer (
  structure B      = Fox_Basis
  structure Term   = Term
  structure Telnet = Telnet

  val debug = true
)

fun telnetd () = let
  val port    = telnet_port
  val program = "/usr/sidd/bin/login"
  val options = let open Telnet.TelnetOpt in [
    Option (Refuse,   Terminal_Type, Local),
    Option (Refuse,   Window_Size, Local),
    Option (Activate, Suppress_Go_Ahead, Local),
    Option (Activate, Echo, Local),
    Option (Activate, Terminal_Type, Remote),
    Option (Activate, Window_Size, Remote)
  ] end
  val pattern = Tcp.Transport_Pattern.Local_Specified { local_port = port }
  val count   = Telnet.Count.Unlimited
in
  TelnetServer.launch port program options pattern count
end

(*---------------------------------------------------------------------------*)
