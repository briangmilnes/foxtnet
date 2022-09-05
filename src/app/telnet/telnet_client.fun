(*---------------------------------------------------------------------------*>
 *
 *  telnet_client.fun
 *  11 / 9 / 95
 *  Sidd Puri
 *
 *  Telnet client program based on the Telnet functor.
 *
<*---------------------------------------------------------------------------*)

(* Telnet client functor ----------------------------------------------------*)

functor TelnetClient (

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

  (* Terminal control -------------------------------------------------------*)

  val stdin  = Posix.FileSys.stdin
  val stdout = Posix.FileSys.stdout

  fun setup_termios () = (
    Term.clear (stdin, Term.L TTY.L.icanon);	  (* cbreak mode *)
    Term.setcc (stdin, [ (TTY.V.min, chr 1), (TTY.V.time, chr 0) ])
  )

  fun output str = (
    Posix.IO.writeVec (stdout, { buf = str, i = 0, sz = NONE });
  ())

  (* Signal handling setup --------------------------------------------------*)

  exception NoSignal

  val winch_sig =
    case Signals.fromString "WINCH" of
      SOME s => s
    | NONE => raise NoSignal

  datatype message = Winch | Done

  (* Helper functions for options negotiations ------------------------------*)

  fun send_winsize control_options = let
    val (rows, cols) = get_winsize stdout
    val neg = [ cols div 256, cols mod 256, rows div 256, rows mod 256 ]
  in
    dprint (
      "[ Sending window size (" ^ makestring rows ^ ", " ^ makestring cols ^
      ") ]\n"
    );
    control_options (Opt.Subneg (Opt.Window_Size, neg))
  end

  (* Handler functions ------------------------------------------------------*)

  val message_pipe : message B.Pipe.T = B.Pipe.new ()

  fun connection_handler (
    Telnet.C { send, extension = Telnet.TCE { flush, control_options }, ... }
  ) = let
    fun winch_handler (s, n, k) = (B.Pipe.enqueue (message_pipe, Winch); k)
    val old_handlers = [
      (winch_sig,
        Signals.setHandler (winch_sig, Signals.HANDLER winch_handler))
    ]

    fun loop () =
      if poll (stdin, 0)
	then case Posix.IO.readVec (stdin, 1) of
	  "\029" => let				  (* ^] suspends telnet *)
	    val termios = TTY.tcgetattr stdin
	  in
	    dprint "[ Suspending telnet ]\n";
	    Posix.Process.kill (
	      Posix.Process.K_PROC (Posix.ProcEnv.getpid ()),
	      Posix.Signal.tstp
	    );
	    TTY.tcsetattr (stdin, TTY.TC.sanow, termios);
	    loop ()
	  end
	| str => (send [IO.Data str]; loop ())
      else case B.Pipe.dequeue_timeout (message_pipe, 10) of
	SOME Winch => (send_winsize control_options; flush (); loop ())
      | SOME Done  => output "\nConnection closed.  Exiting.\n"
      | NONE => loop ()

    val origattr = TTY.tcgetattr stdin
  in
    print "[ Got a connection ]\n";
    setup_termios ();
    loop ();
    TTY.tcsetattr (stdin, TTY.TC.sanow, origattr);
    map Signals.setHandler old_handlers;
    ()
  end

  fun data_handler (con, data) = output (IO.makestring data)

  fun status_handler (
    Telnet.C { extension = Telnet.TCE { control_options, ...}, ... },
    st
  ) = let open Opt in
    dprint ("[ " ^ Status.makestring st ^ " ]\n");
    case st of
      Status.Lower_Status Tcp.Tcp_Status.Connection_Closing =>
	B.Pipe.enqueue (message_pipe, Done)
    | Status.Option_Status (Option (  Activate, Echo, Remote)) =>
	Term.clear (stdin, Term.L TTY.L.echo)
    | Status.Option_Status (Option (Deactivate,   Echo, Remote)) =>
	Term.set   (stdin, Term.L TTY.L.echo)
    | Status.Option_Status (Subneg (Terminal_Type, [1 (* SEND *)])) => let
	val term =
	  case Posix.ProcEnv.getenv "TERM" of SOME t => t | NONE => "network"
      in
	dprint ("[ Sending term type " ^ term ^ " ]\n");
	control_options (Subneg (Terminal_Type, IS :: map ord (explode term)))
      end
    | Status.Option_Status (Option (Activate, Window_Size, Local)) =>
        send_winsize control_options
    | _ => ()
  end

  (* Launch function --------------------------------------------------------*)

  fun launch port setup remote_host = let
    fun handler_fun key = {
      connection_handler = connection_handler,
      data_handler       = data_handler,
      status_handler     = status_handler
    }

    val interface      = "se0"
    val local_hostname = Hostname.hostname interface ^ ".foxnet"
    val local_id       = Test_Addresses.get_ip local_hostname
    val peer_id        = Test_Addresses.get_ip remote_host
    val peer           = Tcp.Transport_Address.Remote_Specified
                           { peer = peer_id, remote_port = port }
    val lower_setup    = Ip.Network_Setup.Setup [{
			   local_id = local_id, interface = "\000",
			   gateways = [], mask = NONE, mtu = NONE
			 }]

    fun session_fun (Telnet.S { connect, ... }) =
      connect (peer, Telnet.H handler_fun)
  in
    Telnet.session ((setup, lower_setup), session_fun)
  end

end (* TelnetClient *)

(* Launch function ----------------------------------------------------------*)

structure TelnetClient = TelnetClient (
  structure B      = Fox_Basis
  structure Term   = Term
  structure Telnet = Telnet

  val debug = false
)

val telnet = let
  val port    = telnet_port
  val options = let open TelnetOpt in [
    Option (Activate, Suppress_Go_Ahead, Remote),
    Option (Activate, Status, Remote),
    Option (Activate, Terminal_Type, Local),
    Option (Activate, Window_Size, Local)
  ] end
in
  TelnetClient.launch port options
end

(*---------------------------------------------------------------------------*)
