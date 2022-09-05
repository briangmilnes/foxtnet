(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	length.fun: IP-compatible length protocol.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Device_Meter

	iii.	RCS Log

$Log: devmeter.fun,v $
Revision 1.5  1996/04/18  21:26:32  cline
updated convert to return a word8 vector

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/11/29  21:55:18  cline
ported to SML/NJ 108.13

Revision 1.2  1995/09/25  16:52:53  cline
added labels for meters.
handle xmeter startup problems.

Revision 1.1  1995/09/22  13:36:21  cline
Initial revision


	1.	functor Device_Meter

	Packet monitor wrapper for ethernet protocols
*)

functor Device_Meter (structure Lower: DEVICE_PROTOCOL
		      val xmeter_pathname: string
		      val label: string
		      structure B: FOX_BASIS
		      val debug_level : int ref option): DEVICE_PROTOCOL =
 struct
  open Lower			(* inherit all types and substructures *)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "devmeter.fun"
			   val makestring = fn _ => NONE)

  local
    val sleep_time = 500
    val scale = 5.0;
    fun dummy_kill () = Trace.local_print "No child process to kill."
    val kill_meter = ref dummy_kill
    val alive = ref false
    val packet_count = ref 0;

    fun start_meter () =
      let
	open Posix (* expose structures Process, FileSys, IO, and Signal *)
	val {infd, outfd} = Posix.IO.pipe ()
	fun convert n = Byte.stringToBytes (Real.toString (real n / scale))
	fun kill_pid pid =
	  (Process.kill (Process.K_PROC pid, Signal.kill);
	   Process.waitpid_nh (Process.W_CHILD pid, []);
	   ())
	  handle x =>
	    Trace.print_handled (x, SOME "kill_pid (killing device meter)");
	fun loop () =
	  (B.Scheduler.sleep sleep_time;
	   if !alive then
	     (IO.writeVec (outfd, {buf=convert (!packet_count), i=0, sz=NONE});
	      packet_count := 0;
	      loop ())
	   else ())
      in
	alive := true;
	case Process.fork() of
	  SOME pid => (* parent of fork *)
	    (kill_meter := (fn _ => (kill_pid pid; ()));
	     B.Scheduler.fork loop)
	| NONE => (* child of fork *)
	    (IO.dup2 {new = FileSys.stdin, old = infd};
	     Process.exec (xmeter_pathname,
			   ["Packet Meter", "-update", "1", "-label", label])
	     handle x => Trace.print_handled (x, NONE);
	     Trace.local_print ("Cannot start packet meter (" ^
				xmeter_pathname ^ ")");
	     Posix.Process.exit 0w1)
      end

    fun stop_meter () = ((!kill_meter) ();
			 alive := false;
			 kill_meter := dummy_kill)

   fun xsend send packet =
     (packet_count := !packet_count + 1;
      send packet)

   fun xconn (C {send, abort, extension}) =
        C {send = xsend send, abort = abort, extension = extension}

   fun xconn_handler upper_handler conn = upper_handler (xconn conn)

   fun xdata_handler upper_handler (conn, packet) =
        (packet_count := !packet_count + 1;
	 upper_handler (xconn conn, packet))

   fun xhandler upper_handler key =
        let
	  val {connection_handler, data_handler,
	       status_handler} = upper_handler key
        in
	  {connection_handler = xconn_handler connection_handler,
	   data_handler = xdata_handler data_handler,
	   status_handler = status_handler}
        end

   fun xconnect connect (address, H handler) =
        connect (address, H (xhandler handler))

   fun xlisten listen (pattern, H handler, max) =
        listen (pattern, H (xhandler handler), max)

   fun xsession session_fun (S {connect, listen, extension}) =
        session_fun (S {connect = xconnect connect,
			listen = xlisten listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        (start_meter ();
	 Lower.session (initialization, xsession session_fun)
	 before stop_meter ())

  end (* local *)

 end (* struct *)
