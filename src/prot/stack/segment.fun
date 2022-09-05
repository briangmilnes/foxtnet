(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test a segment-based protocol.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Segment

		iii.	RCS Log
	
$Log: segment.fun,v $
Revision 1.8  1997/04/22  11:31:32  esb
minor changes.

Revision 1.7  96/02/07  19:19:38  cline
replaced B.Scheduler.reset () with B.Scheduler.sleep 2500

Revision 1.6  1996/01/19  23:06:05  esb
adapted to the new wordarray signature.

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/11/12  16:41:12  esb
adapted to new Word_Array.

Revision 1.3  1995/09/18  19:28:33  cline
added schedule.reset

Revision 1.2  1995/07/05  17:44:33  esb
adapted to new wordarray signature.

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


		1.	functor Test_Segment
*)

functor Test_Segment (structure Sender: PROTOCOL
		      val sender_setup: Sender.Setup.T
		      val receiver_address: Sender.Address.T
		      structure Receiver: PROTOCOL
		      val receiver_setup: Receiver.Setup.T
		      val sender_pattern: Receiver.Pattern.T
		      val segment_size: int
		      structure B: FOX_BASIS
		      val equal_packet: (Word_Array.T * Word_Array.T
					 -> bool) option
		      val debug_level: int ref option
		      val test_name: string): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "segment.fun (" ^ test_name ^ ")"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_constant_string = Trace.debug_constant_string

  val timeout_time = 10000		(* 10 seconds *)

  fun timeout (pipe, value) () =
       (B.Scheduler.sleep timeout_time;
	if B.Pipe.size pipe < 0 then
	 local_print "time-out"
	else ();
	B.Pipe.enqueue (pipe, value))


  fun generate (index, limit) =
       if index >= limit then NONE
       else
	SOME (Word8.fromInt index, (index + 1, limit))

  val send_packet =
       Word_Array.from8 (Word_Array.W8.U_Big.F.new generate (0, segment_size))

  fun status_handler packet =
       Trace.debug_constant_string "received status message"


  fun sender_data_handler _ =
       B.Test.test ("sender received packet", fn _ => false)

  fun send_connection pipe (Sender.C {send, abort, extension}) =
       (B.Pipe.dequeue pipe;
	Trace.debug_print (fn _ => "sending packet to " ^
			   Sender.Address.makestring receiver_address);
	send (Sender.Outgoing.new send_packet))
       handle x => Trace.print_raise_again (x, SOME "send_connection")

  fun send_handler pipe key =
       {connection_handler = send_connection pipe,
	data_handler = sender_data_handler,
	status_handler = status_handler}

  fun send_session pipe (Sender.S {connect, ...}) =
       (connect (receiver_address, Sender.H (send_handler pipe)))
       handle x => Trace.print_raise_again (x, SOME "send_session")

  fun receiver_data_handler (expected, pipe) (_, packet) =
       (Trace.debug_constant_string "received packet";
        if ! expected <= 0 then
	 B.Test.test ("received too many packets", fn _ => false)
	else				(* check the data *)
	 (expected := ! expected - 1;
	  if ! expected = 0 then
	   let val size = Receiver.Incoming.size packet
	       val sub = {start = 0w0, length = size}
	       val received = Receiver.Incoming.sub (packet, sub)
	       fun default_equal (a, b) =
		    Word_Array.W8.U_Big.F.equal (Word_Array.to8 a,
						 Word_Array.to8 b)
	       val equal = case equal_packet of
		              SOME e => e
			    | NONE => default_equal
	   in if equal (send_packet, received) then
	       B.Test.test ("receive", fn _ => true)
	      else
	       (Trace.local_print
		    ("received wrong data, " ^
		     B.Compare.compare (Word_Array.to8 send_packet,
					Word_Array.to8 received));
		B.Test.test ("receive", fn _ => false));
	      B.Pipe.enqueue (pipe, true)
	   end
	  else ()))
       handle x => Trace.print_raise_again (x, SOME "receiver_data_handler")

  fun receive_connection (signal_pipe, wait_pipe) _ =
       (B.Pipe.enqueue (signal_pipe, B.Pipe.dequeue wait_pipe))
	handle x => Trace.print_raise_again (x, SOME "receive_connection")

  fun receive_handler pipe key =
       let val new_pipe = B.Pipe.new (): bool B.Pipe.T
       in {connection_handler = receive_connection (pipe, new_pipe),
	   data_handler = receiver_data_handler (ref 1, new_pipe),
	   status_handler = status_handler}
       end

  fun receive_session (synch, done) (Receiver.S {listen, ...}) =
       (let val new_done = B.Pipe.new (): bool B.Pipe.T
        in listen (sender_pattern, Receiver.H (receive_handler new_done),
		   Receiver.Count.Maximum 1);
	   Trace.debug_constant_string "receiver listening";
	   B.Pipe.enqueue (synch, ());
	   B.Scheduler.fork (timeout (new_done, false));
	   B.Pipe.enqueue (done, B.Pipe.dequeue new_done)
        end)
	 handle x => Trace.print_raise_again (x, SOME "receive_session")


  fun run_tests () =
       let val synch = B.Pipe.new (): unit B.Pipe.T
	   val done = B.Pipe.new (): bool B.Pipe.T
	   val send_completion = B.Pipe.new (): unit B.Pipe.T
	   val receive_completion = B.Pipe.new (): unit B.Pipe.T
	   fun sender () =
		(B.Scheduler.fork (timeout (send_completion, ()));
		 Sender.session (sender_setup, send_session synch);
		 B.Pipe.enqueue (send_completion, ()))
	   fun receiver () =
		(B.Scheduler.fork (timeout (receive_completion, ()));
		 Receiver.session (receiver_setup,
				   receive_session (synch, done));
		 B.Pipe.enqueue (receive_completion, ()))
       in
	 B.Scheduler.sleep 2500;
	 B.Scheduler.fork sender;
	 B.Scheduler.fork receiver;
	 B.Scheduler.fork (timeout (done, false));
	 Trace.debug_constant_string "dequeueing send-completion";
	 B.Pipe.dequeue send_completion;
	 Trace.debug_constant_string "dequeueing receive-completion";
	 B.Pipe.dequeue receive_completion;
	 Trace.debug_constant_string "dequeueing completion";
	 B.Test.test ("correct completion", fn _ => B.Pipe.dequeue done);
	 B.Scheduler.sleep 100
       end

  fun run () = B.Test.tests (test_name, 2, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end
