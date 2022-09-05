(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Ken Cline (Ken.Cline@cs.cmu.edu)
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
	1.	functor Test_Stream

		iii.	RCS Log
	
$Log: stream.fun,v $
Revision 1.7  1997/04/22  11:31:32  esb
minor changes.

Revision 1.6  96/02/07  19:20:50  cline
 replaced B.Scheduler.reset () with B.Scheduler.sleep 2500

Revision 1.5  1996/01/19  23:06:05  esb
adapted to the new wordarray signature.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/11/12  16:41:12  esb
adapted to new Word_Array.

Revision 1.2  1995/09/25  20:34:23  cline
*** empty log message ***

Revision 1.1  1995/09/20  19:55:51  cline
Initial revision



		1.	functor Test_Stream
*)

functor Test_Stream (structure B: FOX_BASIS
		     structure Sender: PROTOCOL
		     val sender_setup: Sender.Setup.T
		     val receiver_address: Sender.Address.T
		     structure Receiver: PROTOCOL
		     val receiver_setup: Receiver.Setup.T
		     val sender_pattern: Receiver.Pattern.T
		     val data_bytes: Word.word
		     val equal: (Word_Array.T * Word_Array.T -> bool) option
		     val timeout_time: int
		     val debug_level: int ref option
		     val test_name: string): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "stream.fun (" ^ test_name ^ ")"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_constant_string = Trace.debug_constant_string

  fun timeout (pipe, value) () =
       (B.Scheduler.sleep timeout_time;
	if B.Pipe.size pipe < 0 then
	 local_print "time-out"
	else ();
	B.Pipe.enqueue (pipe, value))


  fun generate (index, limit) =
       if index >= limit then NONE
       else
	SOME (Word8.fromInt (Word.toInt index), (index + 0w1, limit))

  val outgoing_data = (Sender.Outgoing.new o Word_Array.from8)
			(Word_Array.W8.U_Big.F.new generate (0w0, data_bytes))
  val incoming_data = ref (Receiver.Incoming.uninitialized 0w0)

  fun status_handler packet =
       Trace.debug_constant_string "received status message"

  fun sender_data_handler _ =
       B.Test.test ("sender received packet", fn _ => false)

  fun send_connection pipe (Sender.C {send, abort, extension}) =
       (B.Pipe.dequeue pipe;
	Trace.debug_constant_string "sending data...";
	send outgoing_data)
       handle x => Trace.print_raise_again (x, SOME "send_connection")

  fun send_handler pipe key =
       {connection_handler = send_connection pipe,
	data_handler = sender_data_handler,
	status_handler = status_handler}

  fun send_session pipe (Sender.S {connect, ...}) =
       (connect (receiver_address, Sender.H (send_handler pipe)))
       handle x => Trace.print_raise_again (x, SOME "send_session")

  fun receiver_data_handler (_, packet) =
       (Trace.debug_constant_string "received packet";
	incoming_data := Receiver.Incoming.join (!incoming_data, packet))
       handle x => Trace.print_raise_again (x, SOME "receiver_data_handler")

  fun receive_status_handler pipe packet =
       (B.Pipe.enqueue (pipe, true);
	status_handler packet)

  fun receive_connection (signal_pipe, wait_pipe) _ =
       (B.Pipe.enqueue (signal_pipe, B.Pipe.dequeue wait_pipe);
	Trace.debug_constant_string "leaving receive_connection")
	handle x => Trace.print_raise_again (x, SOME "receive_connection")

  fun receive_handler pipe key =
    let
      val new_done = B.Pipe.new (): bool B.Pipe.T
    in
      {connection_handler = receive_connection (pipe, new_done),
       data_handler = receiver_data_handler,
       status_handler = receive_status_handler new_done}
    end

  fun test_result _ =
    let
      val size =  Receiver.Incoming.size (!incoming_data)
      fun sub s = {start = 0w0, length = s}
      val outgoing = Sender.Outgoing.sub (outgoing_data, sub data_bytes)
      val incoming = Receiver.Incoming.sub (!incoming_data, sub size)
      val eql = case equal of
                   SOME e => e
	         | NONE =>
		    (fn (a, b) =>
		     Word_Array.W8.U_Big.F.equal (Word_Array.to8 a,
						  Word_Array.to8 b))
    in
      (size = data_bytes andalso eql (outgoing, incoming))
      orelse (Trace.local_print
	        (if size <> data_bytes then
		   ("received wrong amount of data: got " ^
		    Word.toString size ^ " bytes, expected " ^
		    Word.toString data_bytes ^ "bytes")
		 else
		   ("received wrong data, " ^
		    B.Compare.compare (Word_Array.to8 outgoing,
				       Word_Array.to8 incoming)));
	      false)
    end

  fun receive_session synch (Receiver.S {listen, ...}) =
    let
      val done = B.Pipe.new (): bool B.Pipe.T
    in
      listen (sender_pattern,
	      Receiver.H (receive_handler done),
	      Receiver.Count.Maximum 1);
      Trace.debug_constant_string "receiver listening";
      B.Pipe.enqueue (synch, ());
      B.Scheduler.fork (timeout (done, false));
      if B.Pipe.dequeue done then
	B.Test.test ("receive", test_result)
      else
	(Trace.local_print "receiver timed out";
	 B.Test.test ("receive", fn _ => false))
    end
  handle x => Trace.print_raise_again (x, SOME "receive_session")

  fun run_tests () =
       let val synch = B.Pipe.new (): unit B.Pipe.T
	   val send_completion = B.Pipe.new (): unit B.Pipe.T
	   val receive_completion = B.Pipe.new (): unit B.Pipe.T
	   fun sender () =
		(B.Scheduler.fork (timeout (send_completion, ()));
		 Sender.session (sender_setup, send_session synch);
		 B.Pipe.enqueue (send_completion, ()))
	   fun receiver () =
		(B.Scheduler.fork (timeout (receive_completion, ()));
		 Receiver.session (receiver_setup, receive_session synch);
		 B.Pipe.enqueue (receive_completion, ()))
       in
	 B.Scheduler.sleep 2500;
	 B.Scheduler.fork sender;
	 B.Scheduler.fork receiver;
	 Trace.debug_constant_string "dequeueing send-completion";
	 B.Pipe.dequeue send_completion;
	 Trace.debug_constant_string "dequeueing receive-completion";
	 B.Pipe.dequeue receive_completion;
	 B.Scheduler.sleep 1000
       end

  fun run () = B.Test.tests (test_name, 1, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end
