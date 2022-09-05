(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Finger

		iii.	RCS Log
	
$Log: finger.fun,v $
Revision 1.2  1996/09/17  15:53:46  cline
rewrite

Revision 1.1  1994/08/28  18:55:13  robby
Initial revision


		1.	functor Finger
*)
functor Finger (structure T: TRANSPORT_PROTOCOL
	        structure B: FOX_BASIS
		val setup: unit -> T.Setup.T
		val lookup: string -> T.Host_Id.T option
		val timeout: int): FINGER =
struct

  val finger_port = Word16.fromInt 79

  fun break s =
    let
      fun loop (l, [])      = (l, [])
	| loop (l, #"@"::r) = (l, r)
	| loop (l, c::r)    = loop (c::l, r)
    in
      case loop ([], B.V.String.explode s) of
	(rwho, where) => (B.V.String.implode (B.V.List.reverse rwho),
			  B.V.String.implode where)
    end

  fun finger spec =
    let
      val (who, where) = break spec
      val session_pipe = B.Pipe.new ()

      val result = ref NONE
      fun result_string () =
	case !result of SOME s => s | NONE => ""

      fun packet_to_string p =
	let fun list_char (w, l) = (Char.chr (Word8.toInt w))::l
	  fun array_to_string a =
	    String.implode (Word_Array.W8.U_Big.R.fold list_char [] a)
	in array_to_string (Word_Array.to8
			    (T.Incoming.sub
			     (p, {start = 0w0, length = T.Incoming.size p})))
	end

      fun string_to_packet s =
	let fun nth_of_s n =
	  Word8.fromInt (Char.ord (String.sub (s, Word.toInt n)))
	in T.Outgoing.new (Word_Array.from8
			   (Word_Array.W8.U_Big.F.tabulate
			    (nth_of_s, Word.fromInt (size s))))
	end

      fun handler_fun _ = 
	let
	  val c_pipe = B.Pipe.new ()
	  fun timeout_thread () = (B.Scheduler.sleep timeout;
				   B.Pipe.enqueue (c_pipe, ()))
	  fun c (T.C {send, ...}) = (B.Scheduler.fork timeout_thread;
				     send (string_to_packet (who ^ "\n"));
				     B.Pipe.dequeue c_pipe)
	  fun d (_, p) = result := SOME (result_string () ^ packet_to_string p)
	  fun s _ = B.Pipe.enqueue (c_pipe, ())
	in
	  {connection_handler=c, data_handler=d, status_handler=s}
	end

      fun session_fun peer (T.S {connect, ...}) = 
	connect (T.Transport_Address.Remote_Specified
		   {peer=peer, remote_port=finger_port},
		 T.H handler_fun)
    in
      if where=""
      then B.V.Print.print
             "Cannot finger local host - please specify <user>@<host>\n"
      else
      case lookup where of
        SOME peer => T.session (setup (), session_fun peer)
      | NONE => print ("Lookup failed on host ("^ where ^ ").\n");
      !result
    end

end
