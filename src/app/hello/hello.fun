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
	1.	functor Hello

		iii.	RCS Log
	
$Log: hello.fun,v $
Revision 1.1  1996/09/16  17:54:27  cline
Initial revision


		1.	functor Hello
*)
functor Hello (structure T: TRANSPORT_PROTOCOL
	       structure B: FOX_BASIS
	       val port: T.Transport_Address.port
	       val setup: unit -> T.Setup.T
	       val lookup: string -> T.Host_Id.T option
	       val pukool: T.Host_Id.T -> string option
	       val timeout: int) =
struct

  val initial_queries = ["name", "email", "purpose", "os", "comments"]
  val bye_query = "bye"

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

  fun handler_fun (is_server, dialog_fun, result_fun)
		  (T.Transport_Key.Key {peer, ...}) = 
    let
      val queries = ref initial_queries
      fun pop_query () =
	case !queries of
	  (q::rest) => q before queries := rest
	| [] => bye_query
      val last_query = ref ""
      val pipe = B.Pipe.new ()
      val live = ref true
      fun quit () = (live:=false; B.Pipe.enqueue (pipe, ()))
      fun timeout_thread () = (B.Scheduler.sleep timeout;
			       if !live then
				 Fox_Basis.V.Print.print "Hello.fun: timeout\n"
			       else ();
			       quit ())
      fun hello peer =
	let
	  val peername = (case pukool peer of
			    SOME s => s
			  | NONE => T.Host_Id.makestring peer)
	in
	  SOME {query = "Hello: " ^ peername ^
		        "\nWe are interested in learning about users of the Fox Net." ^
		        "\nWould you mind taking a moment to answer the following questions?",
		reply = ""}
	end
      val parse_string = ref ""
      fun try_parse (#"\n"   :: rest) = try_parse rest
	| try_parse (#"\013" :: rest) = try_parse rest
	| try_parse (#"?" :: rest) = 
	    let
	      fun try_question [] q =
		     {query = B.V.String.implode q, reply = ""}
		| try_question (#"\n" :: rest) q =
		     {query = B.V.String.implode q,
		      reply = B.V.String.implode (B.V.List.reverse rest)}
		| try_question (#"\013" :: rest) q =
		     {query = B.V.String.implode q,
		      reply = B.V.String.implode (B.V.List.reverse rest)}
		| try_question (c :: rest) q = try_question rest (c :: q)
	    in
		     SOME (try_question rest [])
	    end
	| try_parse _ = NONE
      fun parse_packet p =
	let
	  val s = !parse_string ^ packet_to_string p
	  val result = try_parse (B.V.List.reverse (B.V.String.explode s))
	in
	  parse_string := (if result=NONE then s else "");
	  result
	end
      fun dialog send =
	let
	  fun ask {query, reply} =
	    let
	      val response = reply ^ "\n" ^ query ^ "?\n"
	    in
	      last_query := query;
	      send (string_to_packet response)
	    end
	in
	  fn NONE => ()
	   | (SOME {query="bye", reply}) =>
	       (result_fun ("bye", SOME reply);
		if !queries = []
		  then (if !last_query = "bye" then ()
			else ask {query = "bye", reply = bye_query};
			quit ())
		else ask {query = pop_query (), reply = ""})
	   | (SOME {query, reply}) =>
	       (result_fun (!last_query, SOME reply);
		ask {query = pop_query (), reply = dialog_fun query})
	end
      fun c (T.C {send, ...}) =
	let
	in
	  B.Scheduler.fork timeout_thread;
	  if is_server then dialog send (hello peer) else ();
	  B.Pipe.dequeue pipe;
	  result_fun ("", NONE)
	end
      fun d (T.C {send, ...}, p) =
	dialog send (parse_packet p)
	handle _ => (Fox_Basis.V.Print.print
		       "Hello.fun: exception in data handler";
		     quit ())
      fun s _ = quit ()
    in
      {connection_handler=c, data_handler=d, status_handler=s}
    end

  fun client (translate_query, dialog_fun) server_name =
    let
      fun printline (r, SOME s) =
	    Fox_Basis.V.Print.print (translate_query r ^ s^ "\n")
	| printline (r, NONE) =
	    Fox_Basis.V.Print.print (r ^ "\n")
      val handl = T.H (handler_fun (false, dialog_fun, printline))
      fun address peer =
	   T.Transport_Address.Remote_Specified {peer=peer, remote_port=port}
      fun session_fun peer (T.S {connect, ...}) = connect (address peer, handl)
    in
      case lookup server_name of
	SOME peer => T.session (setup (), session_fun peer)
      | NONE => print ("Lookup failed on host ("^ server_name ^ ").\n")
    end

end
