(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Install DNS for host_id name lookup (parsing).


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Dns_Lookup
	2.	type declarations
	3.	sub-structures DnsM and Cache
	4.	internal function lookup_session
	5.	internal function fork_queries
	6.	function lookup
	7.	function inverse_lookup

		iii.	RCS Log
	
$Log: dnslookup.fun,v $
Revision 1.10  1997/12/11  19:47:20  esb
the debug-level is now set correctly (was previously always NONE).

Revision 1.9  97/04/22  11:30:09  esb
rewrote to query all the servers rather than just the first.

Revision 1.8  96/06/11  03:34:29  esb
increased the timeout to 30 seconds, more reasonable than 5.

Revision 1.7  1996/03/04  21:31:59  esb
changed functor parameters, modified substantially.

Revision 1.6  1996/02/29  17:35:58  esb
the transport setup is now a component in the overall setup.

Revision 1.5  1996/02/23  19:55:29  cline
changed functor parameters, added setup arguments

Revision 1.4  1996/02/16  16:34:27  cline
Moved signature.
Added cache.

Revision 1.3  1996/02/14  20:22:48  esb
removed IP from the functor parameters, as it is not needed.

Revision 1.2  1996/02/07  19:17:10  cline
search list of domains in case name is an abbreviation

Revision 1.1  1996/01/16  22:00:43  cline
Initial revision



	1.	functor Dns_Lookup
*)

functor Dns_Lookup (structure B: FOX_BASIS
		    structure Host_Id: TRANSPORT_HOST_ID
		    structure Dns: DNS_PROTOCOL
		      sharing type Host_Id.T = Dns.Message.internet_address
		    val timeout: int	(* milliseconds *)
		    val debug_level: int ref option): DNS_LOOKUP =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val makestring = fn _ => NONE
			   val module_name = "dnslookup.fun")

(*
	2.	type declarations
*)

  type transport_setup = Dns.Setup.T

  type host_id = Host_Id.T

  type setup = {domain_list: string list,
		servers: host_id list,
		transport: transport_setup}

(*
	3.	sub-structures DnsM and Cache
*)

  structure DnsM = Dns.Message

  structure Cache = Dns_Cache (structure B = B
			       structure DnsM = DnsM
			       val debug_level = debug_level)

(*
	4.	internal function lookup_session
*)

  local

   val header = DnsM.Header {query = true, opcode = DnsM.Query,
			     rcode = DnsM.No_Error, aa = false,
			     tc = false, rd = true, ra = false}

   fun build_query question =
        DnsM.Message {header = header, question = [question],
		      answer = [], authority = [], additional = []}

   fun try_cache (_, []) = NONE
     | try_cache (extract, q :: rest) =
        (case Cache.lookup q of
	    SOME m =>
	     (case extract m of
	         SOME r => SOME r
	       | NONE => try_cache (extract, rest))
	  | NONE =>
	     try_cache (extract, rest))

  in
   fun lookup_session (setup, questions: DnsM.question list, server,
		       extract_result: DnsM.message -> 'res option, done) =
        case try_cache (extract_result, questions) of
	   SOME result => SOME result
	 | NONE =>
	    let val question_list = ref questions
	        val return_value = ref (NONE: 'res option)
	        fun send_more () =
		     case ! question_list of [] => false | _ => true
	        fun send_next send =
	             case ! question_list of
		        [] => ()
		      | first :: rest =>
		         (question_list := rest;
		          send (build_query first))
	        fun handler key =
	             let val close = B.Event.new ()
		         fun connection_handler (Dns.C {send, ...}) =
			      B.Event.wait (close, fn _ => send_next send)
		         fun close_conn () =
		              (B.Event.signal close;
			       ())
		         fun data_handler (Dns.C {send, ...}, message) =
	                      (Cache.add message;
			       case extract_result message of
	                          SOME result =>
			           (return_value := SOME result;
			            close_conn ())
			        | NONE =>
			           if ! done orelse not (send_more ()) then
			            close_conn ()
			           else send_next send)
		         fun status_handler _ = close_conn ()
		     in {connection_handler = connection_handler,
		         data_handler = data_handler,
		         status_handler = status_handler}
		     end
	        fun run_session (Dns.S {connect, ...}) =
		     connect (server, Dns.H handler)
            in Dns.session (setup, run_session);
	       ! return_value
	    end
  end

(*
	5.	internal function make_queries
*)

  local
   val fastest_server = ref (NONE: host_id option)

   val delay_inc = 300		(* milliseconds *)

   fun delay_server (server, (result, delay)) =
        ((server, delay) :: result, delay + delay_inc)

   fun build_delays servers =
        let val (result, _) = B.V.List.revfold delay_server servers ([], 0)
	in B.V.List.reverse result
	end

  in
   fun make_queries (setup, questions, servers,
		     extract_result: DnsM.message -> 'result option) =
        let val result = B.Pipe.new (): 'result B.Pipe.T
	    val done = ref false
	    fun single_query (server, delay) () =
	         (B.Scheduler.sleep delay;
		  if ! done then ()
		  else
		   (Trace.trace_print (fn _ => "querying server " ^
				       Host_Id.makestring server);
		    case lookup_session (setup, questions, server,
					 extract_result, done) of
		       NONE => ()
		     | SOME value =>
			if ! done then ()
			else
			 (done := true;
			  fastest_server := SOME server;
			  B.Pipe.enqueue (result, value))))
	    val server_list = case ! fastest_server of
	                         NONE => build_delays servers
			       | SOME s => build_delays (s :: servers)
	    fun fork_query server = B.Scheduler.fork (single_query server)
	in map fork_query server_list;
	   B.Pipe.dequeue_timeout (result, timeout)
	end
  end

(*
	6.	function lookup
*)

  local

   fun build_question name =
        DnsM.Question {name = DnsM.Domain_Name.parse name,
		       rr_qtype = DnsM.A_Q,
		       rr_class = DnsM.IN}

   fun question name "." = build_question name
     | question name domain = build_question (name ^ "." ^ domain)

   fun extract_host_id (DnsM.Message {answer, ...}) =
        let fun loop [] = NONE
	      | loop (DnsM.RR {rr_type = DnsM.A a, ...} :: _) = SOME a
	      | loop (_::rest) = loop rest
	in loop answer
	end
     | extract_host_id (DnsM.Parse_Error e_message) =
	(Trace.local_print ("Parse error from DNS server: " ^ e_message);
	 NONE)

  in

   fun lookup {domain_list, servers, transport} name =
	case Host_Id.parse name of
	   SOME ip => SOME ip
	 | NONE =>
	    make_queries (transport, map (question name) domain_list,
			  servers, extract_host_id)
  end

(*
    fun lookup {domain_list, servers = [], transport} name =
	 (case Host_Id.parse name of
	     NONE =>
	      (Trace.local_print "empty servers list, DNS query fails";
	       NONE)
	   | SOME ip => SOME ip)
      | lookup {domain_list, servers = (server :: _), transport} name =
	 let val result_pipe = B.Pipe.new (): Host_Id.T option B.Pipe.T
	     fun append_domain "." = name
	       | append_domain d = name^"."^d
	     val name_list = map append_domain domain_list
	     fun build_query name =
		  let val header =
		           DnsM.Header {query = true, opcode = DnsM.Query,
					rcode = DnsM.No_Error, aa = false,
					tc = false, rd = true, ra = false}
		      val question =
		           DnsM.Question {name = DnsM.Domain_Name.parse name,
					  rr_qtype = DnsM.A_Q,
					  rr_class = DnsM.IN}
		  in DnsM.Message {header = header, question = [question],
				   answer = [], authority = [],
				   additional = []}
		  end
	     val queries = map build_query name_list

	     fun extract_host_id (DnsM.Message {answer, ...}) =
		  let fun loop [] = NONE
			| loop (DnsM.RR {rr_type = DnsM.A a, ...} :: _) =
			   SOME a
		        | loop (_::rest) = loop rest
		  in loop answer
		  end
	       | extract_host_id (DnsM.Parse_Error e_message) =
		  (Trace.local_print ("Parse error from DNS server: " ^
				      e_message);
		   NONE)

	 in case Host_Id.parse name of
	       SOME ip => SOME ip
	     | NONE =>
		(B.Scheduler.fork (lookup_session (transport, queries,
						   server,
						   extract_host_id,
						   result_pipe));
		 B.Pipe.dequeue result_pipe)
	 end
*)

(*
	7.	function inverse_lookup
*)

  local

   fun question host_id =
        DnsM.Question {name = DnsM.Domain_Name.invert
		                (DnsM.Domain_Name.parse
				 (Host_Id.makestring host_id)),
		       rr_qtype = DnsM.PTR_Q,
		       rr_class = DnsM.IN}

   fun extract_host_name (DnsM.Message {answer, ...}) =
        let fun loop [] = NONE
	      | loop (DnsM.RR {rr_type=DnsM.PTR addr, ...} :: _) =
	         SOME (DnsM.Domain_Name.makestring addr)
	      | loop (_ :: rest) = loop rest
	in loop answer
	end
     | extract_host_name (DnsM.Parse_Error e_message) =
	(Trace.local_print ("Inverse parse error from DNS server: " ^
			    e_message);
	 NONE)
  in

   fun inverse_lookup {domain_list, servers, transport} host_id =
	make_queries (transport, [question host_id], servers,
		      extract_host_name)
  end

(*
    fun inverse_lookup {domain_list, servers = [], transport} host_id = NONE
      | inverse_lookup {domain_list, servers = (server :: _),
			transport} host_id =
         let val result_pipe = B.Pipe.new (): string option B.Pipe.T
	     val query_header =
	          DnsM.Header
	            {query = true, opcode = DnsM.Query, rcode = DnsM.No_Error,
		     aa = false, tc = false, rd = true, ra = false} 
	     val qname =
		  DnsM.Domain_Name.invert
		    (DnsM.Domain_Name.parse (Host_Id.makestring host_id))
	     val question =
	          DnsM.Question {name = qname, rr_qtype = DnsM.PTR_Q,
				 rr_class = DnsM.IN}
	     val query =
	          DnsM.Message {header = query_header, question = [question],
				answer = [], authority = [], additional = []}
	     fun extract_host_name (DnsM.Message {answer, ...}) =
                  let fun loop [] = NONE
		        | loop (DnsM.RR {rr_type=DnsM.PTR addr, ...} :: _) =
		           SOME (DnsM.Domain_Name.makestring addr)
		        | loop (_::rest) = loop rest
		  in loop answer
		  end
	       | extract_host_name (DnsM.Parse_Error e_message) =
		  (Trace.local_print ("Parse error from DNS server: " ^
				      e_message);
		   NONE)
	 in B.Scheduler.fork (lookup_session (transport, [query], server,
					      extract_host_name, result_pipe));
	    B.Pipe.dequeue result_pipe
	 end
*)

 end

