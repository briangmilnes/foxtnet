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
	1.	signature Dns_Cache
	2.	functor Dns_Cache
	3.	function add
	4.	function lookup

		iii.	RCS Log
	
$Log: dnscache.fun,v $
Revision 1.7  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.6  97/06/04  11:43:43  esb
added a debug_level, as required by cache.fun.

Revision 1.5  97/04/22  11:29:13  esb
rewrote to use the functor Single_Cache.

Revision 1.4  97/01/24  15:02:56  cline
adapted to value restriction

Revision 1.3  1996/04/18  21:30:22  cline
updated to match new TIME signature

Revision 1.2  1996/02/16  16:33:33  cline
Purge cache when fun.
Cleaned up signature.

Revision 1.1  1996/02/15  20:45:53  cline
Initial revision



	1.	signature Dns_Cache

*)

signature DNS_CACHE =
 sig
  type message
  type question
  val add: message -> unit
  val lookup: question -> message option
 end

(*
	2.	functor Dns_Cache
*)

functor Dns_Cache (structure B: FOX_BASIS
		   structure DnsM: DNS_EXTERN
		   val debug_level: int ref option): DNS_CACHE =
 struct
  (* maximum number of entries in cache, and maximum space taken
     to store cache data. *)
  val max_size = 128
  val max_space = 10000

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "dnscache.fun"
			   val makestring = fn _ => NONE)

  type message = DnsM.message
  type question = DnsM.question
  type domain_name = DnsM.Domain_Name.T
  type ip_number = DnsM.internet_address

  structure Cache = Single_Cache (structure V = B.V
				  type key = domain_name
				  type value = (DnsM.rr * B.V.Time.time) list
				  val max_elements = max_size
				  val max_space = max_space
				  val eq = DnsM.Domain_Name.equal
				  val hash = DnsM.Domain_Name.hash
				  val debug_level = debug_level)

(*
	3.	function add
*)

  local
   fun ttl_to_time (DnsM.RR {ttl, ...}) =
         B.V.Time.+ (B.V.Time.now (), B.V.Time.fromSeconds (Word32.toInt ttl))
   fun time_max (t,t') = if B.V.Time.> (t, t') then t else t'
   fun merge (rr, []) = [(rr, ttl_to_time rr)]
     | merge (rr, (first_rr, expiration)::rest) =
         if DnsM.equal_rr (rr, first_rr)
	   then [(first_rr, time_max (expiration, ttl_to_time first_rr))]
	 else (first_rr, expiration)::merge (rr, rest)

   fun rr_space [] = 0
     | rr_space ((DnsM.RR {name, ...}, ttl) :: rest) =
        DnsM.Domain_Name.size name + rr_space rest

   fun add_cache (name, rr) =
        let val merged = case Cache.look name of
	                    SOME rrs => merge (rr, rrs)
			  | NONE => [(rr, ttl_to_time rr)]
            val space = rr_space merged
        in Cache.add (name, merged, space)
        end

   fun add_rr (rr as DnsM.RR {ttl = 0w0:Word32.word, ...}) = ()
     | add_rr (rr as DnsM.RR {name, ...}) = add_cache (name, rr)

   fun add_rrs ([]) = ()
     | add_rrs (rr :: rest) = (add_rr rr; add_rrs rest)

(*
   fun dump () =
     let
       fun dump_rrs [] = ""
	 | dump_rrs ((rr,_)::rest) = ("    " ^ DnsM.makestring_rr rr ^ "\n" ^
				      dump_rrs rest)
       fun dump_item (key, rrs) =
	 "  [" ^ DnsM.Domain_Name.makestring key ^"]" ^ "\n" ^
	 dump_rrs rrs
     in
       print ("Current cache contents:\n" ^ Cache.makestring (dump_item, ""))
     end
*)

  in
   fun add (m as DnsM.Message {answer, authority, additional, ...}) =
        (Trace.trace_print (fn () => "adding message:\n" ^ DnsM.makestring m);
	 add_rrs answer;
	 add_rrs authority;
	 add_rrs additional)
     | add (DnsM.Parse_Error _) = ()
  end

(*
	4.	function lookup
*)

  local
   fun type_to_qtype (DnsM.A _)     = DnsM.A_Q
     | type_to_qtype (DnsM.NS _)    = DnsM.NS_Q
     | type_to_qtype (DnsM.MD _)    = DnsM.MD_Q
     | type_to_qtype (DnsM.MF _ )   = DnsM.MF_Q
     | type_to_qtype (DnsM.CNAME _) = DnsM.CNAME_Q
     | type_to_qtype (DnsM.SOA)     = DnsM.SOA_Q
     | type_to_qtype (DnsM.MB _)    = DnsM.MB_Q
     | type_to_qtype (DnsM.MG _)    = DnsM.MG_Q
     | type_to_qtype (DnsM.MR _)    = DnsM.MR_Q
     | type_to_qtype (DnsM.NULL)    = DnsM.NULL_Q
     | type_to_qtype (DnsM.WKS)     = DnsM.WKS_Q
     | type_to_qtype (DnsM.PTR _)   = DnsM.PTR_Q
     | type_to_qtype (DnsM.HINFO _) = DnsM.HINFO_Q
     | type_to_qtype (DnsM.MINFO)   = DnsM.MINFO_Q
     | type_to_qtype (DnsM.MX _)    = DnsM.MX_Q
     | type_to_qtype (DnsM.TXT _)   = DnsM.TXT_Q

   fun update_ttl (DnsM.RR {name, rr_type, rr_class, ...}, expiration) =
        let val ttl = Word32.fromInt
		        (B.V.Time.toSeconds (B.V.Time.- (expiration,
							 B.V.Time.now ())))
        in DnsM.RR {name = name, rr_type = rr_type,
		    rr_class = rr_class, ttl = ttl}
        end

  (* Allow recursion in case a canonical name record is found, in
     which case the original name is replaced with the canonical name.
     Only one level of recursion is allowed. *)
   fun recursive_lookup (q as DnsM.Question {name, rr_qtype, rr_class},
			 depth) =
        let 
	    fun matching_rrs [] = []
	      | matching_rrs ((h as DnsM.RR {rr_type = DnsM.CNAME cname, ...},
			        expiration) :: tail) =
	         if depth = 0 then
		  case (recursive_lookup (DnsM.Question
					  {name = cname, rr_qtype = rr_qtype,
					   rr_class = rr_class}, 1)) of
		     SOME (DnsM.Message {answer, ...}) => answer
		   | SOME (DnsM.Parse_Error _) => matching_rrs tail
		   | NONE => matching_rrs tail
		 else matching_rrs tail
	      | matching_rrs ((h as DnsM.RR {rr_type, ...}, expiration)
			      :: tail) =
		 if type_to_qtype rr_type = rr_qtype then
		  update_ttl (h, expiration) :: matching_rrs tail
		 else matching_rrs tail
	    val rrs = case Cache.look name of
		         NONE => []
		       | SOME l => matching_rrs l
	    val header = DnsM.Header
	                   {query = false, opcode = DnsM.Query, aa = false,
			    tc = false, rd = false, ra = false,
			    rcode = DnsM.No_Error}
        in case rrs of
	      [] => NONE
	    | _ =>
	       let val m = DnsM.Message
		            {header = header, question = [q],
			     answer = rrs, authority = [], additional = []}
	       in Trace.trace_print (fn () => "Cache hit, found:\n" ^
				     DnsM.makestring m);
	          SOME m
	       end
        end

  in
   fun lookup arg = recursive_lookup (arg, 0)

  end

end
