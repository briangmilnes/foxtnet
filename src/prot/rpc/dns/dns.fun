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

	A domain name server client rpc functor.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Lacunae
	1.	functor Dns
	2.	domain_key_list
	3.	new
	4.	fun default_authorities
	5.	size_query
	6.	emit_query
	7.	parse_query
	8.	initialize and finalize
	9.	call
	10.	update_dns_against_response
	11.	make_address_query and make_authority_query
	12.	timeout_dns
	13.	ports
	14.	DNS Control Algorithm
	15.	find_addresses
	16.	find_names
	17.	find_authorities
	18.	find_canonical_names
	19.	find_authorities_to_query
	20.	rescore_authority 
	21.	query_authorities
	22.	canonical_name_of
	23.	address_query 
	24.	name_query
	25.	authority_query
	26.	interactive debugging

		iii.	RCS Log
	
$Log: dns.fun,v $
Revision 1.29  1995/03/12  17:56:18  esb
adapted to new trace.sig.

Revision 1.28  1995/03/10  03:50:54  esb
adapted to new vendor.sig.

Revision 1.27  1995/03/07  23:50:43  esb
updated tracing.

Revision 1.26  1995/02/04  20:40:19  robby
updated to 107

Revision 1.25  1995/01/17  21:01:10  esb
used FoxMakestring.wordNN instead of makestring.

Revision 1.24  1994/12/01  18:47:07  esb
renamed parameters to Event_Queue.{signal,wait}.

Revision 1.23  1994/11/11  18:12:28  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.22  1994/11/10  16:12:20  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.21  1994/11/07  21:35:59  cline
use V.String

Revision 1.20  1994/10/28  17:51:24  milnes
Fixed a typo.

Revision 1.19  1994/10/28  17:43:29  milnes
Added some tracing in pursuit of a Byte1+ bug in emit_name.
Installed a work around for it by using integers and translating
at the last instant.

Revision 1.18  1994/10/27  22:29:36  milnes
Added some nice string upcases.

Revision 1.17  1994/10/27  21:40:58  milnes
Reordered the default_authorities.

Revision 1.16  1994/10/27  20:29:12  cline
added SML/NJ 105 compatibility

Revision 1.15  1994/10/19  23:07:01  milnes
converted to use foxword

Revision 1.14  1994/10/07  18:57:21  milnes
Added name_query.

Revision 1.13  1994/09/23  16:50:56  milnes
Added a whole new control strategy that is much more robust over
name servers giving you bogus data.

Revision 1.12  1994/09/12  18:23:48  milnes
Updated with more tracing to help track bugs in nameservers,
more still would be a good idea.

Revision 1.11  1994/08/30  21:38:40  robby
added an andrew-side default authority

Revision 1.10  1994/08/30  21:32:20  milnes
Improved some prints.

Revision 1.9  1994/08/28  20:33:01  milnes
Updated for the new signature.

Revision 1.8  1994/08/25  13:29:40  milnes
Updated for canonical names, not cannonical names.

Revision 1.7  1994/08/23  17:17:17  milnes
Added support for cannonical names.

Revision 1.6  1994/08/23  11:19:48  robby
added default_authorities

Revision 1.5  94/07/13  16:54:40  milnes
Updated to allow ping to use dns.

Revision 1.4  1994/07/04  21:36:38  esb
adapted to Copy/Create split.

Revision 1.3  1994/07/01  03:35:52  danwang
Moved control structures into Fox_Basis.

Revision 1.2  1994/06/29  19:34:07  milnes
Constrained the functor with the Dns signature.

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision




	iv.	Lacunae

	The Dns service is incomplete, it could be extended with
well-known service records or other useful queries. It also does not
have the best control structure: it probably ought to keep batting
averages and response times on servers. Also, I'm not sure that it is
attending to issues of authoritative answers versus non-authoritative
answers.  I'm not sure this is handling timeouts of zero either.

 The call function is not caching open connections and reusing
them, and eventually timing them out. This makes multi-threaded uses
of this a bit odd, I should at least allocate new random port numbers
and catch open failures.

  I've installed canonical names, but I'm not reformulating the
query in terms of canonical names.


	1.	functor Dns
*)

functor Dns (structure B: FOX_BASIS
             datatype Tcp_Or_Udp = Tcp | Udp
             val Tcp_Or_Udp: Tcp_Or_Udp
             structure Tcp_Or_Udp: PROTOCOL
	     val make_address: FoxWord32.word * FoxWord16.word
                             -> Tcp_Or_Udp.address
             (* Build an address for communicating using the foreign IP
                number and the local port number. *)
             sharing type Tcp_Or_Udp.incoming = B.Dyn_Array.T
                 and type Tcp_Or_Udp.outgoing = B.Dyn_Array.T
                 and type Tcp_Or_Udp.allocation = int
	     val debug_level: int ref option): DNS  = 
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "dns.fun")
  val local_print = Trace.local_print
  val trace_print = Trace.trace_print
  val trace_constant_string = Trace.trace_constant_string
  val debug_print = Trace.debug_print
  
  val n1ux3F = SW.n8"0x3F"
  val n1ux78 = SW.n8"0x78"
  val n1uxC0 = SW.n8"0xC0"
  val n2u255 = SW.n16"255"
  val n2u32768 = SW.n16"32768"
  val n2uxFFFF = SW.n16"0xFFFF"
  val n4u999999 = SW.n32"999999"
  val n4ux0FFFFFFF = SW.n32"0x0FFFFFFF"
  val n4ux80022332 = SW.n32"0x80022332"
  val n4ux8002deb4 = SW.n32"0x8002deb4"
  val n4uxC6290004 = SW.n32"0xC6290004"
  val n4uxff=SW.n32"0xff"
  val n1u0=SW.n8"0"
  val n1u1=SW.n8"1"
  val n1u2=SW.n8"2"
  val n1u3=SW.n8"3"
  val n1u4=SW.n8"4"
  val n1u5=SW.n8"5"
  val n1u8=SW.n8"8"
  val n1u4=SW.n8"4"
  val n1ux0F=SW.n8"0x0F"
  val n2ux0=SW.n16"0x0"
  val n2u1=SW.n16"1"
  val n2u2=SW.n16"2"
  val n2u4=SW.n16"4"
  val n2u12=SW.n16"12"

  val ord = B.V.Char.ord
  val chr = B.V.Char.chr
  val concat = B.V.String.concat
  val explode = B.V.String.explode

  fun pairToWord (high, low) = let open FoxWord32 in
	  orb ( lshift(intToWord high, 16), intToWord low)
        end

  datatype rr_class = Wild_Record | In_Record
  fun makestring_rr_class Wild_Record = "Wild_Record"
    | makestring_rr_class In_Record = "In_Record"

  datatype rdata = Name_Server of string (* Name Server *)
                 | Address_Data of FoxWord32.word (* Address *)
	         | Canonical_Name of string
                 | Ptr of string
		 | Unimplemented

  fun makestring_ip_number ip =
   (makestring (FoxWord32.wordToInt (FoxWord32.rshiftl (ip, 24))) ^ "." ^
    makestring (FoxWord32.wordToInt (FoxWord32.andb (FoxWord32.rshiftl (ip, 16), n4uxff))) ^ "." ^
    makestring (FoxWord32.wordToInt (FoxWord32.andb (FoxWord32.rshiftl (ip, 8), n4uxff))) ^ "." ^
    makestring (FoxWord32.wordToInt (FoxWord32.andb (ip, n4uxff))))

  fun makestring_reversed_ip_number ip =
   (makestring (FoxWord32.wordToInt (FoxWord32.andb (ip, n4uxff))) ^ "." ^
    makestring (FoxWord32.wordToInt (FoxWord32.andb (FoxWord32.rshiftl (ip, 8), n4uxff))) ^ "." ^
    makestring (FoxWord32.wordToInt (FoxWord32.andb (FoxWord32.rshiftl (ip, 16), n4uxff))) ^ "." ^
    makestring (FoxWord32.wordToInt (FoxWord32.rshiftl (ip, 24))))

  fun makestring_rdata (Name_Server string) = "Name_Server " ^ string ^ " "
   |  makestring_rdata (Address_Data ubyte4) = 
      "Address_Data " ^ makestring_ip_number ubyte4 ^ " " 
   |  makestring_rdata (Canonical_Name string) = "Canonical_Name " ^ string ^ " "
   |  makestring_rdata (Ptr string) = "Ptr " ^ string ^ " "
   |  makestring_rdata Unimplemented = "Unimplemented"

  datatype resource_record =
             Resource_Record of {name: string,
				 class: rr_class,
				 ttl: FoxWord32.word,    (* Time to live. *)
				 rdata: rdata}

  fun makestring_resource_record (Resource_Record {name,class,ttl,rdata}) =
   ("Resource_Record{name= " ^ name ^ ", " ^
                       "class=  " ^ (makestring_rr_class class) ^ ", " ^
                       "ttl= " ^ (makestring ttl) ^ ", " ^
                       "rdata= " ^ (makestring_rdata rdata) ^ "}")

   (* 
      As per RFC 1035 [11] we should have a type and an rdatalength, 
     but type is encoded in Rdata disjunction and length is calculated
     when the packet is parsed/emitted. 
   *)
   
  datatype query_or_response = QueryServer | ServerResponse
  fun makestring_query_or_response QueryServer = "QueryServer" 
   |  makestring_query_or_response ServerResponse = "ServerResponse"

  datatype opcode = Query_Op | IQuery_Op | Status_Op 
  fun makestring_opcode Query_Op = "Query_Op"
    | makestring_opcode IQuery_Op = "IQuery_Op" 
    | makestring_opcode Status_Op = "Status_Op"

  datatype response_code = No_Error | Format_Error | Server_Failure | 
            Name_Error | Not_Implemented | Refused 

  fun makestring_response_code No_Error = "No_Error"
    | makestring_response_code Format_Error = "Format_Error"
    | makestring_response_code Server_Failure = "Server_Failure"
    | makestring_response_code Name_Error = "Name_Error"
    | makestring_response_code Not_Implemented = "Not_Implemented"
    | makestring_response_code Refused = "Refused"
 
  datatype header =
     Header of {id: FoxWord16.word,
                qr: query_or_response,
                opcode: opcode,  
                aa: bool, (* Authoritative Answer *)
                tc: bool, (* Truncated in transmission *)
                rd: bool, (* Recursion Desired *)
                ra: bool, (* Recursion Available *)
                rcode: response_code}

  fun makestring_bool true = "true"
    | makestring_bool false = "false"

  fun makestring_header (Header {id, qr, opcode, aa, tc, rd, ra, rcode}) =
       ("Header {id =" ^ FoxMakestring.word16 id ^
              ", qr = " ^ makestring_query_or_response qr ^ 
              ", opcode = " ^ makestring_opcode opcode ^
              ", aa = " ^ makestring_bool aa ^
              ", tc = " ^ makestring_bool tc ^
              ", rd = " ^ makestring_bool rd ^
              ", ra = " ^ makestring_bool ra ^
              ", rcode = " ^ makestring_response_code rcode ^ "}")

  datatype qtype = Query_Address | Query_Name_Server | Query_PTR
  fun makestring_qtype Query_Address = "Query_Address"
   |  makestring_qtype Query_Name_Server = "Query_Name_Server"
   |  makestring_qtype Query_PTR = "Query_PTR"
 
  datatype qclass = Query_Internet | Query_Wildcard
  fun makestring_qclass Query_Internet = "Query_Internet"
    | makestring_qclass Query_Wildcard = "Query_Wildcard"
 
  datatype question = 
     Question of {name: string, qtype: qtype, qclass: qclass}

  fun makestring_question (Question {name,qtype,qclass}) =
   ("Question {name = " ^ name ^ ", qtype = " ^ (makestring_qtype qtype) ^ ","
                ^ "qclass = "  ^ (makestring_qclass qclass) ^ "}")

  datatype query = 
     Query of {header: header,
	       question: question list,
	       authority: resource_record list,
	       answer: resource_record list,
	       additional: resource_record list}
	
  fun makestring_list (f,l) = 
   let
     fun makestring_list_aux [] = ["]"]
       | makestring_list_aux (a :: []) = (f a) :: ["]"]
       | makestring_list_aux (a :: r) = (f a) :: ", " :: (makestring_list_aux r)
   in
     concat ("[" :: (makestring_list_aux l))
   end 

  fun makestring_query (Query {header,question,authority,answer,additional}) =
   ("\nQuery {header = " ^ (makestring_header header) ^ 
          ",\n      question = " ^ (makestring_list (makestring_question,question)) ^
          ",\n      authority = " ^ (makestring_list (makestring_resource_record,authority)) ^
          ",\n      answer = " ^ (makestring_list (makestring_resource_record,answer)) ^
          ",\n      additional = " ^ (makestring_list (makestring_resource_record,additional)) ^ "}")
        
  structure Tcp_Or_Udp = Tcp_Or_Udp

  fun rrequal (Resource_Record {name, class, rdata,...},
	       Resource_Record {name = name1, class = class1,
				rdata = rdata1,...}) =
        (name = name1 andalso class = class1 andalso rdata = rdata1)

  fun member (rr, []) = false
    | member (rr1, rr2 :: rest) =
      if rrequal(rr1,rr2) then true else member(rr1,rest)

  structure Tree = 
    Tree (type label = string 
 	  val label_eq = (op= : string * string -> bool)
          fun makestring_label s = s
          type data = resource_record list
          fun makestring_data rrs = 
            (makestring_list (makestring_resource_record, rrs))
          val null_data = [])

  fun makestring_rr_tree t =
    let
     fun ms (Tree.Node{alabel,data,zchildren},children_strings) =
      SOME 
       (concat 
        ((fold op@ (map (fn rr => [(makestring_resource_record rr) ^ "\n"]) data)
                  [])
        @ children_strings))
    in
     case Tree.walk (t,ms) of
       NONE => ""
     | SOME s => s
    end
   
  fun makestring_domain (s) = ("\"" ^ s ^ "\"")

  (* Each time we query an authority, we keep track of the amount of
     time (ms) to answer the query, and if it times out give it 30 seconds. *)
 
  datatype score = Score of {avg_rt : int, queries : int}

  fun makestring_score (Score {avg_rt, queries}) =
    ("Score{avg_rt =" ^ (makestring avg_rt) ^
     ", queries = " ^ (makestring queries) ^ "}")

  fun hash_string s = (fold Integer.+ (map ord (explode s)) 0)

 exception Dns_Implementation_Error of string

 fun implementation_error name =
  (
   local_print ("raising Dns_Implementation_Error " ^ name);
   raise (Dns_Implementation_Error name)
  )

(*
	RFC 1034 and 1035 advocate keeping a weighted average of the
	success and cost of communicating with each authority over any
	available address. However, we don't.
*)
 
  datatype dns = 
     Dns of {authorities: Tree.tree, (* The RR's of authoritative name servers. *)
             addresses: Tree.tree,  (* The addresses of machines. *)
	     names : (FoxWord32.word,resource_record list) B.Store.T,
              (* A store mapping from the addresses of machines to their names. *)
             canonical_names : Tree.tree,
             (* The RR's specifying canonical names for addresses and authorities. *)
             default_authorities: resource_record list,
             (* The score board of response times for servers. *)
             scoreboard : (string,score) B.Store.T}

  exception Timeout
  exception Label_Too_Long of string

  val default_ttl = n4u999999


(*
	2.	domain_key_list
*)

fun domain_key_list namestring =
 let
  fun dkl ([], l) = l
    | dkl (#"." :: r, f) = dkl (r,[] :: f)
    | dkl (c :: r, l :: rest) = dkl (r, (c :: l) :: rest)
    | dkl (c :: r, []) = raise Match
 in
  if namestring = "" then [""] else
  "" :: (map (implode o rev) (dkl (explode namestring,[[]])))
 end


(*
	3.	new
*)

 fun time () = 
  let val B.V.Time.TIME {sec,...} = B.V.Time.gettimeofday ()
  in FoxWord32.intToWord sec
  end

 fun ip_hash (b) = FoxWord32.wordToInt (FoxWord32.andb(n4ux0FFFFFFF,b))
    
 exception No_Authorities

 fun tree_data NONE = []
   | tree_data (SOME (Tree.Node{data,...})) = data

 fun store_data NONE = []
   | store_data (SOME (_,data)) = data

 fun new [] = raise No_Authorities
   | new daips =
  (debug_print (fn _ => "new dns server");
   let
     val now = time ()

     fun mk_authorities [] = (Tree.new ("",[]),[])
       | mk_authorities ((domain, server, ip_number) :: rest) =
        let 
          val domain = B.V.String.upcase domain
          val rr = Resource_Record {name = domain, class = In_Record,
				    ttl = FoxWord32.+(now,default_ttl),
				    rdata = Name_Server (B.V.String.upcase
							 server)}
          val (t, default_list) = mk_authorities rest
	  val new_authorities = rr :: (tree_data (Tree.find(t,domain_key_list domain)))
        in
          (Tree.add (t, domain_key_list domain, new_authorities),
           rr :: default_list)
        end

     fun mk_addresses_and_names [] = 
          (Tree.new ("",[]),
           B.Store.new(ip_hash,op= :(FoxWord32.word * FoxWord32.word -> bool)))
       | mk_addresses_and_names ((domain, server, ip_number) :: rest) =
        let 
          val server = B.V.String.upcase server
          val rr = Resource_Record {name = server, class = In_Record,
				    ttl = FoxWord32.+(now,default_ttl),
				    rdata = Address_Data ip_number}
          val (t,n) = mk_addresses_and_names rest
          val new_address_rrs = rr :: (tree_data (Tree.find(t, domain_key_list domain)))
          val new_name_rrs =    rr :: (store_data (B.Store.look(n,ip_number)))
        in
          (Tree.add (t, domain_key_list server, new_address_rrs),
           B.Store.add(n,ip_number,new_name_rrs))
        end

     val (authorities, authorities_list) = mk_authorities daips
     val (addresses, names) = mk_addresses_and_names daips

   in
    Dns {authorities = authorities,
         addresses = addresses,
         names = names,
         canonical_names = Tree.new("",[]),
         default_authorities = authorities_list,
         scoreboard = 
           B.Store.new(hash_string,op= : string * string -> bool)}
   end)


(*
	4.	fun default_authorities
*)

  fun default_authorities () =
    let 
      val mango_ip = n4ux8002deb4
      val ns_internic_net_ip = n4uxC6290004
      val netserver_ip = n4ux80022332
    in
      [("cs.cmu.edu", "mango.srv.cs.cmu.edu", mango_ip),
       ("andrew.cmu.edu", "netserver.andrew.cmu.edu", netserver_ip), 
       ("", "ns.internic.net", ns_internic_net_ip)]
    end


(*
	5.	size_query
*)

fun size_name n =
 let 
  (* Each name gets a length byte, and a byte for each character.
    The last name (or root) is of length zero, so it is a zero byte. *)
  fun size_name_by_chars (#"." :: rest) = 1 + (size_name_by_chars rest)
    | size_name_by_chars (a :: rest) = 1 + (size_name_by_chars rest)
    | size_name_by_chars [] = 1 
 in
 1 + (size_name_by_chars (explode n))
 end 



fun size_rdata (Name_Server name) = size_name name
  | size_rdata (Canonical_Name name) = size_name name
  | size_rdata (Ptr name) = size_name name
  | size_rdata (Address_Data _) = 4
  | size_rdata Unimplemented =
     implementation_error "attempt to size Unimplemented rdata"

fun size_query (Query {header, question, authority, answer, additional}) =
 let
  fun size_resource_record (Resource_Record {name, rdata, ...}) = 
       10 + (size_name name) + (size_rdata rdata)

  fun size_header h = 12

  fun size_question (Question {name,...}) = (size_name name) + 4

  fun sum_list (counter,[]) = 0
    | sum_list (counter, a :: rest) = (counter a) + (sum_list (counter, rest))

 in
  (if Tcp_Or_Udp = Tcp then 2 else 0) + (* Add in that two byte length field. *)
  (size_header header) + 
  (sum_list (size_question, question)) + 
  (sum_list (size_resource_record, authority)) + 
  (sum_list (size_resource_record, answer)) +
  (sum_list (size_resource_record, additional))
 end 


(*
	6.	emit_query
*)

fun print_packet packet = 
     (local_print ("packet of length " ^ makestring (B.Dyn_Array.size packet));
      local_print (B.Format.makestring (B.Dyn_Array.read packet)))

val aa_mask = n1u4
val tc_mask = n1u2
val rd_mask = n1u1
val ra_mask = n1u8

fun emit_query (q as (Query {header, question, authority,
			     answer, additional}),
                packet) =
 let 
  val size = B.Dyn_Array.size packet

(* The original loop.
  fun emit_name (name, loc) = 
   let
     fun write (loc_of_size_byte, chars_written, write_at_next,[]) = 
        (if chars_written = n1uxff 
         then raise (Label_Too_Long ("in domain name " ^ (makestring_domain name)))
         else if chars_written <> n1u0 then
	  B.Dyn_Array.update1 (packet, loc_of_size_byte, chars_written)
         else ();
         B.Dyn_Array.update1 (packet, write_at_next, n1u0);
         (* The root name is a single octet filled with zero. *)
         write_at_next + 1)
      | write (loc_of_size_byte, chars_written, write_at_next, "." :: rest) = 
        (B.Dyn_Array.update1 (packet, loc_of_size_byte, chars_written);
         write (write_at_next, n1u0, write_at_next + 1, rest)
        )
      | write (loc_of_size_byte, chars_written, write_at_next, c :: rest) =
        (B.Dyn_Array.update1 (packet, write_at_next, FoxWord8.intToWord (ord c));
         write (loc_of_size_byte, FoxWord8.+(chars_written,n1u1),
		write_at_next + 1, rest))
   in
    write (loc, n1u0, loc+1, explode name)
   end  
*)

(* The Byte1 loop.
  fun emit_name (name, loc) = 
   let
     fun write (loc_of_size_byte, chars_written, write_at_next,[]) = 
        (if chars_written = n1uxff 
         then raise (Label_Too_Long ("in domain name "^
	                             (makestring_domain name)))
         else if chars_written <> n1u0 then
	  B.Dyn_Array.update1 (packet, loc_of_size_byte, chars_written)
         else ();
         B.Dyn_Array.update1 (packet, write_at_next, n1u0);
         (* The root name is a single octet filled with zero. *)
         write_at_next + 1)
      | write (loc_of_size_byte, chars_written, write_at_next, #"." :: rest) = 
        (B.Dyn_Array.update1 (packet, loc_of_size_byte, chars_written);
         write (write_at_next,1u0, write_at_next + 1, rest)
        )
      | write (loc_of_size_byte, chars_written, write_at_next, c :: rest) =
        (B.Dyn_Array.update1 (packet, write_at_next, FoxWord8.intToWord (ord c));
         write (loc_of_size_byte, FoxWord8.+(chars_written,n1u1),
		write_at_next + 1, rest))
   in
    write (loc, 1u0, loc+1, explode name)
   end  
*)

(* With integer counts of the characters written. Work around for bug. *)
  fun emit_name (name, loc) =
   let
     fun write (loc_of_size_byte, chars_written, write_at_next,[]) =
        (if chars_written = 255
         then raise (Label_Too_Long ("in domain name " ^ (makestring_domain name)))
         else if chars_written <> 0 then
	  B.Dyn_Array.update1 (packet, loc_of_size_byte, FoxWord8.intToWord chars_written)
         else ();
         B.Dyn_Array.update1 (packet, write_at_next, n1u0);
         (* The root name is a single octet filled with zero. *)
         write_at_next + 1)
      | write (loc_of_size_byte, chars_written, write_at_next, #"." :: rest) = 
        (B.Dyn_Array.update1 (packet, loc_of_size_byte, FoxWord8.intToWord chars_written);
         write (write_at_next, 0, write_at_next + 1, rest)
        )
      | write (loc_of_size_byte, chars_written, write_at_next, c :: rest) =
        (B.Dyn_Array.update1 (packet, write_at_next, FoxWord8.intToWord (ord c));
         write (loc_of_size_byte, chars_written + 1, write_at_next + 1, rest))
   in
    write (loc, 0, loc+1, explode name)
   end

  fun emit_rdata (Name_Server name, loc) = emit_name (name, loc)
    | emit_rdata (Address_Data address, loc) =
       (B.Dyn_Array.update4 (packet, loc, B.Order.B4.to_big address);
        loc+4)
    | emit_rdata (Canonical_Name name, loc) = emit_name (name,loc)
    | emit_rdata (Ptr name, loc) = emit_name (name,loc)
    | emit_rdata (Unimplemented, loc) = 
      implementation_error "attempt to emit Unimplemented rdata"

  (* Read the type code from the rdata constructor. *)
  fun emit_type (Name_Server _, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u2); loc + 2)
    | emit_type (Address_Data _, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u1); loc + 2)
    | emit_type (Canonical_Name string, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u4); loc + 2)
    | emit_type (Ptr string, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u12); loc + 2)
    | emit_type (Unimplemented,_) =
       implementation_error "attempt to emit type of Unimplemented rdata"

  fun emit_length (i, loc) = 
    (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big (FoxWord16.intToWord i)); loc + 2)

  fun emit_ttl (ttl, loc) = 
    (B.Dyn_Array.update4 (packet, loc, B.Order.B4.to_big ttl); loc + 4)

  fun emit_class (Wild_Record, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u255); loc + 2)
    | emit_class (In_Record, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u1); loc + 2)

  fun emit_resource_record (Resource_Record {name, class, ttl, rdata}, loc) =
   emit_rdata (rdata,
    emit_length (size_rdata rdata,
        emit_ttl (ttl,
         emit_class (class, emit_type (rdata, emit_name (name, loc))))))
 
   fun emit_opcode Query_Op = n1u0
     | emit_opcode IQuery_Op = n1u1
     | emit_opcode Status_Op = n1u2

   fun emit_rcode No_Error = n1u0
     | emit_rcode Format_Error = n1u1
     | emit_rcode Server_Failure = n1u2
     | emit_rcode Name_Error = n1u3
     | emit_rcode Not_Implemented = n1u4
     | emit_rcode Refused = n1u5

  fun emit_query_or_response QueryServer = n1u0 
    | emit_query_or_response ServerResponse = n1u1

  fun emit_header (Header {id, qr, opcode, aa, tc, rd, ra, rcode},
                   qdcount, anscount, nscount, arcount, loc) =
   let 
     val b2 = FoxWord8.orb(emit_rcode rcode, if ra then ra_mask else n1u0);
     val b1 = FoxWord8.orb(if rd then rd_mask else n1u0,
               FoxWord8.orb(if tc then tc_mask else n1u0,
                FoxWord8.orb(if aa then aa_mask else n1u0,
                 FoxWord8.orb(FoxWord8.lshift(emit_opcode opcode, 3),
                          FoxWord8.lshift(emit_query_or_response qr, 7)))))
   in
    B.Dyn_Array.update2 (packet, 0, B.Order.B2.to_big id);
    B.Dyn_Array.update1 (packet, 2, b1);
    B.Dyn_Array.update1 (packet, 3, b2);
    B.Dyn_Array.update2 (packet,4,B.Order.B2.to_big (FoxWord16.intToWord qdcount));
    B.Dyn_Array.update2 (packet,6,B.Order.B2.to_big (FoxWord16.intToWord anscount));
    B.Dyn_Array.update2 (packet,8,B.Order.B2.to_big (FoxWord16.intToWord nscount));
    B.Dyn_Array.update2 (packet,10,B.Order.B2.to_big (FoxWord16.intToWord arcount));
    loc + 12
   end

  fun emit_qclass (Query_Internet, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u1); loc+2)
    | emit_qclass (Query_Wildcard, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u255); loc+2)

  fun emit_qtype (Query_Address, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u1); loc+2)
    | emit_qtype (Query_Name_Server, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u2); loc+2)
    | emit_qtype (Query_PTR, loc) = 
       (B.Dyn_Array.update2 (packet, loc, B.Order.B2.to_big n2u12); loc+2)

  fun emit_question (Question{name, qtype, qclass}, loc) =
    emit_qclass (qclass, emit_qtype (qtype, emit_name (name, loc)))

  fun emit_list (emitter,[], loc) = loc
    | emit_list (emitter, a :: rest, loc) = 
       emit_list (emitter, rest, emitter (a, loc))
 in
  emit_list (emit_resource_record, additional,
   emit_list (emit_resource_record, authority,
    emit_list (emit_resource_record, answer,
     emit_list (emit_question, question,
      emit_header (header, 
                  length question, length answer,
                  length authority, length additional, 
                  (if Tcp_Or_Udp = Tcp
                   then (B.Dyn_Array.update2
			 (packet,0,B.Order.B2.to_big (FoxWord16.intToWord size)); 2)
                   else 0))))));
  packet
 end 


(*

	7.	parse_query
*)
 
 fun parse_query packet =
  let
   exception Bad_Parse

   fun parse_name loc =
    let 
     fun parse_chars (l, n) =
      (case FoxWord8.wordToInt n of 
         0 => []
       | _ => 
	   (String.str (chr (FoxWord8.wordToInt (B.Dyn_Array.sub1 (packet, l)))) ::
	    parse_chars (l+1, FoxWord8.-(n,n1u1))))

     fun parse_label loc = 
      let
        val length = B.Dyn_Array.sub1 (packet, loc)
      in
	if length = n1u0
	  then ([""], loc+1)
	  else (if FoxWord8.andb(n1uxC0, length) <> n1u0
		  then  (* A compressed name, see RFC 1035 pg 30. *)
		    let val index1 = FoxWord16.intToWord 
		      (FoxWord8.wordToInt (FoxWord8.andb
					(B.Dyn_Array.sub1 (packet, loc),
					 n1ux3F)))
			val index2 = FoxWord16.intToWord 
			  (FoxWord8.wordToInt (B.Dyn_Array.sub1 (packet, loc + 1)))
			val index  = FoxWord16.wordToInt
			  (FoxWord16.orb (FoxWord16.lshift (index1, 8), index2))
			val (label,_) = parse_label index
		    in
		      (label, loc+2)
		    end
		  else let val label_list = parse_chars (loc+1, length)
			   val new_loc = loc + 1 + (FoxWord8.wordToInt length)
			   val (rest, new_loc) = parse_label new_loc
		  in 
		    if rest = [""]
		      then ((label_list @ rest), new_loc)
		      else ((label_list @ ("." :: rest)), new_loc)
		  end)
      end
  
     val (labellist, loc) = (parse_label loc)
     val name = B.V.String.upcase (concat labellist)
   in
     (name, loc)
    end

   fun parse_rdata (loc, 1, rdlength) (* Address_Data *)  =
       (Address_Data (B.Order.B4.from_big (B.Dyn_Array.sub4 (packet, loc))), loc+4)
     | parse_rdata (loc, 2, rdlength) (* Name_Server *) = 
       let 
         val (string, loc) = parse_name loc
       in
        (Name_Server string, loc)
       end
     | parse_rdata (loc, 5, rdlength) (* Canonical_Name *) =
       let 
         val (string, loc) = parse_name loc
       in
        (Canonical_Name string, loc)
       end
     | parse_rdata (loc, 12, rdlength) (* PTR *) =
       let 
         val (string, loc) = parse_name loc
       in
        (Ptr string, loc)
       end
     | parse_rdata (loc,_, rdlength) = (Unimplemented, loc+rdlength)

   fun parse_ttl loc = (B.Order.B4.from_big (B.Dyn_Array.sub4 
					     (packet, loc)), loc+4)

(*
   fun internalize_class n2u255 = Wild_Record
     | internalize_class n2u1   = In_Record
     | internalize_class _ = raise Bad_Parse
*)

   fun internalize_class n =
    (case FoxWord16.wordToInt n of
       255 => Wild_Record
     | 1   => In_Record
     | _   => raise Bad_Parse)

   fun parse_class loc = 
     (internalize_class (B.Order.B2.from_big 
			 (B.Dyn_Array.sub2 (packet, loc))), loc+2)

   fun parse_list (loc, n, parser) =
    let
      fun parse_list_aux (l, loc, n) =
       (case FoxWord16.wordToInt n of
          0 =>  (rev l, loc)
        | _ =>
          let
            val (value, newloc) = parser loc
          in
           parse_list_aux (value :: l, newloc, FoxWord16.-(n,n2u1))
          end)
    in
     parse_list_aux ([], loc, n) 
    end

   fun parse_resource_record loc =
    let
     val (name, loc)  = parse_name loc
     val (rtype, loc)  = 
      (FoxWord16.wordToInt(B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc))), loc+2)
     val (class, loc) = parse_class loc
     val rdlength     = FoxWord16.wordToInt (B.Order.B2.from_big
					 (B.Dyn_Array.sub2 (packet, loc)))
     val (ttl, loc)   = parse_ttl loc
     val (rdata, loc) = parse_rdata (loc+2, rtype, rdlength)
    in (Resource_Record {name = name, class = class, ttl = ttl,
			 rdata = rdata}, loc)
    end 

   fun internalize_qclass n =
     (case FoxWord16.wordToInt n of
       1   => Query_Internet
     | 255 => Query_Wildcard
     |   _ => raise Bad_Parse)

   fun parse_qclass loc = 
     (internalize_qclass (B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc))), loc+2)

   fun internalize_qtype n =
     (case FoxWord16.wordToInt n of
        1  => Query_Address
      | 2  => Query_Name_Server
      | 12 => Query_PTR
      |  _ => raise Bad_Parse)

   fun parse_qtype loc = 
     (internalize_qtype (B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc))), loc+2)

   fun parse_question loc = 
    let val (name, loc) = parse_name loc
        val (qtype, loc) = parse_qtype loc
        val (qclass, loc) = parse_qclass loc
    in
     (Question{name = name, qtype = qtype, qclass = qclass}, loc)
    end

(*
   fun internalize_opcode n1u0 = Query_Op	
     | internalize_opcode n1u1 = IQuery_Op
     | internalize_opcode n1u2 = Status_Op
     | internalize_opcode _ = raise Bad_Parse

   fun internalize_rcode n1u0 = No_Error
     | internalize_rcode n1u1 = Format_Error
     | internalize_rcode n1u2 = Server_Failure
     | internalize_rcode n1u3 = Name_Error
     | internalize_rcode n1u4 = Not_Implemented
     | internalize_rcode n1u5 = Refused
     | internalize_rcode _ = raise Bad_Parse

   fun internalize_query_or_response n1u0 = QueryServer
     | internalize_query_or_response n1u1 = ServerResponse
     | internalize_query_or_response _ = raise Bad_Parse

*)

   fun internalize_opcode n = 
    (case (FoxWord8.wordToInt n) of
       0 => Query_Op
     | 1 => IQuery_Op
     | 2 => Status_Op
     | _ => raise Bad_Parse)

   fun internalize_rcode n = 
    (case FoxWord8.wordToInt n of 
       0 => No_Error
     | 1 => Format_Error
     | 2 => Server_Failure
     | 3 => Name_Error
     | 4 => Not_Implemented
     | 5 => Refused
     | _ => raise Bad_Parse)

   fun internalize_query_or_response n =
    (case FoxWord8.wordToInt n of
       0 => QueryServer
     | 1 => ServerResponse
     | _ => raise Bad_Parse)

   fun parse_header loc = 
    let 
     val b2 = B.Dyn_Array.sub1 (packet, loc+2)
     val b3 = B.Dyn_Array.sub1 (packet, loc+3)
    in
     (Header{id = B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc)),
             qr = internalize_query_or_response (FoxWord8.rshiftl(b2, 7)),
             opcode = internalize_opcode 
	     (FoxWord8.rshiftl(FoxWord8.andb(b2, n1ux78),3)),
             aa = FoxWord8.andb(b2, aa_mask) <> n1u0,
             tc = FoxWord8.andb(b2, tc_mask) <> n1u0,
             rd = FoxWord8.andb(b2, rd_mask) <> n1u0,
             ra = FoxWord8.andb(b3, ra_mask) <> n1u0,
             rcode = internalize_rcode (FoxWord8.andb(b3, n1ux0F))},
      loc + 12,
      B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc+4)),
      B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc+6)),
      B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc+8)),
      B.Order.B2.from_big (B.Dyn_Array.sub2 (packet, loc+10)))
    end
 in 
  let
   val (length,loc) =
     (if Tcp_Or_Udp = Tcp 
      then (SOME (B.Order.B2.from_big (B.Dyn_Array.sub2(packet,0))), 2)
      else (NONE, 0))
  in
   if Tcp_Or_Udp = Tcp then 
   (case length of 
     SOME length => 
      debug_print (fn _ => "Tcp length = " ^ FoxMakestring.word16 length ^
		   " length of packet = " ^
		   makestring (B.Dyn_Array.size packet))
    | NONE => ())
    else ();
   let 
    val (header, loc, qdcount, ancount, nscount, arcount) = parse_header loc
    val (question, loc)   = parse_list (loc, qdcount, parse_question)
    val (answer, loc)     = parse_list (loc, ancount, parse_resource_record)
    val (authority, loc)  = parse_list (loc, nscount, parse_resource_record)
    val (additional, loc) = parse_list (loc, arcount, parse_resource_record)
   in
   SOME (Query{header = header,
               question   = question,
               authority  = authority,
               answer     = answer,
               additional = additional})
   end
  end 
   handle ByteArray.Subscript =>
           (local_print "parse_packet subscript error";
            print_packet packet; NONE)
         | B.Dyn_Array.Subscript =>
           (local_print "parse_packet subscript error";
            print_packet packet; NONE)
         | Bad_Parse => 
           (local_print "parse_packet subscript error";
            print_packet packet; NONE)
         | x => 
           (local_print ("parse_packet other exception " ^
		   System.exn_name x);
            print_packet packet; NONE)
 end


(*
	8.	initialize and finalize
*)

val state = ref 0

fun initialize () = 
 let val refcount = Tcp_Or_Udp.initialize ()
 in
  state := refcount;
  refcount
 end 
 
fun finalize () = 
 let val refcount = Tcp_Or_Udp.finalize ()
 in
  state := refcount;
  refcount
 end 



(*
	9.	call

 Call makes a single RPC call to an authority and closes the connection.

 Fix to use an exponential backoff.

*)

val initial_query_timeout = 2000
val max_query_timeout = 30000

fun call (address, query) =
let 
 fun callaux (address, query, query_timeout) =
  if query_timeout > max_query_timeout then
   (trace_constant_string "max_query_timeout exceeded in backoff, failing";
    NONE)
  else
  let
    val _ = trace_print (fn _ => "query = " ^ makestring_query query)
    val queue = B.Event_Queue.new ()
 
   fun receive_data_and_status connection =
   let 
    fun receive_data packet = 
     let val response = parse_query packet
     in
      case response of 
        NONE => (local_print "call bad response packet ignored")
      | SOME response => 
         (trace_constant_string "call received response";
          B.Event_Queue.signal {queue = queue, match = fn () => true,
				value = SOME response};
          ())
     end
     fun receive_status _ = ()
    in
     (receive_data, receive_status)
    end
 
    fun timeout_on_query_wait () =
         (B.Scheduler.sleep query_timeout;
          debug_print (fn _ => "call timing out");
          B.Event_Queue.signal {queue = queue, match = fn () => true,
			        value = NONE};
          ())
 
    val _ = trace_print (fn _ => "connecting to " ^
			 Tcp_Or_Udp.makestring_address address)
 
    val connection = 
     Tcp_Or_Udp.connect (address, Tcp_Or_Udp.Handler receive_data_and_status)
 
    fun send_query () = 
      (debug_print (fn _ => "call sending query");
       let
        val (packet,sender) =
         Tcp_Or_Udp.allocate_send (connection,size_query query)
       in
        emit_query (query,packet);
        sender ()
       end)
    in
     let 
      val time = B.Time.time_of_day()
      val result = 
       B.Event_Queue.wait {queue = queue, event = (),
			   while_waiting = timeout_on_query_wait o send_query}
     in
       trace_print (fn _ => "closing connecting to " ^ 
		    Tcp_Or_Udp.makestring_address address);
       Tcp_Or_Udp.close connection;
       case result of
         NONE => (trace_constant_string "query timed out"; 
                   callaux(address, query, 2 * query_timeout))
       | SOME result => 
          (trace_print (fn _ => "received response = " ^ makestring_query result);
           SOME (result,
		 floor (1000.0 *
			(B.Time.time_to_real (B.Time.- (B.Time.time_of_day (),
							time))))))
     end
      handle Dns_Implementation_Error s =>
       (Tcp_Or_Udp.close connection; raise (Dns_Implementation_Error s))
    end 
 in
  callaux(address,query,initial_query_timeout)
 end 


(*
		10.	update_dns_against_response
*)

fun update_dns_against_response
     (Dns {authorities, addresses, names, canonical_names, 
           default_authorities, scoreboard},
      Query{answer, authority, additional, ...}) =
 let 
  val now = time ()
  val found_new = ref false

  fun update_authorities (authorities, []) = authorities
    | update_authorities
       (authorities,
        (rr as 
         (Resource_Record {name = domain, class = In_Record, ttl,
			  rdata = Name_Server authority})) :: rest) =
      (trace_print (fn _ => "adding authority for domain " 
		    ^ (makestring_domain domain) ^
		    " authority = " ^ authority);
       let 
        val rrs = (case Tree.find(authorities,domain_key_list domain) of
                    NONE => [] | 
                    SOME (Tree.Node{data=rrs,...}) => rrs)
        val new_rrs =
         if member(rr,rrs) 
         then rrs 
         else 
         (found_new := true;
          (Resource_Record {name = domain, class = In_Record,
                            ttl = FoxWord32.+(ttl,now), 
			    rdata = Name_Server authority} :: rrs))
      in
       update_authorities
         (Tree.add (authorities, domain_key_list domain, new_rrs), rest)
      end)
    | update_authorities (authorities, rr :: rest) =
       update_authorities (authorities, rest)

   fun digit_to_int d = (ord d - ord #"0")
   fun string_to_int digits = 
    (revfold (fn (d,n) => 10 * n + (digit_to_int d)) (explode digits) 0)

   fun in_addr_string_to_ip_number s =
    (case domain_key_list s of 
     (["", "ARPA","IN-ADDR",s3,s2,s1,s0]) => 
     let 
      val n0 = FoxWord32.intToWord (string_to_int s0)
      val n1 = FoxWord32.intToWord (string_to_int s1)
      val n2 = FoxWord32.intToWord (string_to_int s2)
      val n3 = FoxWord32.intToWord (string_to_int s3)
     in
      SOME (
        FoxWord32.orb(FoxWord32.lshift(n3,24),
         FoxWord32.orb(FoxWord32.lshift(n2,16),
          FoxWord32.orb(FoxWord32.lshift(n1,8),n0))))
     end                  
     | _ => NONE)

  fun update_addresses_and_names (addresses, names, []) = (addresses,names)
    | update_addresses_and_names
       (addresses,
        names,
        (rr as 
         (Resource_Record {name = hostname, class = In_Record, ttl,
                           rdata = Address_Data ip})) :: rest) =
      (trace_print (fn _ => "adding address name = " ^ hostname ^
		    " address = " ^ (makestring_ip_number ip));
      let
       val address_rrs =
         (case Tree.find(addresses,domain_key_list hostname) of
           NONE => [] 
         | SOME (Tree.Node{data=l,...}) => l)
       val name_rrs =
         (case B.Store.look(names,ip) of
            NONE => []
          | SOME (_,name_rrs) => name_rrs)
       val new_address_rrs = 
        if member(rr,address_rrs) 
        then address_rrs
        else 
        (found_new := true;
         (Resource_Record {name = hostname, class = In_Record,
                           ttl = FoxWord32.+(ttl,now), rdata = Address_Data ip})
         :: address_rrs)

       val new_name_rrs =
        if member(rr,name_rrs) 
        then name_rrs
        else 
         (found_new := true;
          (Resource_Record {name = hostname, class = In_Record,
                            ttl = FoxWord32.+(ttl,now), rdata = Address_Data ip})
          :: name_rrs)
      in
       update_addresses_and_names
         (Tree.add (addresses, domain_key_list hostname, new_address_rrs), 
           B.Store.add(names,ip,new_name_rrs),
           rest)
      end)
    | update_addresses_and_names
       (addresses,
        names,
        (rr as 
         (Resource_Record {name = addr_string, class = In_Record, ttl,
                           rdata = Ptr hostname})) :: rest) =
      (trace_print (fn _ => "adding address name = " ^ hostname ^
		    " address = " ^ addr_string);
       (case in_addr_string_to_ip_number addr_string of
        SOME ip => 
        let 
         val name_rrs = (case B.Store.look(names,ip) of
                          NONE => []
                        | SOME (_,name_rrs) => name_rrs)
         val new_name_rrs =
          if member(rr,name_rrs) 
          then name_rrs
          else 
           (found_new := true;
            (Resource_Record {name = hostname, class = In_Record,
                              ttl = FoxWord32.+(ttl,now), rdata = Address_Data ip})
            :: name_rrs)
        in
         update_addresses_and_names
           (addresses, B.Store.add(names,ip,new_name_rrs), rest)
        end
        | NONE => 
          (trace_constant_string "bad in-addr string, can't add it";
           update_addresses_and_names (addresses, names, rest))))
    | update_addresses_and_names (addresses, names, rr :: rest) =
          update_addresses_and_names (addresses, names, rest)

(*
	This should probably check for canonical name loops.
*)

  fun update_canonical_names (addresses, []) = addresses 
    | update_canonical_names 
       (addresses, 
        (rr as
         (Resource_Record {name = hostname, class = In_Record, ttl,
                         rdata = Canonical_Name cname})) :: rest) =
          (trace_print (fn _ => "found Canonical name " ^ cname ^
			" for hostname " ^ hostname); 
          let 
           val rrs = (case Tree.find (addresses,domain_key_list hostname) of
                       NONE => [] | 
                       SOME (Tree.Node{data=rrs,...}) => rrs)
           val new_rrs = 
             if member(rr,rrs) 
             then rrs 
             else 
              (found_new := true;
              (Resource_Record {name = hostname, class = In_Record, 
                                ttl = FoxWord32.+(ttl,now),
                                rdata = Canonical_Name cname}
               :: rrs))
          in
           update_canonical_names
            (Tree.add (addresses, domain_key_list hostname, new_rrs),
             rest)
          end)
    | update_canonical_names (addresses, rr :: rest) =
       update_canonical_names (addresses, rest)

  val all = authority @ answer @ additional
 in
  trace_constant_string "updating state against response";
  let 
    val authorities = update_authorities (authorities, all)
    val (addresses,names)  = update_addresses_and_names (addresses, names, all)
    val canonical_names = update_canonical_names (canonical_names,all)
  in 
   trace_print (fn _ => "authorities\n" ^ (makestring_rr_tree authorities));
   trace_print (fn _ => "addresses \n" ^ (makestring_rr_tree addresses));
   trace_print (fn _ => "canonical_names \n" ^ (makestring_rr_tree canonical_names));
   (Dns{authorities = authorities,
        addresses   = addresses, 
        names       = names,
        canonical_names = canonical_names,
        default_authorities = default_authorities,
        scoreboard = scoreboard},
    !found_new)
   end 
 end

(*
	11.	make_address_query and make_authority_query
*)

fun make_address_query (name) = 
  Query{header = Header{id = n2ux0,opcode = Query_Op, 
                        aa=false,tc=false,rd=false,ra=false,
                        rcode = No_Error,qr=QueryServer},
               answer = [], additional = [], authority = [],
               question= [Question{name = name, qtype =Query_Address,
                                   qclass= Query_Internet}]}

fun make_name_query (ip) = 
  Query{header = Header{id = n2ux0,opcode = Query_Op,
                        aa=false,tc=false,rd=false,ra=false,
                        rcode = No_Error,qr=QueryServer},
               answer = [], additional = [], authority = [],
               question= [Question{name = (makestring_reversed_ip_number ip) ^ ".IN-ADDR.ARPA",
                                   qtype = Query_PTR,
                                   qclass= Query_Internet}]}

fun make_authority_query (name) = 
  Query{header = Header{id = n2ux0,opcode = Query_Op,
                        aa=false,tc=false,rd=false,ra=false,
                        rcode = No_Error,qr=QueryServer},
               answer = [], additional = [], authority = [],
               question= [Question{name = name, qtype = Query_Name_Server,
                                    qclass= Query_Internet}]}

(*
	12.	timeout_dns
       
*)

fun timeout_dns (Dns{authorities,addresses,names,
                     canonical_names,default_authorities,scoreboard}) =
 let
  val time = time ()

  fun timeout_rr_list [] = []
    | timeout_rr_list 
       ((rr as (Resource_Record {ttl, name = domain, 
                                rdata = Name_Server authority, ...}))
              :: rest) =
       if FoxWord32.<(ttl,time)
       then 
        (trace_print (fn _ => "timing out authority domain =" ^ (makestring_domain domain) ^ 
                  " authority = " ^ authority);
         timeout_rr_list rest)
       else 
        rr :: (timeout_rr_list rest)
    | timeout_rr_list 
       ((rr as (Resource_Record {ttl, name = hostname, 
                                 rdata = Address_Data ip, ...}))
              :: rest) =
      if FoxWord32.<(ttl,time)
      then 
       (trace_print (fn _ => "timing out address = " ^ hostname ^ " address " ^
                  (makestring_ip_number ip));
        timeout_rr_list rest)
       else 
        rr :: (timeout_rr_list rest)
    | timeout_rr_list 
       ((rr as (Resource_Record {ttl, name = hostname, 
                                 rdata = Canonical_Name name, ...}))
              :: rest) =
      if FoxWord32.<(ttl,time)
      then 
      (trace_print (fn _ => "timing out canonical_name = " ^ name ^ " for alias " ^ hostname);
        timeout_rr_list rest)
       else 
        rr :: (timeout_rr_list rest)
    | timeout_rr_list (rr :: rest) = rr :: (timeout_rr_list rest)

 fun timeout_node (Tree.Node{alabel,data,zchildren},new_children) =
  let
   val new_data = timeout_rr_list data 
  in
   case (new_data,new_children) of
     ([],[]) => NONE
   | _ => SOME (Tree.Node{alabel=alabel,data=new_data,zchildren=new_children})
  end

 val addresses = Tree.walk(addresses,timeout_node)

in 
 Dns{authorities = Tree.walk (authorities,timeout_node),
     addresses =  addresses,
     names = names,
     canonical_names = Tree.walk(canonical_names,timeout_node),
     default_authorities = default_authorities,
     scoreboard = scoreboard}
end 


(*
	13.	ports
*)

 val min_port  = n2u32768
 val max_port  = n2uxFFFF
 val local_port = ref min_port

 fun next_port () =
  (local_port := FoxWord16.+(! local_port,n2u1);
   if FoxWord16.<(!local_port,min_port) orelse FoxWord16.>(!local_port, max_port)
   then local_port := min_port
   else ();
  !local_port)


(*

	14.	DNS Control Algorithm

 The control loop for DNS on a query is 

 1 check the database for the address/authority, if found timeout the 
    database and return the value.

 2 produce a sorted list of authorities ordered first by
    their closeness to the target and then by their scoring.

 3 Query the list of authorities in order until someone produces
   a NEW piece of information for one of the data bases. 
 
  Update addresses using canonical names that return.

  Update the query using canonical names, and go to 1.

  Timing out any old records in both the address and authorities database.

  Then go to step 1.

   Retry repeatedly with exponential backoff, starting at
  max{4, 2 * rtt estimate} seconds, multiplying by two
  and cutting off at 30 seconds.  With each missed query, at 30
  seconds to the estimate.

 5  If no new information is found after the whole list is exhausted, fail.
    If after N times we still don't have the answer, fail.
*)



(*
	15.	find_addresses
*)

fun find_addresses (Dns{addresses,...}, name) =
 let
  fun get_addresses ((Resource_Record{rdata = Address_Data ip, ...}) :: rest) = 
      ip :: get_addresses(rest)
    | get_addresses [] = []
    | get_addresses (rr :: rest) = get_addresses rest 
 in
  (case Tree.find(addresses, domain_key_list name) of
     NONE => NONE
   | SOME (Tree.Node{data=rrs,...}) => 
     (case get_addresses rrs of
       [] => NONE
     | l => SOME l))
 end

(*
	16.	find_names
*)

fun find_names (Dns{names,...}, ip) = 
 let
  val rrs = store_data (B.Store.look(names,ip))
  fun get_names ((Resource_Record{name,...}) :: rest) = name :: get_names(rest)
    | get_names [] = []
 in
   get_names rrs
 end
  


(*
	17.	find_authorities
*)

fun find_authorities (Dns{authorities,...}, name) =
 let
  fun get_authorities ((Resource_Record{rdata = Name_Server ns, ...}) :: rest) = 
      ns :: get_authorities(rest)
    | get_authorities [] = []
    | get_authorities (rr :: rest) = get_authorities rest 
 in
  (case Tree.find(authorities, domain_key_list name) of
     NONE => NONE 
   | SOME (Tree.Node{data=rrs,...}) =>
     (case get_authorities rrs of
       [] => NONE
     | l => SOME l))
 end




(*
	18.	find_canonical_names

 Believe it or not, you can have more than one canonical name for a machine, and
andrew.cmu.edu takes advantage of this to load ballance.

*)

fun find_canonical_names (Dns{canonical_names,...}, name) =
 let
  fun get_canonical_names
       ((Resource_Record{rdata = Canonical_Name name, ...}) :: rest) =
      name :: get_canonical_names (rest)
    | get_canonical_names [] = []
    | get_canonical_names (rr :: rest) = get_canonical_names rest 
 in
  (case Tree.find(canonical_names, domain_key_list name) of
     NONE => NONE 
   | SOME (Tree.Node{data=rrs,...}) =>
     (case get_canonical_names rrs of
       [] => NONE
     | l => SOME l))
 end



(*
	19.	find_authorities_to_query
*)

fun extract_authority_name (Resource_Record {rdata = Name_Server n, ...}) = n
  | extract_authority_name _ = 
     raise (Dns_Implementation_Error "extract_authority_name")

fun find_authorities_to_query
  (Dns {addresses, authorities, names, canonical_names, 
        default_authorities, scoreboard},
   name) =
 let
  fun get_avg_rt auth =
   (case B.Store.look(scoreboard,auth) of 
       NONE => max_query_timeout
     | SOME (scoreboard,Score {avg_rt,...}) => avg_rt)


  fun lt ((s1,i1),(s2,i2)) = (i1 < i2)

   (* From paulson via the SML/NJ library. *)

  fun sort (op > : 'a * 'a -> bool) ls = let 
     fun merge([],ys) = ys
       | merge(xs,[]) = xs
       | merge(x::xs,y::ys) =
           if x > y then y::merge(x::xs,ys) else x::merge(xs,y::ys)
     fun mergepairs(ls as [l], k) = ls
       | mergepairs(l1::l2::ls,k) =
           if k mod 2 = 1 then l1::l2::ls
           else mergepairs(merge(l1,l2)::ls, k div 2)
       | mergepairs _ = raise (Dns_Implementation_Error "mergepairs")
     fun nextrun(run,[])    = (rev run,[])
       | nextrun(run,x::xs) = if x > hd run then nextrun(x::run,xs)
                              else (rev run,x::xs)
     fun samsorting([], ls, k)    = hd(mergepairs(ls,0))
       | samsorting(x::xs, ls, k) = let 
           val (run,tail) = nextrun([x],xs)
           in samsorting(tail, mergepairs(run::ls,k+1), k+1)
           end
     in 
       case ls of [] => [] | _ => samsorting(ls, [], 0)
     end

  fun sort_authorities a =
   map #1 ((sort lt) (map (fn auth => (auth, get_avg_rt auth)) a))

  fun authorities_along_path [] =
      (trace_constant_string ("taking default authorities"); 
       (map extract_authority_name default_authorities))
    | authorities_along_path path = 
      (trace_print (fn _ => "checking for authorities on domain "
                ^ (B.V.List.fold (fn (x, y) => x ^ "." ^ y)(rev path) ""));
       (case Tree.find(authorities,path) of
           NONE => authorities_along_path (rev (tl (rev path)))
         | SOME (Tree.Node{data = ats, ...}) =>
            (sort_authorities (map extract_authority_name ats))
              @
            (authorities_along_path (rev (tl (rev path))))))

  fun in_addr_path path =
   (case (rev path) of
      ("ARPA" :: "IN-ADDR" :: rest) => true
     | _ => false)

  fun authorities_along_in_addr_path ["IN-ADDR","ARPA",""] = 
      (trace_constant_string ("taking default authorities for in-addr.arpa query");
       (map extract_authority_name default_authorities))
    | authorities_along_in_addr_path [_] = 
         raise (Dns_Implementation_Error "authorities_along_in_addr_path")
    | authorities_along_in_addr_path []  = 
         raise (Dns_Implementation_Error "authorities_along_in_addr_path")
    | authorities_along_in_addr_path path = 
      (trace_print (fn _ => "checking for authorities on domain for in-addr.arpa query"
                ^ (B.V.List.fold (fn (x, y) => x ^ "." ^ y) path ""));
       (case Tree.find(authorities,path) of
           NONE => authorities_along_in_addr_path (tl path)
         | SOME (Tree.Node{data = ats, ...}) =>
            (sort_authorities (map extract_authority_name ats))
              @
            (authorities_along_in_addr_path (tl path))))

  val path = domain_key_list name
  val authorities = if in_addr_path path
                    then authorities_along_in_addr_path (rev path)
                    else authorities_along_path path
 in
   trace_print (fn _ => "find_authorities_to_query found "^ 
		(concat (map (fn s => (s ^ " ")) authorities)));
   authorities
 end


(*
	20.	rescore_authority 

*)

fun rescore_authority 
     (Dns {authorities,
           addresses,
           names,
           canonical_names,
           default_authorities,
           scoreboard},
      authority, 
      time) =
 let 
  val scoreboard' = 
    (case B.Store.look(scoreboard,authority) of
       NONE => 
        B.Store.add(scoreboard, authority, Score{avg_rt = time, queries = 1})
     | SOME (scoreboard,Score{avg_rt,queries}) => 
        B.Store.add(scoreboard, authority, 
                    Score{avg_rt = (queries * avg_rt + time) div (queries + 1),
                                    queries = queries + 1}))
  in
   Dns {authorities = authorities,
        addresses = addresses,
        names = names,
        canonical_names = canonical_names,
        default_authorities = default_authorities,
        scoreboard = scoreboard'}
  end


(*
	21.	query_authorities
*)

fun query_authorities (dns, name, query) =
 let 
  fun query_authority (dns, []) = (dns,false)
    | query_authority (dns, authority :: rest) =
   let 
    fun query_authority_at_address (dns,[]) = (dns, false)
      | query_authority_at_address (dns, ip :: rest) =
         (case call(make_address(ip,next_port()), query) of
            NONE => query_authority_at_address 
                      (rescore_authority(dns,name,max_query_timeout),
                       rest)
          | SOME (response,time) =>
             (case update_dns_against_response
                     (rescore_authority(dns,name,max_query_timeout),response) of
                (dns,false) => query_authority_at_address (dns,rest)
              | (dns,true) => (dns,true)))
   in
     case find_addresses (dns,authority) of
       NONE => 
        (trace_print (fn _ => "No address for authority " ^ authority);
          query_authority(dns,rest))  
     | SOME addresses => 
        (case query_authority_at_address (dns,addresses) of
           (dns,false) => query_authority(dns,rest)
         | (dns,true) => (dns,true))
   end 
 in
   case find_authorities_to_query (dns,name) of
     [] => (dns, false)
   | authorities => query_authority (dns,authorities)
 end



(*
	22.	canonical_name_of
*)

fun canonical_name_of (dns, name) =
 (case find_canonical_names (dns, name) of
   NONE => 
   (trace_print (fn _ => "no canonical name for " ^ name);
     name)
 | SOME [cname] => 
   (trace_print (fn _ => "found canonical name " ^ cname ^ " for name " ^ name);
     cname)
  | _ =>
   (trace_print (fn _ => "Incorrect canonical name entry for name " ^ name);
     name))



(*
	23.	address_query 
	
*)

val query_depth_bound = 20
fun address_query (dns, name) =
 let
  val name = B.V.String.upcase name 
  fun address_query' (dns,name,depth) = 
   if depth > query_depth_bound then
    (trace_constant_string "address_query exceeding depth bound, aborted";
      (timeout_dns dns,NONE))
   else 
   (case find_addresses (dns,name) of
     SOME addresses => 
      (trace_print (fn _ => "host " ^ name ^ " at addresses " ^
		    concat (map makestring_ip_number addresses));
       (timeout_dns dns, SOME addresses))
   | NONE =>
    (trace_print (fn _ => "no address in database for host " ^ name);
      let 
       val (dns, something_new_found) = 
         query_authorities(dns, name,  make_address_query name)
      in 
       if something_new_found 
       then 
        (trace_constant_string "learned something new";
         address_query'(dns, canonical_name_of(dns,name),1 + depth))
       else
        (trace_constant_string "did not learn anything new after querying all servers, failed";
         (timeout_dns dns, NONE))
      end))
  in
   address_query'(dns,name,1)
  end


(*
	24.	name_query
	
*)

fun name_query (dns, ip) =
 let
  val ip_string = (makestring_reversed_ip_number ip) ^ ".IN-ADDR.ARPA"

  fun name_query' (dns,depth) = 
   if depth > query_depth_bound then
    (trace_constant_string "name_query exceeding depth bound, aborted";
      (timeout_dns dns,NONE))
   else 
   (case find_names (dns,ip) of
    (names as (a :: rest)) =>
      (trace_print (fn _ => "host at address" ^ ip_string ^ " has names " ^ 
       (makestring_list((fn x => x),names)));
       (timeout_dns dns, SOME names))
   | [] =>
    (trace_print (fn _ => "no names in database for address " ^ ip_string);
      let 
       val (dns, something_new_found) = 
         query_authorities(dns, ip_string,  make_name_query ip)
      in 
       if something_new_found 
       then 
        (trace_constant_string "learned something new";
         name_query'(dns, 1 + depth))
       else
        (trace_constant_string "did not learn anything new after querying all servers, failed";
         (timeout_dns dns, NONE))
      end))
  in
   name_query'(dns,1)
  end
  
(*
	25.	authority_query
*)

fun authority_query (dns, name) =
 let
  val name = B.V.String.upcase name 
  fun authority_query' (dns,name,depth) = 
   if depth > query_depth_bound then
    (trace_constant_string "address_query exceeding depth bound, aborted";
     (timeout_dns dns,NONE))
   else 
 (case find_authorities (dns,name) of
    SOME authorities => 
     (trace_print (fn _ => "domain " ^ name ^ " has authorities" ^
		   concat (map (fn name => name ^ " ") authorities));
      (timeout_dns dns, SOME authorities))
  | NONE =>
   (trace_print (fn _ => "no authorities in database for domain " ^ name);
   let 
     val authorities = find_authorities_to_query (dns,name)
   in
    (case authorities of
       [] => 
        (trace_print (fn _ => "no authorities to query for authority " ^ name);
         (timeout_dns dns, NONE))
     | _ => 
     let 
      val (dns, something_new_found) = 
        query_authorities(dns, name,  make_authority_query name)
     in 
      if something_new_found 
      then 
       (trace_constant_string "learned something new, retrying query";
        authority_query'(dns, canonical_name_of(dns,name), 1 + depth))
      else
       (trace_constant_string "did not learn anything new after querying all servers, failed.";
        (timeout_dns dns, NONE))
     end)
   end))
  in
   authority_query'(dns,name,1)
  end



(*
	26.	interactive debugging
*)

val server = ref ("",NONE) : (string * FoxWord32.word option) ref

fun set_server (name) =
 (case address_query (new (default_authorities()), name) of
    (dns,NONE) => local_print ("could not find the address for server" ^ name)
  | (dns,SOME addresses) => (server := (name,SOME (hd addresses))))

fun icall (query) =
 (local_print ("icall: query = " ^ makestring_query query);
  case ! server of
    (name, NONE) => local_print "icall: no server set, call set_server."
  | (name, SOME address) =>
   (local_print ("icall: server = " ^ name );
    initialize();
   let
     val response = call(make_address(address,next_port()), query)
   in
    finalize();
    case response of
      NONE => local_print "icall: no response to query"
    | SOME x => local_print ("icall: response to query = " ^ (makestring_query query))
   end))

end (* struct *)
