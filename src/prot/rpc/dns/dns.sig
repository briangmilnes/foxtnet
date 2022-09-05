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

	The signature of a domain name server client rpc.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	DNS

		iii.	RCS Log
	
$Log: dns.sig,v $
Revision 1.9  1994/10/19  23:07:01  milnes
converted to use foxword

Revision 1.8  1994/10/07  18:57:35  milnes
Added name queries.

Revision 1.7  1994/09/23  16:52:04  milnes
Default authorities now returns a list of names with addresses,
not a DNS, and query gone. Call also returns time to make the
call.

Revision 1.6  1994/08/28  20:30:17  milnes
Simplified and added Dns_Implementation_Error.

Revision 1.5  1994/08/25  13:29:02  milnes
Correct the spelling of cannonical.

Revision 1.4  1994/08/23  17:17:37  milnes
Added support for cannonical names.

Revision 1.3  1994/08/23  11:20:44  robby
added default_authorities

Revision 1.2  94/07/04  21:36:38  esb
adapted to Copy/Create split.

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision


		1.	DNS

	Please see Mockapetris's nice RFC 1035 for a description of
	DNS. We have tried carefully to match its nomenclature and
	structure.
*)

signature DNS = 
 sig
  datatype rr_class = Wild_Record | In_Record
  datatype rdata = Name_Server of string (* Name Server *)
                 | Address_Data of FoxWord32.word (* Address *)
	         | Canonical_Name of string
                 | Ptr of string
		 | Unimplemented 
  datatype resource_record =
            Resource_Record of {name: string,
				class: rr_class,
				ttl: FoxWord32.word,    (* Time to live. *)
				rdata: rdata}

   (* 
      As per RFC 1035 [11] we should have a type and an rdatalength, 
     but type is encoded in Rdata disjunction and length is calculated
     when the packet is parsed/emitted. 
   *)
   
   datatype query_or_response = QueryServer | ServerResponse
   datatype opcode = Query_Op | IQuery_Op | Status_Op 
   datatype response_code = No_Error | Format_Error | Server_Failure | 
             Name_Error | Not_Implemented | Refused 

   datatype header =
     Header of {id: FoxWord16.word,
                qr: query_or_response,
                opcode: opcode,  
                aa: bool, (* Authoritative Answer *)
                tc: bool, (* Truncated in transmission *)
                rd: bool, (* Recursion Desired *)
                ra: bool, (* Recursion Available *)
                rcode: response_code}

   datatype qtype = Query_Address | Query_Name_Server | Query_PTR
   datatype qclass = Query_Internet | Query_Wildcard

   datatype question = 
     Question of {name: string, qtype: qtype, qclass: qclass}

   datatype query =
     Query of {header: header,
	       question: question list,
	       authority: resource_record list,
	       answer: resource_record list,
	       additional: resource_record list}
	
   structure Tcp_Or_Udp: PROTOCOL
   type dns
   val new: (string * string * FoxWord32.word) list -> dns 
   (* New requires the names of a domain, its authoritative server
      and its IP address. *)
   val default_authorities : unit -> (string * string * FoxWord32.word) list

   exception No_Authorities (* If new is called with an empty list. *)
   exception Label_Too_Long of string
   exception Dns_Implementation_Error of string
   (* Signalled if the implementation makes an error or if some
     unimplemented message arrives. *)

   val initialize: unit -> int
   val finalize: unit -> int

   val call: Tcp_Or_Udp.address * query -> (query * int) option
   val address_query: dns * string -> dns * (FoxWord32.word list option)
   val authority_query: dns * string -> dns * (string list option)
   val name_query: dns * FoxWord32.word -> dns * (string list option)

   (* A simple interface for interactive debugging of name server calls. *)
   val set_server : string -> unit
   val icall : query -> unit
   val make_address_query : string -> query
   val make_name_query : FoxWord32.word -> query
   val make_authority_query : string -> query
 end
