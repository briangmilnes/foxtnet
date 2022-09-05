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
	iv.	Overview
	1.	signature DNS_MESSAGE

		iii.	RCS Log
	
$Log: dns.sig,v $
Revision 1.5  1997/04/22  11:28:13  esb
added a size function to the Domain_Name signature.

Revision 1.4  96/03/04  21:27:56  esb
added the "invert" function to Domain_Name, for inverse queries.

Revision 1.3  1996/01/16  21:59:37  cline
*** empty log message ***

Revision 1.2  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.1  1995/06/20  17:24:41  esb
Initial revision


		iv.	Overview

	This is the protocol interface to DNS.

		1.	signature DNS_MESSAGE
*)

signature DNS_MESSAGE = 
 sig

  structure Domain_Name: sig include KEY
                             val parse: string -> T
			     val invert: T -> T
			     val size: T -> int
			 end

  type internet_address
  type time_value

  datatype rr_type_data =
    A               (* a host address *)
      of internet_address
  | NS              (* an authoritative name server *)
      of Domain_Name.T
  | MD              (* a mail destination (Obsolete - use MX) *)
      of Domain_Name.T
  | MF              (* a mail forwarder (Obsolete - use MX) *)
      of Domain_Name.T
  | CNAME           (* the canonical name for an alias *)
      of Domain_Name.T
  | SOA             (* marks the start of a zone of authority *)
    (* This is currently not implemented, but should contain the
       following information:
      of {mname: Domain_Name.T,
	  rname: Domain_Name.T,
	  serial: Word32.word,
	  refresh: time_value,
	  retry: time_value,
	  expire: time_value,
	  minimum: Word32.word}
    *)
  | MB              (* a mailbox domain name (EXPERIMENTAL) *)
      of Domain_Name.T
  | MG              (* a mail group member (EXPERIMENTAL) *)
      of Domain_Name.T
  | MR              (* a mail rename domain name (EXPERIMENTAL) *)
      of Domain_Name.T
  | NULL            (* a null RR (EXPERIMENTAL) *)
  | WKS             (* a well known service description *)
    (* This is currently not implemented, but should contain the
       following information:
      of  {address: internet_address,
	   protocol: Word8.word,
	   services: int list}
    *)
  | PTR             (* a domain name pointer *)
      of Domain_Name.T
  | HINFO           (* host information *)
      of {cpu:string, os:string}
  | MINFO           (* mailbox or mail list information *)
    (* This is currently not implemented, but should contain the
       following information:
      of {rmailbx:Domain_Name.T, emailbx:Domain_Name.T}
    *)
  | MX              (* mail exchange *)
      of {preference:Word16.word, exchange:Domain_Name.T}
  | TXT             (* text strings *)
      of string list

  datatype rr_qtype =
    A_Q             (* a host address *)
  | NS_Q            (* an authoritative name server *)
  | MD_Q            (* a mail destination (Obsolete - use MX) *)
  | MF_Q            (* a mail forwarder (Obsolete - use MX) *)
  | CNAME_Q         (* the canonical name for an alias *)
  | SOA_Q           (* marks the start of a zone of authority *)
  | MB_Q            (* a mailbox domain name (EXPERIMENTAL) *)
  | MG_Q            (* a mail group member (EXPERIMENTAL) *)
  | MR_Q            (* a mail rename domain name (EXPERIMENTAL) *)
  | NULL_Q          (* a null RR (EXPERIMENTAL) *)
  | WKS_Q           (* a well known service description *)
  | PTR_Q           (* a domain name pointer *)
  | HINFO_Q         (* host information *)
  | MINFO_Q         (* mailbox or mail list information *)
  | MX_Q            (* mail exchange *)
  | TXT_Q           (* text strings *)
  | AXFR_Q          (* request for a transfer of an entire zone *)
  | MAILB_Q         (* request for mailbox-related records (MB, MG or MR) *)
  | MAILA_Q         (* request for mail agent RRs (Obsolete - see MX) *)
  | All_Records_Q   (* request for all records *)

  datatype rr_class =
    IN              (* the Internet *)
  | CS              (* the CSNET class (Obsolete - used only for examples in
                       some obsolete RFCs) *)
  | CH              (* the CHAOS class *)
  | HS              (* Hesiod [Dyer 87] *)
  | Any_Class       (* Query for any class *)
 
  datatype rr = RR of {name: Domain_Name.T,
		       rr_type: rr_type_data,
		       rr_class: rr_class,
		       ttl: Word32.word}

  datatype question = Question of {name: Domain_Name.T,
				   rr_qtype: rr_qtype,
				   rr_class: rr_class}

  datatype opcode = Query | Inverse_Query | Status

  datatype rcode = No_Error | Format_Error | Server_Failure | Name_Error
		 | Not_Implemented | Refused

  datatype header = Header of {query: bool,
			       opcode: opcode,
			       aa: bool,	(* authoritative answer *)
			       tc: bool,	(* truncated *)
			       rd: bool,	(* recursion desired *)
			       ra: bool,	(* recursion available *)
			       rcode: rcode}

  datatype message = Message of {header: header,
				 question: question list,
				 answer: rr list,
				 authority: rr list,
				 additional: rr list}
                   | Parse_Error of string

 end
