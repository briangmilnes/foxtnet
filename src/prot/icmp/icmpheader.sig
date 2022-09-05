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

	Signatures for unmarshaled ICMP messages.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ICMP_REDIRECT
	2.	signature ICMP_UNREACHABLE
	3.	signature ICMP_INCOMING
	4.	signature ICMP_OUTGOING

		iii.	RCS Log
	
$Log: icmpheader.sig,v $
Revision 1.2  1995/09/26  16:29:08  esb
added router advertisement messages and obsolete for obsolete messages.

Revision 1.1  1995/08/30  19:27:47  esb
Initial revision


	1.	signature ICMP_REDIRECT
*)

signature ICMP_REDIRECT =
 sig
  datatype redirect =
      Network_Redirect
    | Host_Redirect
    | Tos_Network_Redirect
    | Tos_Host_Redirect

  include PRINTABLE
   sharing type T = redirect

  type code
  val code: T -> code
  exception Unknown_Code
  val decode: code -> T
 end

(*
	2.	signature ICMP_UNREACHABLE

	The additional parameter for Frag_Needed is specified by RFC 1191.
*)

signature ICMP_UNREACHABLE =
 sig
  datatype unreachable =
      Network_Unreachable
    | Host_Unreachable
    | Protocol_Unreachable
    | Port_Unreachable
    | Fragmentation_Needed of {mtu: FoxWord16.word}
    | Source_Route_Failed
    | Network_Unknown
    | Host_Unknown
    | Source_Host_Isolated
    | Communication_With_Network_Prohibited
    | Communication_With_Host_Prohibited
    | Network_Unreachable_For_Tos
    | Host_Unreachable_For_Tos

  include PRINTABLE
   sharing type T = unreachable

  type code
  val code: T -> (code * FoxWord16.word)
  exception Unknown_Code
  val decode: (code * FoxWord16.word) -> T
 end

(*
	3.	signature ICMP_INCOMING
*)

signature ICMP_INCOMING =
 sig
  type ip_number
  type ip_protocol
  type ip_option
  type redirect
  type unreachable

  structure Data: EXTERNAL

    (* When a parameter problem message arrives, Icmp parses it out
       and represents it as a problem in the IP header, Ip options,
       or the data of the packet. *)
  datatype problem_specifier =
      Header of FoxWord8.word
    | Option of ip_option
    | End_Of_Options
    | Missing_Required_Option
    | Data of FoxWord8.word
  val makestring_problem: problem_specifier -> string

  datatype icmp_in  =
      Unreachable of unreachable * Data.T
    | Transit_Time_Exceeded of Data.T
    | Reassembly_Time_Exceeded of Data.T
    | Parameter_Problem of problem_specifier * Data.T
    | Source_Quench of Data.T
    | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     data: Data.T}
    | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			   originate: FoxWord32.word,
			   receive: FoxWord32.word,
			   transmit: FoxWord32.word,
			   returned: FoxWord32.word}
    | Traceroute of {forwarded: bool, id: FoxWord16.word,
		     out_hops: FoxWord16.word, return_hops: FoxWord16.word,
		     out_speed: FoxWord32.word, out_mtu: FoxWord32.word}
    | Redirect of {reason: redirect, new_gateway: ip_number, header: Data.T}
    | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: Data.T}
    | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word,
		     originate: FoxWord32.word, receive: FoxWord32.word}
    | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
    | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     address_mask: ip_number}
    | Router_Advertisement of
         {lifetime: FoxWord16.word,
	  addresses: {address: ip_number,
		      preference_level: FoxWord32.word} list}
    | Router_Solicitation
    | Obsolete				(* used for Information messages *)

  include EXTERNAL
   sharing type T = icmp_in

  val unmarshal: Data.T -> T
 end

(*
	4.	signature ICMP_OUTGOING
*)

signature ICMP_OUTGOING =
 sig
  type ip_number
  type unreachable

  structure Data: EXTERNAL

  datatype icmp_message =
      Unreachable of unreachable * Data.T
    | Transit_Time_Exceeded of Data.T
    | Reassembly_Time_Exceeded of Data.T
    | Parameter_Problem of {pointer: FoxWord8.word, data: Data.T}
    | Missing_Required_Option of Data.T
    | Source_Quench of Data.T
    | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: Data.T}
    | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     data: Data.T}
    | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word}
    | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			   originate: FoxWord32.word, receive: FoxWord32.word}
    | Traceroute of {forwarded: bool, id: FoxWord16.word,
		     out_hops: FoxWord16.word, return_hops: FoxWord16.word,
		     out_speed: FoxWord32.word, out_mtu: FoxWord32.word}
    | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
    | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		     address_mask: ip_number}
    | Router_Advertisement of
         {lifetime: FoxWord16.word,
	  addresses: {address: ip_number,
		      preference_level: FoxWord32.word} list}
    | Router_Solicitation
  include EXTERNAL
   sharing type T = icmp_message

  val marshal: T -> Data.T
 end
