(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	An empty ICMP for IP.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor EmptyIcmp

		iii.	RCS Log
	
$Log: emptyicmp.fun,v $
Revision 1.6  1995/03/24  01:41:39  esb
adapted to new ip.sig

Revision 1.5  1995/01/18  21:00:37  esb
renamed EmptyIcmp to Empty_Icmp.

Revision 1.4  1995/01/17  21:06:51  esb
adapted to new icmp.sig

Revision 1.3  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.2  1994/09/30  16:59:55  esb
changed DLOCS to Dyn_Locs.

Revision 1.1  1994/08/19  18:02:22  milnes
Initial revision


		1.	functor EmptyIcmp
*)

functor Empty_Icmp (structure B: FOX_BASIS): ICMP_PROTOCOL =
 struct

  val not_implemented = "emptyicmp.fun: icmp not implemented."

  type ip_number = FoxWord32.word
  type ip_option = FoxWord8.word
  type ip_data = B.Dyn_Array.T

  datatype icmp_address = Icmp_Address of ip_number

  type icmp_connection = unit
   
  datatype redirect =
      Network_Redirect
    | Host_Redirect
    | Tos_Network_Redirect
    | Tos_Host_Redirect

  fun makestring_redirect Network_Redirect = "network redirect"
    | makestring_redirect Host_Redirect = "Host redirect"
    | makestring_redirect Tos_Network_Redirect = "TOS network redirect"
    | makestring_redirect Tos_Host_Redirect = "TOS host redirect"

  datatype unreachable = 
      Network_Unreachable
    | Host_Unreachable
    | Protocol_Unreachable
    | Port_Unreachable
    | Fragmentation_Needed
    | Source_Route_Failed
    | Network_Unknown
    | Host_Unknown 
    | Source_Host_Isolated
    | Communication_With_Network_Prohibited
    | Communication_With_Host_Prohibited
    | Network_Unreachable_for_Tos
    | Host_Unreachable_for_Tos

  fun makestring_unreachable _ = not_implemented

(*
	2.	internal structure In
*)

  structure In =
   struct
    (* When a parameter problem message arrives, Icmp parses it out
       and represents it as a problem in the IP header, Ip options,
       or the data of the packet. *)
    datatype problem_specifier = 
        Header of FoxWord8.word
      | Option of ip_option
      | Data of FoxWord8.word * ip_data

    fun makestring_problem _ = not_implemented

    datatype icmp_message  =
        Unreachable of unreachable
      | Transit_Time_Exceeded
      | Reassembly_Time_Exceeded
      | Parameter_Problem of problem_specifier
      | Source_Quench 
      | Redirect of redirect
      | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: ip_data}
      | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       data: ip_data}
      | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word,
		       originate: FoxWord32.word}
      | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			     originate: FoxWord32.word,
			     receive: FoxWord32.word,
			     transmit: FoxWord32.word}
      | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
      | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       address_mask: ip_number}

    fun makestring_incoming _ = not_implemented

   end (* struct *)

(*
	3.	internal structure Out
*)

  structure Out =
   struct 
    datatype allocation =
        Unreachable of unreachable * ip_data
      | Reassembly_Time_Exceeded of ip_data
      | Transit_Time_Exceeded of ip_data
      | Parameter_Problem of {pointer: FoxWord8.word, data: ip_data}
      | Source_Quench of ip_data
      | Echo of {id: FoxWord16.word, sequence: FoxWord16.word, data: ip_data}
      | Echo_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       data: ip_data}
      | Time_Stamp of {id: FoxWord16.word, sequence: FoxWord16.word,
		       originate: FoxWord32.word}
      | Time_Stamp_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
			     originate: FoxWord32.word,
			     receive: FoxWord32.word,
			     transmit: FoxWord32.word}
      | Mask_Request of {id: FoxWord16.word, sequence: FoxWord16.word}
      | Mask_Reply of {id: FoxWord16.word, sequence: FoxWord16.word,
		       address_mask: ip_number}

    fun makestring_allocation _ = not_implemented

    type icmp_message = unit
   end

(*
	4.	other objects
*)

  type address = icmp_address
  type connection = icmp_connection
  type incoming = In.icmp_message
  type outgoing = Out.icmp_message
  type status = unit
  type allocation = Out.allocation 

  exception Initialization_Failed of string
  exception Protocol_Finalized 
  exception Connection_Closed of connection * string 
  exception Illegal_Address of string 
  exception Open_Failed of string 
  exception Send_Failed of string

  datatype handler =
      Handler of connection -> ((incoming -> unit) * (status -> unit))

  fun initialize () = raise (Initialization_Failed not_implemented)
  fun finalize () = raise Protocol_Finalized
  fun connect _ = raise (Open_Failed not_implemented)
  fun start_passive _ = raise (Open_Failed not_implemented)
  fun allocate_send _ = raise (Connection_Closed ((), not_implemented))    
  fun close () = raise (Connection_Closed ((), not_implemented))    
  fun abort () = raise (Connection_Closed ((), not_implemented))    
  fun makestring_address _ = not_implemented
  fun makestring_incoming _ = not_implemented
  fun makestring_outgoing _ = not_implemented
  fun makestring_status   _ = not_implemented
  fun hash_address _ = 0
  fun hash_connection () = 0
  fun equal_address _ = false
  fun equal_connection _ = false
  fun serve_mask _ = ()
  fun stop_mask _ = ()
  datatype on_off = On | Off
  fun service _ = Off
  fun set_service _ = ()
  fun no_connection_service _ = Off
  fun set_no_connection_service _ = ()

 end

