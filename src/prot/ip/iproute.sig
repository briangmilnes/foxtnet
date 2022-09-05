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

	iproute.sig: interface to a stateless module for IP routing.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_ROUTE

		iii.	RCS Log
	
$Log: iproute.sig,v $
Revision 1.8  1996/02/23  21:13:31  esb
added makestring and has_default_gateway.

Revision 1.7  1995/09/26  16:24:38  esb
added function to support ICMP router info message.

Revision 1.6  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.5  1995/08/24  00:51:04  esb
added some support for IP.

Revision 1.4  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.3  1995/06/20  17:02:17  esb
adapted to new protocol signature.

Revision 1.2  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.1  1994/06/15  20:45:56  milnes
Initial revision

		1.	signature IP_ROUTE

	Note that gateways MUST be on attached networks (RFC 1122, p. 48)
*)

signature IP_ROUTE =
 sig

  type ip_number
  type preference
  type interface
  type T			(* explicit state, managed by caller *)

  val makestring: T -> string

  exception Unknown_Class of ip_number

  val new: {interface: string, address: ip_number,
	    gateways: ip_number list, mask: ip_number option} list -> T

  val interface_for_address: T * ip_number -> string option
  val address_for_interface: T * string -> ip_number option

  val set_interface_address: T * string * ip_number -> T
  val disable_interface: T * string -> T
  val set_interface_mask: T * string * ip_number -> T
  val unset_interface_mask: T * string -> T


  exception Gateway_Not_Connected of ip_number
  val add_default_gateway: T * ip_number -> T
  val remove_default_gateway: T * ip_number -> T
  val has_default_gateway: T * string -> bool
  val add_preference_gateway: T * ip_number * preference * int (* ttl *) -> T
  val add_specific_gateway:
        T * {destination: ip_number, gateway: ip_number} -> T
  val remove_specific_gateway: T * {destination: ip_number} -> T

  datatype resolution = Unicast of {next_hop: ip_number}
                      | Broadcast
                      | Loopback

  val resolve: T * ip_number -> {interface: interface,
				 interface_ip: ip_number,
				 next_hop: resolution} option

  val valid_incoming: T * interface * ip_number -> bool

  val is_unicast_address: T * ip_number -> bool

  val gc: T -> T			(* call once a second *)

 end (* sig *)
