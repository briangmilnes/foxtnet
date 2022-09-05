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

	A signature for the entire TCP/IP stack over ethernet.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_IP_ETH_TRACE
	2.	structure TCP_IP_ETH_PROTOCOL_NUMBERS
	3.	structure TCP_IP_ETH

		iii.	RCS Log
	
$Log: tcpipeth.sig,v $
Revision 1.4  1995/03/07  23:52:52  esb
updated tracing.

Revision 1.3  1994/10/20  21:33:24  milnes
No changes.

Revision 1.2  1994/10/19  23:19:26  milnes
added structure Addressing.

Revision 1.1  1994/10/10  18:26:20  milnes
Initial revision


		1.	signature TCP_IP_ETH_TRACE
*)

signature TCP_IP_ETH_TRACE =
 sig
  val stack: int ref option
  val dns: int ref option
  val tcp: int ref option
  val udp: int ref option
  val ip: int ref option
  val icmp: int ref option
  val arp: int ref option
  val eth: int ref option
  val dev: int ref option
 end (* sig *)

(*
		2.	structure TCP_IP_ETH_PROTOCOL_NUMBERS
*)

signature TCP_IP_ETH_PROTOCOL_NUMBERS =
 sig
  (* Ethernet Protocol Numbers *)
   val ip_over_eth: FoxWord16.word
   val arp_over_eth: FoxWord16.word
  (* IP Protocol Numbers *)
   val icmp_over_ip: FoxWord8.word
   val tcp_over_ip: FoxWord8.word
   val udp_over_ip: FoxWord8.word
 end 

(*
		3.	structure TCP_IP_ETH
*)

signature TCP_IP_ETH =
 sig
  structure Numbers: TCP_IP_ETH_PROTOCOL_NUMBERS
    (* Addressing is a module that reads in addressing information
       from the environment, user files, lists or the operating system. *)
  structure Addressing: ADDRESSING

  structure Tcp: TCP_PROTOCOL
  structure Dns: DNS
  structure Udp: UDP_PROTOCOL
  structure Ip: IP_PROTOCOL
  structure Icmp: ICMP_PROTOCOL
  structure Ip_No_Icmp: IP_PROTOCOL
  structure Ip_Mux: IP_MULTIPLEXER
  structure Arp: ADDRESS_RESOLUTION
  structure Eth: ETHERNET_PROTOCOL
  structure Dev: DEVICE_PROTOCOL

  structure Set: TCP_IP_ETH_TRACE

  val initialize: unit -> int
  val finalize: unit -> int

  (* Raised when a call to initialize fails. *)
  exception Initialization_Failed of string

 end



