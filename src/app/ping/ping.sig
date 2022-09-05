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

	The signature of a ping operation, similar to typical unix pings.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

		iii.	RCS Log
	
$Log: ping.sig,v $
Revision 1.6  1997/06/04  11:59:42  esb
added serve_forever.

Revision 1.5  96/03/01  19:37:09  esb
new implementation, for Foxnet V2

Revision 1.4  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.3  1994/05/23  18:08:47  milnes
Updated to use icmp.

Revision 1.2  1994/05/23  14:06:04  milnes
Added executable saving.

Revision 1.1  1994/05/04  19:14:18  milnes
Initial revision

		1.     signature PING
*)

signature PING =
 sig
  val ping: string * int -> unit
  val traceroute: string -> unit	(* uses TTL *)
  val traceroute_icmp: string -> unit	(* uses ICMP traceroute *)

  val complete_ping: {host: string, count: int, size: int option,
		      quiet: bool, interval: int (* ms *) option,
		      record_route: bool} -> unit

  val serve: {seconds: int} -> unit
  val serve_forever: unit -> unit

  val make_ping_executable : string -> unit
 end


