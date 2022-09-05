(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline    (Ken.Cline@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	packetfilter.sig: an interface to the OSF1 packetfilter pseudo-device.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PACKET_FILTER

*)

(*

		iii.	RCS Log

$Log: packetfilter.sig,v $
Revision 1.5  1997/03/27  13:47:15  esb
removed select, added type T to the signature.

Revision 1.4  96/03/04  20:48:30  derby
Added writev to the packetfilter.

Revision 1.3  1995/09/18  19:29:17  esb
first running version.

Revision 1.2  1995/01/14  02:32:01  esb
renamed signature to accord with naming conventions.

Revision 1.1  1994/10/20  17:57:49  cline
Initial revision


*)

(*
	1.	signature PACKET_FILTER
*)

signature PACKET_FILTER =
 sig
  type T = Posix.FileSys.file_desc
  type filter

  val pfopen:			string -> T	(* interface name *)
  val close:			T -> unit
  val get_ethernet_address:	T -> Word_Array.T
  val set_filter:		T * filter -> unit
  val readi:			T * Word_Array.T * int * int -> int
(* select for read with int millisecond timeout *)
(* Returns false if it times out, true if the file descriptor is ready. *)
  val write:			T * Word_Array.T * int -> unit
  val writev: T * Word_Array.T list -> int
  exception Packet_Filter
 end
