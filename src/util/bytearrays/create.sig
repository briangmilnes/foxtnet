(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Efficient array creation functions in ML.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature CREATE

		iii.	RCS Log
	
$Log: create.sig,v $
Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.1  1994/07/04  18:16:19  esb
Initial revision


	1.	signature CREATE
*)

signature CREATE =
 sig

  (* raised if the size is negative *)
  exception Illegal_Create of int

  (* create an uninitialized bytearray *)
  val create: int -> Word8Array.array

  (* copy part of an array into a newly created array *)
  val copy_create: Word8Array.array
                 * int (* source *) * int (* number of bytes *)
                 -> Word8Array.array

  (* create a new bytearray filed with values computed by the given fun *)
  val create_fn: int (* size *)
               * (int -> Word8.word)  (* initialization function *)
               -> Word8Array.array

(*
  (* create an uninitialized bytearray whose address will never change. *)
  val create_buffer: int -> Word8Array.array
*)

 end (* sig *)
