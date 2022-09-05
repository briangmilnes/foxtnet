(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	externkey.sig: signature for a class of externs that are also keys.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PRINTABLE
	2.	signature KEY
	3.	signature EXTERN_KEY
	4.	signature EXTERNAL

	iii.	RCS Log

$Log: externkey.sig,v $
Revision 1.3  1996/04/18  18:46:26  cline
changed hash from T->int to T->word

Revision 1.2  1996/01/19  23:10:02  esb
adapted to the new wordarray signature.

Revision 1.1  1995/06/20  17:43:24  esb
Initial revision


	1.	signature PRINTABLE
*)

signature PRINTABLE =
 sig
  type T
  val makestring: T -> string
 end (* sig *)

(*
	2.	signature KEY
*)

signature KEY =
 sig
  include PRINTABLE
  val equal: T * T -> bool
  val hash: T -> word
 end (* sig *)

(*
	3.	signature EXTERN_KEY

	Any structure that matches this signature will also match
	signature KEY and signature EXTERN.
*)

signature EXTERN_KEY =
 sig
  include EXTERN
  val equal: T * T -> bool
  val hash: T -> word
  val makestring: T -> string
 end (* sig *)

(*
	4.	signature EXTERNAL

	for a PROTOCOL, the EXTERNAL type used to encode data sent and
	received must be addressable and have a representation as a
	word array.
*)

signature EXTERNAL =
 sig
  include PRINTABLE
  val new: Word_Array.T -> T
  val uninitialized: Word.word -> T
  val size: T -> Word.word
  val sub: T * {start: Word.word, length: Word.word} -> Word_Array.T
  val update: T * Word.word * Word_Array.T -> unit
  val join: T * T -> T
  val split: T * Word.word -> T * T
  val fold: T * (Word_Array.T * 'a -> 'a) * 'a -> 'a (* front to back *)
  val makestring_max: T * Word.word -> string
 end (* sig *)
