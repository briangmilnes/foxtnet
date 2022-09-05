(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A signature to interface to the SML vendor's added functionality.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature VENDOR_CHAR
	2.	signature VENDOR_STRING
	3.	signature VENDOR_LIST
	4.	signature VENDOR_ARRAY
	5.	signature VENDOR_CONTROL
	6.	signature VENDOR_PRINT
	7.	signature VENDOR_TIME
	8.	signature VENDOR_MISC
	9.	signature VENDOR_IO

	

		iii.	RCS Log
	
$Log: vendor.sig,v $
Revision 1.32  1997/11/14  11:38:23  cline
*** empty log message ***

Revision 1.31  1997/03/10  18:49:58  esb
changed the type of "index" to reflect its function.

Revision 1.30  97/02/13  00:31:57  esb
eliminated imperative types

Revision 1.29  1996/05/08  14:40:59  cline
removed timeToReal, realToTime, and * from Time

Revision 1.28  1996/04/18  18:54:47  cline
changed TIME signature

Revision 1.27  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.26  1996/03/11  14:39:59  cline
added operating_system identification and string comparison operators

Revision 1.25  1996/02/23  19:52:59  cline
added IO, modified String and Char

Revision 1.24  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.23  1995/10/23  18:43:02  cstone
Added member to VENDOR_LIST

Revision 1.22  1995/09/17  22:04:20  esb
removed getuniversal.

Revision 1.21  1995/08/08  18:16:48  esb
added getuniversal time.

Revision 1.20  1995/06/20  16:42:00  esb
major rearrangement.

Revision 1.19  1995/03/09  22:42:37  esb
reorganized and cleaned up.

Revision 1.18  1995/02/04  21:46:40  robby
updated to 107

Revision 1.17  1994/11/07  21:25:09  cline
expanded VENDOR_STRING

Revision 1.16  1994/10/19  22:56:54  esb
added environment.

Revision 1.15  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.14  1994/10/06  15:39:14  robby
added update to the array functor

Revision 1.13  94/09/30  16:16:46  esb
upgraded to work with 1.05 as well as 0.93

Revision 1.12  1994/07/04  19:06:19  esb
checked in Brian's changes.

Revision 1.11  1994/06/05  18:40:21  milnes
Added a stdio flush.

Revision 1.10  1994/05/23  13:57:21  milnes
Added some string functions to help translate strings to integers
to ease command line processing for ping.

Revision 1.9  1994/05/04  01:35:19  esb
Brian added get_universal_time.

Revision 1.8  94/04/27  15:49:21  esb
added deltams.

Revision 1.7  94/02/17  02:58:54  milnes
Timing board and related changes.

Revision 1.6  1994/01/13  16:20:14  milnes
On path to updating coroutines, added some more timing stuff to Vendor.misc.

Revision 1.5  94/01/13  12:44:40  milnes
No changes.

Revision 1.4  94/01/11  15:25:21  milnes
Added earlier to VENDOR_MISC so that we don't rewrite it.

Revision 1.3  94/01/11  20:14:29  cline
added function index to VENDOR_STRING

Revision 1.2  1993/10/25  19:31:11  cline
removed .U from Byte[421].U

Revision 1.1  1993/06/10  22:33:19  milnes
Initial revision


*)

(*
		1.	signature VENDOR_CHAR
*)

signature VENDOR_CHAR =
  sig
    eqtype char = char
    val chr: int -> char
    val ord: char -> int
    val maxOrd: int
    val to_lower: char -> char
    val to_upper: char -> char
    val < : char * char -> bool
    val <= : char * char -> bool
    val > : char * char -> bool
    val >= : char * char -> bool
    val caseless_equal: char * char -> bool
    val caseless_less: char * char -> bool
    val caseless_greater: char * char -> bool
    val isSpace: char -> bool
  end

(*
		2.	signature VENDOR_STRING
*)

signature VENDOR_STRING =
  sig
    eqtype string = string
    val length: string -> int
    val ordof: string * int -> char
    val substring: string * int * int -> string
    val concat: string list -> string
    val ^ : string * string -> string
    val str: char -> string
    val implode: char list -> string
    val explode: string -> char list

    val to_lower: string -> string
    val to_upper: string -> string
    val caseless_equal: string * string -> bool

    val index: char * string * int -> int option
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    exception Bad_Integer_String of string
    val string_to_int: string -> int

    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool
  end

(*
		3.	signature VENDOR_LIST
*)

signature VENDOR_LIST = 
 sig
  val fold: ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
  val revfold: ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
  val length: 'a list -> int
  val reverse: 'a list -> 'a list
  val map: ('a -> 'b) -> 'a list -> 'b list
  val app: ('a -> unit) -> 'a list -> unit
  val revapp: ('a -> unit) -> 'a list -> unit
  val nth: 'a list * int -> 'a
  val member: ''a list -> ''a -> bool
 end (* sig *)

(*
		4.	signature VENDOR_ARRAY
*)

signature VENDOR_ARRAY =
 sig
  type 'a array
  val tabulate: int * (int -> 'a) -> 'a array
  val sub: 'a array * int -> 'a
  val length: 'a array -> int
  val update: 'a array * int * 'a -> unit
 end (* sig *)

(*
		5.	signature VENDOR_CONTROL
*)

signature VENDOR_CONTROL =
 sig
  type 'a cont
  val callcc: ('a cont -> 'a) -> 'a
  val throw: 'a cont -> 'a -> 'b
  val exnName: exn -> string
 end (* sig *)

(*
		6.	signature VENDOR_PRINT
*)

signature VENDOR_PRINT =
 sig
  val print: string -> unit
  val flush: unit -> unit 
  val print_byte1f3: Word8.word -> unit
  val print_byte1Uarray: Word8Array.array -> unit
 end (* sig *)

(*
		7.	signature VENDOR_TIME
*)

signature VENDOR_TIME =
 sig
  type time
  val now: unit -> time
  val toSeconds: time -> int		(* range 0...oo *)
  val fromSeconds: int -> time
  val toMilliseconds: time -> int	(* range 0...999 *)
  val fromMilliseconds: int -> time
  val toMicroseconds: time -> int	(* range 0...999,999 *)
  val fromMicroseconds: int -> time

  val split: time -> {years: int,
		      months: int,
		      days: int,
		      weekdays: int,
		      hours : int,
		      minutes: int,
		      seconds: int,
		      microseconds: int}

  val - : time * time -> time
  val + : time * time -> time

  val addms: time * int -> time
  val deltams: time * time -> int

  val < : time * time -> bool
  val > : time * time -> bool
  val = : time * time -> bool

  val toString: time -> string
  val toDate: time -> string

  val zeroTime : time
 end (* sig *)

(*
		8.	signature VENDOR_MISC
*)

signature VENDOR_MISC =
 sig
  exception Bad_Size
  val create_uninitialized: int -> Word8Array.array (* may raise Bad_Size *)
  val environment: unit -> string list
 end (* sig *)


(*
		9.	signature VENDOR_IO
*)

signature VENDOR_IO =
 sig
   type instream
   type outstream
   val std_in : instream
   val std_out : outstream
   val std_err : outstream
   val open_in : string -> instream
   val open_out : string -> outstream
   val open_append : string -> outstream
   val close_in : instream -> unit
   val close_out : outstream -> unit
   val output : outstream * string -> unit
   val outputc : outstream -> string -> unit
   val input : instream * int -> string
   val inputc : instream -> int -> string
   val input_line : instream -> string
   val lookahead : instream -> string
   val end_of_stream : instream -> bool
   val flush_out : outstream -> unit
 end (* sig *)

(*
                8.      signature VENDOR
*)

signature VENDOR =
 sig
  structure Char: VENDOR_CHAR
  structure String: VENDOR_STRING
  structure List: VENDOR_LIST
  structure Array: VENDOR_ARRAY
  structure Control: VENDOR_CONTROL
  structure Print: VENDOR_PRINT
  structure Time: VENDOR_TIME
  structure Misc: VENDOR_MISC
  structure IO: VENDOR_IO
  val operating_system: string
 end (* sig *)

