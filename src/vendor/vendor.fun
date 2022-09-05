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

	A trivial functor that shoe-horns pervasives into a structure
	of type VENDOR.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Fox_Print: VENDOR_PRINT
	2.	functor V: VENDOR
	3.	functor Vendor_List
	4.	structure Vendor_Char
	5.	structure Vendor_String
	6.	structure Vendor_Time
	7.	structure Vendor_Misc
	8.	 functor Control
	9.	fun operating_system
	10.	structure Vendor_IO

		iii.	RCS Log
	
$Log: vendor.fun,v $
Revision 1.22  1997/11/14  11:37:38  cline
adapted for use with SMLNJ 109.32 basis

Revision 1.21  97/03/21  18:22:29  cline
open structure Cont

Revision 1.20  97/03/10  18:50:19  esb
fixed to correspond to new vendor.sig.

Revision 1.19  96/05/14  01:20:51  esb
fixed toMilli/Microseconds to return values in the appropriate range.

Revision 1.18  1996/05/08  14:40:33  cline
remover * from Time

Revision 1.17  1996/04/18  18:55:23  cline
Changed Vendor_Time

Revision 1.16  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.15  1996/03/11  14:39:21  cline
added operating_system identification

Revision 1.14  1996/02/23  19:53:20  cline
added IO, modified String and Char

Revision 1.13  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.12  1995/10/23  18:43:16  cstone
Added member to Vendor_List

Revision 1.11  1995/09/17  22:04:20  esb
removed getuniversal.

Revision 1.10  1995/09/07  15:00:49  cline
updated for use with 108.9

Revision 1.9  1995/08/08  18:16:48  esb
added getuniversal time.

Revision 1.8  1995/06/20  16:42:00  esb
major rearrangement.

Revision 1.7  1995/03/09  22:42:48  esb
reorganized and cleaned up.

Revision 1.6  1995/03/06  18:48:14  esb
new joint release.


	1.	functor Fox_Print: VENDOR_PRINT

	Thin SML/NJ's structures, and use String.print as the default printer.
*)

functor Fox_Print (val string_length: string -> int
		   val string_substring: string * int * int -> string
		   val print: string -> unit
		   val flush: unit -> unit): VENDOR_PRINT =
 struct
  exception Print_Byte

  fun exported_print s = (print s; flush ())

  val print = exported_print
  val flush = flush

  fun print_byte1f3 byte =
       app print [if byte <= 0wx0f then "0" else "", Word8.toString byte]

  fun print_byte1Uarray array =
       let val length = Word8Array.length array
           fun loop n =
	    if n = length then ()
	    else (print_byte1f3 (Word8Array.sub (array, n));
	          print ".";
	          loop (n + 1))
       in loop 0
       end

 end (* struct *)

(*
	2.	functor V: VENDOR
*)


functor V (): VENDOR =
 struct
  local

(*
	3.	functor Vendor_List
*)

   functor Vendor_List (): VENDOR_LIST =
    struct
     val map = List.map
     val length = List.length
     val reverse = List.rev
     val app = List.app
     val nth = List.nth
     fun member xs y =
       let
	 fun loop [] = false
	   | loop (x::xs) = (x = y) orelse loop xs
       in
	 loop xs
       end
     fun fold f list init = List.foldr f init list
     fun revfold f list init = List.foldl f init list
     fun revapp f list = List.app f (List.rev list)
    end

(*
	4.	structure Vendor_Char
*)

   structure Vendor_Char: VENDOR_CHAR =
    struct
     open Char

     val lower_to_upper_delta = ord #"a" - ord #"A"

     fun to_upper char =
          if char >= #"a" andalso char <= #"z" then
	   chr (ord char - lower_to_upper_delta)
	  else char

     fun to_lower char =
          if char >= #"A" andalso char <= #"Z" then
	   chr (ord char + lower_to_upper_delta)
	  else char

     fun caseless_equal (a, b) = to_lower a = to_lower b
     fun caseless_less (a, b) = to_lower a < to_lower b
     fun caseless_greater (a, b) = to_lower a > to_lower b
    end

(*
	5.	structure Vendor_String
*)

   structure Vendor_String: VENDOR_STRING =
    struct
     open String
     val length = size
     val ordof = sub

     fun index (ch, str, start) =
          if Int.>= (start, String.size str) then NONE
	  else
	   let fun loop ([], _) = NONE
		 | loop (first :: rest, skip_count) =
	            if Int.< (skip_count, 0) orelse ch <> first then
		     loop (rest, skip_count + 1)
		    else
		     SOME (start + skip_count)
	   in loop (String.explode str, ~start)
	   end

     exception Bad_Integer_String of string

     fun string_to_int string =
          let fun stoi ([], i) = i
		| stoi (#"0" :: r, i) = stoi (r, 10*i+0)
		| stoi (#"1" :: r, i) = stoi (r, 10*i+1)
		| stoi (#"2" :: r, i) = stoi (r, 10*i+2)
		| stoi (#"3" :: r, i) = stoi (r, 10*i+3)
		| stoi (#"4" :: r, i) = stoi (r, 10*i+4)
		| stoi (#"5" :: r, i) = stoi (r, 10*i+5)
		| stoi (#"6" :: r, i) = stoi (r, 10*i+6)
		| stoi (#"7" :: r, i) = stoi (r, 10*i+7)
		| stoi (#"8" :: r, i) = stoi (r, 10*i+8)
		| stoi (#"9" :: r, i) = stoi (r, 10*i+9)
		| stoi _ = raise (Bad_Integer_String string)
	      val es = explode string
	      fun sign (#"~" :: []) = raise (Bad_Integer_String string)
		| sign (#"~" :: r) = ~(stoi (r, 0))
		| sign ow = stoi (ow, 0)
	  in if Integer.> (List.length es, 0) then sign es
	     else raise (Bad_Integer_String string)
	  end

     fun to_upper string = map Vendor_Char.to_upper string
     fun to_lower string = map Vendor_Char.to_lower string
     fun caseless_equal (a, b) = to_lower a = to_lower b

    end (* structure Vendor_String *)

(*
	6.	structure Vendor_Time
*)

   functor Vendor_Time (): VENDOR_TIME =
    struct

     local
       (* here is the conversion of seconds to daytime GMT (time-zone info
	  is not easily available). *)

       fun leap_day n =
	 if (n mod 4 = 0) andalso ((n mod 100 <> 0) orelse (n mod 400 = 0))
	 then 1 else 0

       fun year_days n = 365 + leap_day n

       fun month_days (_, 1) = 31
	 | month_days (year, 2) = 28 + leap_day year
	 | month_days (_, 3) = 31
	 | month_days (_, 4) = 30
	 | month_days (_, 5) = 31
	 | month_days (_, 6) = 30
	 | month_days (_, 7) = 31
	 | month_days (_, 8) = 31
	 | month_days (_, 9) = 30
	 | month_days (_,10) = 31
	 | month_days (_,11) = 30
	 | month_days (_, _) = 31

       fun compute_month (year, days) =
	 let fun loop (month, days_left) =
	   if days_left < month_days (year, month) then
	     (month, days_left + 1)
	   else loop (month + 1, days_left - month_days (year, month))
	 in loop (1, days)
	 end

       fun compute_year days =
	 let fun loop (year, days_left) =
	   if days_left <= year_days year then (year, days_left)
	   else loop (year + 1, days_left - year_days year)
	 in loop (1970, days)
	 end

       fun month_name 1 = "Jan"
	 | month_name 2 = "Feb"
	 | month_name 3 = "Mar"
	 | month_name 4 = "Apr"
	 | month_name 5 = "May"
	 | month_name 6 = "Jun"
	 | month_name 7 = "Jul"
	 | month_name 8 = "Aug"
	 | month_name 9 = "Sep"
	 | month_name 10 = "Oct"
	 | month_name 11 = "Nov"
	 | month_name _ = "Dec"

       fun weekday_name 0 = "Sun"
	 | weekday_name 1 = "Mon"
	 | weekday_name 2 = "Tue"
	 | weekday_name 3 = "Wed"
	 | weekday_name 4 = "Thu"
	 | weekday_name 5 = "Fri"
	 | weekday_name _ = "Sat"

       fun two_digit n = if n < 10 then "0" ^ Integer.toString n
			 else Integer.toString n

     in
       fun split t =
	 let val total_seconds = Integer.fromLarge (Time.toSeconds t)
	     val total_minutes = total_seconds div 60
	     val total_hours = total_minutes div 60
	     val total_days = total_hours div 24
	     val (years, days_in_year) = compute_year total_days
	     val (months, days) = compute_month (years, days_in_year)
	     val hours = total_hours - (total_days * 24)
	     val minutes = total_minutes - (total_hours * 60)
	     val seconds = total_seconds - (total_minutes * 60)
	     val weekdays = (total_days + 4) mod 7 (* sunday is weekday 0 *)
	     val microseconds =
	         Integer.fromLarge
	           (Time.toMicroseconds
		      (Time.- (t, Time.fromSeconds (Time.toSeconds t))))
	 in {years  = years,
	     months = months,
	     days   = days,
	     weekdays = weekdays,
	     hours    = hours,
	     minutes  = minutes,
	     seconds  = seconds,
	     microseconds = microseconds}
	 end

       fun toDate t = case split t of
	 {years,months,days,weekdays,hours,minutes,seconds,microseconds} =>
	   weekday_name weekdays ^ ", " ^
	   two_digit days ^ " " ^
	   month_name months ^ " " ^
	   Integer.toString years ^ " " ^
	   two_digit hours ^ ":" ^
	   two_digit minutes ^ ":" ^
	   two_digit seconds ^ " GMT"
     end

     open Time
     (* adapt to SML/NJ types since foxnet doesn't use Int32 *)
     val toSeconds = Integer.fromLarge o toSeconds
     val fromSeconds = fromSeconds o Integer.toLarge
     val fromMilliseconds = fromMilliseconds o Integer.toLarge
     val fromMicroseconds = fromMicroseconds o Integer.toLarge

     fun addms (time, ms) =
          Time.+ (time, fromMilliseconds ms)

     val deltams = Integer.fromLarge o toMilliseconds o Time.-

     val makestring = toString

     fun op= (a, b: Time.time) = a = b

(* the built-in raises Overflow for most times;  The more reasonable
   value to return is in the range 0..999 for ms or 0..999,999 for us *)
     local
      fun fractional_time time =
	  Time.- (time, (Time.fromSeconds (Time.toSeconds time)))

     in
      val toMilliseconds =
	  Integer.fromLarge o Time.toMilliseconds o fractional_time

      val toMicroseconds =
	  Integer.fromLarge o Time.toMicroseconds o fractional_time
     end

  end (* functor Vendor_Time *)

(*
	7.	structure Vendor_Misc
*)

   functor Vendor_Misc (): VENDOR_MISC =
    struct

     exception Bad_Size

     fun create_uninitialized size =
          if size > 0 then Unsafe.Word8Array.create size
	  else if size = 0 then Word8Array.array (0, 0w0)
	  else raise Bad_Size

     val environment = Posix.ProcEnv.environ

    end (* functor Vendor_Misc *)

(*
	8.	 functor Control
*)

     functor Control () =
       struct
	 open SMLofNJ
	 open Cont
	 open General
       end

(*
	9.	fun operating_system
*)

   fun operating_system () =
     let exception Operating_System
         fun loop [] = raise Operating_System
	   | loop (("sysname",os)::_) = os
	   | loop (_::rest) = loop rest
     in
       loop (Posix.ProcEnv.uname ())
     end

(*
	10.	structure Vendor_IO
*)

structure Vendor_IO =
  struct
    type instream	= TextIO.instream
    type outstream	= TextIO.outstream
    val std_in	= TextIO.stdIn
    val std_out	= TextIO.stdOut
    val std_err	= TextIO.stdErr
    val open_in	= TextIO.openIn
    val open_out	= TextIO.openOut
    val open_append	= TextIO.openAppend
    val close_in	= TextIO.closeIn
    val close_out	= TextIO.closeOut
    val output	= TextIO.output
    fun outputc stream string = output (stream, string)
    val input		= TextIO.inputN
    fun inputc stream n = input (stream, n)
    val input_line	= TextIO.inputLine
    fun lookahead stream = (case TextIO.lookahead stream of
			      SOME c => Char.toString c
			    | NONE => "")
    val end_of_stream	= TextIO.endOfStream
    val flush_out	= TextIO.flushOut
  end

  in (* local *)

   structure Char: VENDOR_CHAR = Vendor_Char
   structure String: VENDOR_STRING = Vendor_String
   structure List: VENDOR_LIST = Vendor_List ()
   structure Array: VENDOR_ARRAY = Array
   structure Control: VENDOR_CONTROL = Control ()
   structure Print: VENDOR_PRINT =
        Fox_Print (val string_length = String.length
		   val string_substring = String.substring
		   val print = (print:string->unit)
		   val flush = fn () => ())
   structure Time: VENDOR_TIME = Vendor_Time ()
   structure Misc: VENDOR_MISC = Vendor_Misc ()
   structure IO: VENDOR_IO = Vendor_IO
   val operating_system = operating_system ()
  end (* local *)
 end  (* functor V *)
