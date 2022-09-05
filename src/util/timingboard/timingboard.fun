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
	
	A functor for the timingboard, it is just a 4 byte array that
 is read with Byte4.sub.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor TimingBoard

		iii.	RCS Log
	
$Log: timingboard.fun,v $
Revision 1.10  1994/09/30  16:35:30  esb
gutted the entire module.

Revision 1.9  1994/08/24  22:33:52  esb
introduced local_print, time-per-call output

Revision 1.8  1994/04/26  17:52:28  esb
added a count of times each counter is started.

Revision 1.7  94/04/20  19:48:23  milnes
Printer changes.

Revision 1.6  1994/03/04  02:28:32  milnes
Reduced the number of spaces when printing a timer.

Revision 1.5  1994/02/25  18:21:18  milnes
Simplified timingboard interface.

Revision 1.4  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file
move problems that confused rcs.


		1.	functor TimingBoard
*)

functor TimingBoard (val try_to_install: bool
                     val do_prints: bool): TIMINGBOARD =
 struct

(*
  fun local_print s = print ("timingboard.fun: " ^ s ^ "\n")

  val installed = ref false
  val array = ref (ByteArray.array(4,0))
  val safe = true
  val sub = if safe then FoxWord32.sub else Byte4.Unsafe_U.sub
  val update = if safe then FoxWord32.update else Byte4.Unsafe_U.update
  val plus = if safe then FoxWord32.+ else Byte4.Unsafe_U.+
  val minus = if safe then FoxWord32.- else Byte4.Unsafe_U.-

  val fetch: unit -> ByteArray.bytearray =
         System.Unsafe.CInterface.c_function "fetch_tb"

  val zero_tb_gc: unit -> unit =
         System.Unsafe.CInterface.c_function "zero_tb_gc"
  val mkstrngs_tb: unit -> string list =
         System.Unsafe.CInterface.c_function "mkstrngs_tb"

  fun initialize () =
       let val address = Byte4.Unsafe_U.sub (fetch (), 0)
       in installed := (if try_to_install andalso address <> 4u0 then true
			else false);
	  if not (! installed) then  (* This should be updated. *)
	   let val new = ByteArray.array(4,0)
	   in array := new
	   end (* let *)
	  else
	   let val new = ByteArray.array(4,0)
	   in if do_prints then
	       local_print ("address = " ^ Byte4.makestring address)
	      else ();
	       Byte4.update(new,0,address);
	       array := (! (System.Unsafe.cast new: ByteArray.bytearray ref))
	   end (* let *)
       end (* let *)

  val _ = initialize () (* Auto initialize upon functor application. *)

  type counter = string * Byte4.bytearray * ubyte4 ref

  fun name    (s, _, _) = s		(* Return the string name. *)
  fun sum     (_, b, _) = sub (b, 0)	(* This is the sum to date. *)
  fun current (_, b, _) = sub (b, 4)	(* This is the last count stored. *)
  fun running (_, b, _) = (4ux0 <> sub (b, 4)) (* if not 0, timer running. *)
  fun count   (_, _, c) = ! c	        (* Number of times started. *)

  val counters = ref ([]: counter list)	(* A list of counters. *)

  fun find (x, []) = false
    | find (x, (y, b, c) :: l) = if x = y then true else find (x, l)

  exception Extract of string

  fun extract (x, []) = raise Extract x
    | extract (x, (y, b, c) :: l) = if x = y then (y, b, c)
				    else extract (x, l)

  fun zero_counter (s, b, c) =
       (update (b, 0, 4ux0);
	update (b, 4, 4ux0);
	c := 4ux0)

  fun zero_counters () = (app zero_counter (! counters); zero_tb_gc ())

  fun add s =
       if find (s, ! counters) then
	let val counter = extract (s, ! counters)
	in zero_counter counter;
	   counter
	end
       else
	let val counter = (s, ByteArray.array (8, 0), ref 4ux0)
	in counters := (counter :: ! counters);
	   counter
	end

  exception Counter_Not_Found of string

  fun get s =
       if find (s, ! counters) then extract (s, ! counters)
       else raise (Counter_Not_Found s)

  fun start (s, b, c) =
       (update (b, 4, Byte4.Unsafe_U.sub (! array, 0));
	c := Byte4.+ (! c, 4u1))

  fun stop (s, b, c) =
       let val stop  = Byte4.Unsafe_U.sub (! array, 0)
	   val current = sub (b, 0)
	   val start = sub (b, 4)
       in if start > stop then
           update (b, 0, plus (current, plus (minus (4uxFFFFFFFF, start),
					      stop)))
	  else update (b, 0, plus (current, minus (stop, start)));
	  update (b, 4, 4ux0)
       end

  fun elapsed_time_on_running_counter (s, b, c) =
       let val stop  = Byte4.Unsafe_U.sub (! array, 0)
	   val start = sub (b, 4)
       in if start > stop then
           plus (minus (4uxFFFFFFFF, start), stop)
	  else minus (stop, start)
       end

  fun zero_or_restart_counter counter =
       if running counter then (zero_counter counter; start counter)
       else zero_counter counter

  fun zero_or_restart_counters () = app zero_or_restart_counter (! counters)

  fun reset () = counters := []

  val fifty_spaces = "                                                  "

  fun blank_string n = String.substring (fifty_spaces, 0, n)

  (* These are defined here to allow timingboard to be loaded before
     the utilities, as it is used to time the utilties.*)

  fun extend (s, l) =
       if String.length s >= l then s
       else blank_string (l - String.length s) ^ s

  fun makestring_int (i, string_length) =
       extend (Integer.makestring i, string_length)

  fun makestring_rounded (f, decimal_place_to_round_at, string_length) =
       let fun exp 0 = 1.0
	     | exp n = 10.0 * exp (n - 1)
	   val power = exp decimal_place_to_round_at
	   val result = Real./ (real (truncate (f * power + 0.5)), power)
       in extend (Real.makestring result, string_length)
       end

  fun makestring_counter_percent (max_counter, (s, b, c)) =
       let val error = ref (NONE: (string * exn) option)
	   val number = sub (b, 0)
	    (* this function avoids integer overflow by shifting down
	       the byte4 and adding in the low-order part afterwards. *)
	   fun real_byte4 n =
	        (real (Byte4.to_int (Byte4.>> (n, 4))) * 16.0 +
		 real (Byte4.to_int (Byte4.&& (n, 4uxf))))
           val add_in_running = if running (s, b, c) then
	                         elapsed_time_on_running_counter (s, b, c)
				else 4u0
	   val number_plus = Byte4.+ (number, add_in_running)
	   val number_string = extend (Byte4.makestring number_plus,
				       32 - String.length s)
	   val percentage = if max_counter = 4u0 then 0.0
			    else Real./ ((real_byte4 number_plus) * 100.0,
					 real_byte4 max_counter)
	   val percent_string = (if max_counter <> 4u0 then
			          "   " ^
				  makestring_rounded (percentage, 1, 5) ^ "%"
				 else "")
	                        handle x => (error := SOME ("p2", x); "0%")
	   val count = ! c
	   val count_string = (makestring_int (Byte4.to_int count, 10))
	                      handle x => (error := SOME ("count", x); "0")
            (* there are 12.5 ticks per microseconds. We multiply the
               count by 25, then divide the result by two. *)
	   val count_ticks = Byte4.div (Byte4.* (4u25, count), 4u2)
	   val usec = if count_ticks = 4u0 then 4u0
		      else Byte4.div (number_plus, count_ticks)
	   val usec_string = (makestring_int (Byte4.to_int usec, 10))
	                      handle x => (error := SOME ("usec", x); "0")
	   val error_if_any = if running (s, b, c) then
	                       "TimingBoard: error, timer " ^ s ^
			       " still running, added in " ^
			       Byte4.makestring add_in_running ^ ".\n"
			      else case ! error of
			         NONE => ""
			       | SOME (s, x) =>
				  "exception " ^ System.exn_name x ^
				  " in " ^ s ^ ".\n"
       in s ^ number_string ^ percent_string ^ count_string ^ usec_string ^
	  "\n" ^ error_if_any
       end

  fun makestring_counter counter = makestring_counter_percent (4u0, counter)

  fun read_counter maximal_counter (c as (_, b, _)) =
       (sub (b, 0),
	makestring_counter_percent (maximal_counter, c))

  local
   fun firstn (0, _) = []
     | firstn (_, []) = []
     | firstn (n, a :: b ) = a :: firstn (n - 1, b)

   fun split l =
        let val length = List.length l
	in if length mod 2 = 0 then
            (firstn (length div 2, l), nthtail (l, length div 2))
	   else
	    (firstn (length div 2, l), nthtail (l, length div 2))
	end

  in (* local *)
   fun mergesort p l =
        let fun merge ([],[]) = []
	      |  merge (a,[])  = a
	      |  merge ([],b)  = b
	      |  merge (a as (af :: ar), b as (bf :: br)) =
		  if p(af,bf) then af :: merge(ar, b)
		  else bf :: merge(a,br)

	    fun sort [] = []
	      | sort [a] = [a]
	      | sort b =
		 let val (l,r) = split b
		 in merge (sort l, sort r)
		 end
	in sort l
	end
  end (* local *)

  fun maximum_counter_value () =
       let fun max_counter (counter, accum) =
	        Byte4.max (sum counter, accum)
       in fold max_counter (! counters) 4u0
       end

  fun makestring_counters () =
       (let val strings =
	         map (read_counter (maximum_counter_value ())) (! counters)
	    fun counter_less ((n1, _), (n2, _)) = Byte4.<= (n1, n2)
	    val list = mergesort counter_less strings
	    fun append (s, r) = s ^ r
	    fun fold_pair ((k, s), r) = s ^ r
        in (List.revfold append (mkstrngs_tb ())
	    (List.revfold fold_pair list ""))
        end)
	 handle x =>
	  (local_print ("exception " ^ System.exn_name x ^
			" in makestring_counters");
	   "exception " ^ System.exn_name x ^
	   " in timingboard.fun:makestring_counters")

  fun time (counter, thunk) =
       if running counter then thunk()
       else
	(start counter;
	 let val return = thunk ()
	 (* If this raises exceptions we may have unseen consequences. *)
	 in stop counter;
	    return
	 end)
*)

(* to be used to measure the approximate effect of profiling.
  fun start _ = ()
  fun stop _ = ()
  fun time (_, thunk) = thunk ()
*)

  fun initialize () = ()

  val installed = ref false

  val array = ref (ByteArray.array (4, 0))

  type counter = string

  fun id x = x
  val name = id
  fun sum counter = FoxWord32.intToWord 0
  val current = sum
  fun running _ = false
  val count = sum

  val add = id

  exception Counter_Not_Found of string 

  val get = id

  fun reset () = ()

  val zero_counters = reset

  val zero_or_restart_counters = reset

  val makestring_counter = id

  fun makestring_counters () = "counters not implemented"

  fun start _ = ()

  val stop = start

  fun time (counter, f) = f ()

 end (* struct *)
