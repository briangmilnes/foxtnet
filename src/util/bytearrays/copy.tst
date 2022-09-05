(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test code to test the Copy utility.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Copy
	2.      structure Test_Copy

		iii.	RCS Log
	
$Log: copy.tst,v $
Revision 1.12  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.11  1995/06/20  17:29:44  esb
minor changes.

Revision 1.10  1995/03/12  17:58:32  esb
adapted to new trace.sig.

Revision 1.9  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.8  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.7  1994/08/02  19:31:05  esb
adapted to uncurried test.sig.

Revision 1.6  1994/06/16  16:50:23  danwang
Updated for functorized Fox_Basis

Revision 1.5  1994/05/10  07:42:53  esb
sped up test by making it less thorough (only up to 40-byte arrays).

Revision 1.4  94/03/16  16:30:43  esb
added a massive copy test.

Revision 1.3  94/02/17  01:13:40  esb
set do_prints to false.

Revision 1.2  93/10/29  05:37:13  esb
removed the sharing constraint.

Revision 1.1  1993/10/29  04:46:02  esb
Initial revision


		1.	functor Test_Copy
*)

functor Test_Copy (structure Copy: COPY
		   structure Debug: DEBUG
		   structure V: VENDOR
		   structure Test: TEST
		   val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "copy.tst"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  fun test_copy (src_size, src_pos, bytes, dest_size, dest_pos) =
       let val src = Word8Array.array (src_size, 0w0)
	   val dest = Word8Array.array (dest_size, 0w0)
	   fun inc n = n + 0w1
	   fun print (name, low, wanted, actual) =
	        local_print ("error in copy (" ^
			     Integer.toString src_size ^ ", " ^
			     Integer.toString src_pos ^ ", " ^
			     Integer.toString bytes ^ ", " ^
			     Integer.toString dest_size ^ ", " ^
			     Integer.toString dest_pos ^
			     "), byte at pos " ^
			     Integer.toString low ^ " of " ^
			     name ^ " is " ^
			     Integer.toString (Word8.toInt actual) ^
			     " instead of " ^
			     Integer.toString (Word8.toInt wanted) ^ "\n");
	   fun tabulate (array, low, high, initial_value) =
	        if low >= high then ()
		else
		 (Word8Array.update (array, low, initial_value);
		  tabulate (array, low + 1, high, inc initial_value))
	   fun check (name, array, low, high, initial_value) =
	        if low >= high then true
		else if Word8Array.sub (array, low) <> initial_value then
		 (print (name, low, initial_value, Word8Array.sub (array, low));
		  false)
		else check (name, array, low + 1, high, inc initial_value)
	   val src_start = Word8.fromInt (bytes mod 256)
	   val dest_start = Word8.fromInt ((bytes + 128) mod 256)
       in tabulate (src, 0, src_size, src_start);
	  tabulate (dest, 0, dest_size, dest_start);
	  Copy.copy (src, src_pos, bytes, dest, dest_pos);
	  check ("src", src, 0, src_size, src_start) andalso
	  check ("dest", dest, 0, dest_pos, dest_start) andalso
	  check ("dest", dest, dest_pos, dest_pos + bytes,
		 src_start + Word8.fromInt src_pos) andalso
	  check ("dest", dest, dest_pos + bytes, dest_size,
		 dest_start + Word8.fromInt (dest_pos + bytes))
       end
      handle Copy.Illegal_Copy {source_length, source_offset, bytes,
				dest_length, dest_offset} =>
	      (local_print ("illegal copy (" ^
			    Integer.toString source_length ^ ", " ^
			    Integer.toString source_offset ^ ", " ^
			    Integer.toString bytes ^ ", " ^
			    Integer.toString dest_length ^ ", " ^
			    Integer.toString dest_offset ^ ")\n");
	       false)
	   | Copy.Self_Copy => (local_print "self copy\n"; false)
	   | x =>
	      (local_print ("copy exception " ^ V.Control.exnName x ^ "\n");
	       false)
              

  fun copy_loop (40, _, _) = true
    | copy_loop (n, 40, _) = copy_loop (n + 1, 0, 0)
    | copy_loop (n, m, 40) = copy_loop (n, m + 1, 0)
    | copy_loop (n, m, p) =
       if test_copy (n + m, n, m, p + m, p) then copy_loop (n, m, p + 1)
       else false

  fun test_bounds (low, high) () =
       let val delta = (high - low) mod 7 - 3
           val src_offset = if delta < 0 then low - delta else low
           val dest_offset = if delta < 0 then low else low + delta
           val bytes = high - low + 1
           val _ = debug_print (fn _ => "testing copy " ^
			        Integer.toString src_offset ^ "->" ^
			        Integer.toString dest_offset ^ " (" ^
			        Integer.toString bytes ^ ")\n")
           val source = Word8Array.array (high + 7, 0w0)
           fun loop n = if n >= Word8Array.length source then source
	                else (Word8Array.update (source, n, Word8.fromInt n);
			      loop (n + 1))
           val dest = Word8Array.array (high + 7, 0w0)
           val copy = Copy.copy (loop 0, src_offset, bytes, dest, dest_offset)
	               handle x => (local_print ("exception in Copy.copy (" ^
						 Integer.toString low ^
						 ", " ^
						 Integer.toString high ^
						 ")");
				    raise x)
           fun compare (src, src_low, bytes, dest, dest_low) =
	        if bytes = 0 then true
	        else if src_low + 1 >= Word8Array.length src then false
	        else if dest_low + 1 >= Word8Array.length dest then false
	        else if Word8Array.sub (src, src_low) <>
	                Word8Array.sub (dest, dest_low) then false
	        else compare (src, src_low + 1, bytes - 1, dest, dest_low + 1)
       in compare (source, src_offset, bytes, dest, dest_offset)
       end

  fun test_num (low, high) =
       Integer.toString low ^ "..." ^ Integer.toString high

  fun loop_inner (low, high) =
       if high <= low then ()
       else (Test.test (test_num (low, high), test_bounds (low, high));
	     loop_inner (low, high - 3))

  fun loop_outer (x, y) () =
       if x >= y then
	Test.test ("massive copy", fn () => copy_loop (0, 0, 0))
       else (loop_inner (x, y);
	     if (x quot 4) * 4 = x then      (* x is a multiple of four *)
	      loop_outer (x + 1, y) ()       (* make x odd *)
	     else if (x quot 2) * 2 = x then (* x is even *)
	      loop_outer (x * 4, y) ()       (* bake x into multiple of four *)
	     else                            (* x is odd *)
	      loop_outer (x * 2, y) ())      (* make x even *)

  fun run () = 
       if Debug.include_tests then
        Test.tests ("Copy", 27, (loop_outer (1, 22)))
       else ()

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2. structure Test_Copy
*)

structure Test_Copy = Test_Copy (structure Copy = Fox_Basis.Copy
				 structure Debug = Fox_Basis.Debug
				 structure V = Fox_Basis.V
				 structure Test = Fox_Basis.Test
				 val debug_level = NONE)






