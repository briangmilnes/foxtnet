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

	Test code for filters.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Filter
	2.	structure Test_Filter

		iii.	RCS Log
	
$Log: filter.tst,v $
Revision 1.15  1996/04/30  20:21:06  esb
removed _Ops.

Revision 1.14  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.13  1995/06/20  16:43:19  esb
brought up to date with filter.sig

Revision 1.12  1995/03/09  22:43:59  esb
adapted to new filter.fun

Revision 1.11  1995/01/20  17:45:58  esb
tested it and made it work for the MIPS.

Revision 1.10  1995/01/14  02:24:20  esb
fixed the filter, not tested yet.

Revision 1.9  1994/11/01  18:26:11  cline
select proper comparison data depending on architecture

Revision 1.8  1994/10/25  16:38:43  esb
added filter_os.

Revision 1.7  1994/10/04  17:42:37  esb
updated to use FoxWords.

Revision 1.6  1994/08/02  19:28:58  esb
adapted to uncurried test.sig.

Revision 1.5  1994/06/16  16:33:06  danwang
Updated for functorized Fox_Basis

Revision 1.4  1993/10/25  19:29:35  cline
removed .U from Byte[421].U

Revision 1.3  1993/09/02  15:21:47  esb
removed the functor Filter from the Test_Filter functor parameters.

Revision 1.2  93/07/22  18:24:28  nickh
Adjusted offsets for sub and update to use byte offsets.

Revision 1.1  1993/06/10  22:29:09  milnes
Initial revision


		1.	functor Test_Filter

   Construct a variety of filters, and compare them to their expected values.

*)

functor Test_Filter (structure B: FOX_BASIS
		       sharing type B.Filter.packet = Word8Array.array
		    ): TEST_STRUCTURE = 
 struct
  fun local_print s = B.V.Print.print ("filter.tst: " ^ s ^ "\n")

  val filter_os = if Compiler.architecture = ".alpha32" then "osf1" else "mach"

  structure F = B.Filter

  fun makestring_list [] = ""
    | makestring_list (h :: rest) =
       (Word16.toString h) ^ "." ^ (makestring_list rest)
     
  fun makestring_array (a, n) =
       if Word8Array.length a = n * 2 then ""
       else
	(Word16.toString (Word16.sub (a, n*2))) ^ "." ^
	(makestring_array (a, n + 1))

  fun different_length (array, index, filter) =
	let val array_size = Word8Array.length array
	    val expected_size = (index + length filter) * 2 
	    val word = if array_size > expected_size then "long" else "short"
	    val s1 = "filter " ^ word ^ "er than expected, delta is "
	    val delta = abs (array_size - expected_size)
	    val s2 = Integer.toString delta ^ " bytes"
	in local_print (s1 ^ s2);
	   false
	end

  fun different_value (index, value, expected) =
       (local_print ("filter value at index " ^
		     Integer.toString index ^ "/" ^
		     Integer.toString (index * 2) ^ " is 0x" ^
		     Word16.toString value ^ ", expected 0x" ^
		     Word16.toString expected);
        false)

  fun same  (word_array, n, l) =
    let
      fun same_array (array, n, []) =
	if Word8Array.length array = n * 2 then true
	else different_length (array, n, [])
	| same_array (array, n, head :: rest) =
	  if Word8Array.length array <= n * 2 + 1 then
	    different_length (array, n, head :: rest)
	  else if Word16.sub (array, n * 2) <> head then
	    different_value (n, Word16.sub (array, n * 2), head)
	       else same_array (array, n + 1, rest)
      val (array, _, _) = Word_Array.expose word_array
    in
      same_array (array, n, l)
    end

  fun run () =
       let val n2 = Word16.fromInt
(* values for Mips/MACH from:
     /afs/cs/project/mach3/latest/src/mk/kernel/device/net_status.h
   values for Alpha/OSF from:
     ????
 *)
	   val nbpa = 0w10		(* bits per argument *)
	   fun pair (arg, opcode) =
	        let val arg_masked = Word16.andb (n2 arg, n2 0x3ff)
		    val op_masked = Word16.andb (n2 opcode, n2 0xfc00)
		in Word16.orb (op_masked, arg_masked)
		end
	   val nopush = 0
	   val pushlit = 1
	   val pushzero = 2
	   val pushind = 14
	   val pushhdrind = 15
	   val pushstk = 992
	   fun data i =
	        case Compiler.architecture of
		   ".alpha32" => 16 + i
		 | _ => if i < 7 then 960 + i else 16 + (2 + i - 7)
(* see the comment in filter.fun [fun assemble_value] for the explanation
   of the "2 + i". *)
	   val nop = Bits.<< (0, nbpa)
	   val eq = Bits.<< (1, nbpa)
	   val lt = Bits.<< (2, nbpa)
	   val le = Bits.<< (3, nbpa)
	   val gt = Bits.<< (4, nbpa)
	   val ge = Bits.<< (5, nbpa)
	   val and_op = Bits.<< (6, nbpa)
	   val or = Bits.<< (7, nbpa)
	   val xor = Bits.<< (8, nbpa)
	   val cor = Bits.<< (9, nbpa)
	   val cand = Bits.<< (10, nbpa)
	   val cnor = Bits.<< (11, nbpa)
	   val cnand = Bits.<< (12, nbpa)
	   val neq = Bits.<< (13, nbpa)
	   val lsh = Bits.<< (14, nbpa)
	   val rsh = Bits.<< (15, nbpa)
	   val add = Bits.<< (16, nbpa)
	   val sub = Bits.<< (17, nbpa)
	   val bpf = Bits.<< ((Bits.<< (1, 0w6) - 1), nbpa)

	   val ftrue = F.make F.True
           val ftrue_expected = [pair (pushlit, nop), n2 1]
	   val ffalse = F.make F.False
	   val ffalse_expected = [pair (pushzero, nop)]
  
	   val feq = F.make (F.Equal (F.Literal (n2 0x5), F.Data 13))
	   val feq_expected = [pair (pushlit, nop), n2 0x5, pair (data 13, eq)]
	   val fls = F.make (F.Less (F.Data 3, F.Literal (n2 0x9)))
	   val fls_expected = [pair (data 3, nop), pair (pushlit, lt), n2 0x9]
	   val fle = F.make (F.Leq (F.Data 0, F.Data 1))
	   val fle_expected = [pair (data 0, nop), pair (data 1, le)]
	   val fgt = F.make (F.Greater (F.Literal (n2 0x5),
					F.Literal (n2 0x9)))
	   val fgt_expected = [pair (pushlit, nop), n2 0x5,
			       pair (pushlit, gt), n2 0x9]
	   val fge = F.make (F.Geq (F.Lshift (F.Data 3, 5),
				    F.Literal (n2 0x9)))
	   val fge_expected = [pair (data 3, nop), pair (pushlit, lsh), n2 0x5,
			       pair (pushlit, ge), n2 0x9]
	   val fne = F.make (F.Neq (F.Literal (n2 0x5), F.Literal (n2 0x9)))
	   val fne_expected = [pair (pushlit, nop), n2 0x5,
			       pair (pushlit, neq), n2 0x9]
	   val fwhen = F.make (F.When_Equal
			       (F.Literal (n2 0x5), F.Data 3,
				F.Less (F.Data 5, F.Literal (n2 0x7))))
	   val fwhen_expected = [pair (pushlit, nop), n2 0x5,
				 pair (data 3, cor), pair (data 5, nop),
				 pair (pushlit, lt), n2 0x7]
	   val funless = F.make (F.Unless_Equal
				 (F.Literal (n2 0x1ff), F.Data 0,
				  F.Neq (F.Rshift (F.Data 7, 2), F.Data 3)))
	   val funless_expected = [pair (pushlit, nop), n2 0x1ff,
				   pair (data 0, cand), pair (data 7, nop),
				   pair (pushlit, rsh), n2 2,
				   pair (data 3, neq)]
	   fun code_eq (lit, ind, cont) =
	        F.Unless_Equal (F.Literal (n2 lit), F.Data ind, cont)
	   val freal =
	        F.make (code_eq
			(300, 0,
			 code_eq (600, 1,
				  F.Equal (F.Literal (n2 900), F.Data 2))))
	   val freal_expected = [pair (pushlit, nop), n2 300,
				 pair (data 0, cand),
				 pair (pushlit, nop), n2 600,
				 pair (data 1, cand),
				 pair (pushlit, nop), n2 900,
				 pair (data 2, eq)]
	 (* conversion to a format which will be big-endian when
	    stored in an array. *)
	   fun n2o high low = n2 (low * 256 + high)
	   fun make_array (_, _, []) = ()
	     | make_array (array, index, head :: rest) =
	        (Word16.update (array, index, head);
		 make_array (array, index + 2, rest))
	   val test_packet = Word8Array.array (1600, 0w0)
	   val _ = make_array
	             (test_packet, 0,
		      [n2o 0 1, n2o 2 3, n2o 4 5, (* eth src *)
		       n2o 9 8, n2o 7 6, n2o 5 4, (* eth dest *)
		       n2o 8 0,		(* eth protocol *)
		       n2o 0x45 0, n2o 0 21, n2o 0 13, n2o 0 0,
		       n2o 1 9, n2o 0 0,
		       n2o 8 2, n2o 0x12 0x34, n2o 8 2, n2o 0x56 0x78,
		       n2o 3 0])
	   fun check (a, b) c = F.Unless_Equal (a, b, c)
	   val pass_filter =
	         check (F.Literal (n2o 8 0), F.Data 6)
	         (check (F.Literal (n2o 8 2), F.Data 15)
		  (F.Equal (F.Literal (n2o 0x56 0x78), F.Data 16)))
	   val drop_filter =
	         check (F.Literal (n2o 8 0), F.Data 6)
	         (check (F.Literal (n2o 8 2), F.Data 15)
		  (F.Equal (F.Literal (n2o 0x12 0x34), F.Data 16)))
	   fun all_tests () =
	        (B.Test.test ("ftt", fn () => same (ftrue, 0, ftrue_expected));
		 B.Test.test ("fff",
			      fn () => same (ffalse, 0, ffalse_expected));
		 B.Test.test ("feq", fn () => same (feq, 0, feq_expected));
		 B.Test.test ("fls", fn () => same (fls, 0, fls_expected));
		 B.Test.test ("fle", fn () => same (fle, 0, fle_expected));
		 B.Test.test ("fgt", fn () => same (fgt, 0, fgt_expected));
		 B.Test.test ("fge", fn () => same (fge, 0, fge_expected));
		 B.Test.test ("fne", fn () => same (fne, 0, fne_expected));
		 B.Test.test ("fwh", fn () => same (fwhen, 0, fwhen_expected));
		 B.Test.test ("ful",
			      fn () => same (funless, 0, funless_expected));
		 B.Test.test ("real",
			      fn () => same (freal, 0, freal_expected));
		 B.Test.test ("pass",
			      fn () => F.filter pass_filter test_packet);
		 B.Test.test ("drop",
			    fn () => not (F.filter drop_filter test_packet)))

       in B.Test.tests ("Filter", 13, all_tests)
       end

  val _ = if ! B.Debug.do_tests then run() else ()

 end

(*

		2.	structure Test_Filter

*)

structure Test_Filter = Test_Filter (structure B = Fox_Basis)

