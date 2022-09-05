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

	Test code to test the Word structures.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Word
	2.	structure Test_Word1
	3.	structure Test_Word2
	4.	structure Test_Word4
	5.	structure Test_Word8
	6.	structure Test_Word16
	7.	structure Test_Word24
	8.	structure Test_Word32
	9.	structure Test_Word48
	10.	structure Test_Word64
	11.	structure Test_Word128
	12.	structure Test_Word256
	2.      structure Test_Store

		iii.	RCS Log
	
$Log: word.tst,v $
Revision 1.10  1996/03/12  22:21:13  esb
removed signature FOXWORD_OPERATIONS, tried to optimizie Word48.sub/update.

Revision 1.9  1996/02/06  22:07:02  esb
added Test_Word24, made Test_Word64 non-comprehensive by default.

Revision 1.8  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.7  1995/11/10  23:29:27  esb
added tests for the new word structures.

Revision 1.6  1995/10/17  21:43:46  esb
added tests for FoxWord1, fixed bugs.

Revision 1.5  1995/09/14  15:47:16  esb
reordered comprehensives

Revision 1.4  1995/06/20  16:22:47  esb
minor fixes.

Revision 1.3  1995/04/05  15:10:26  esb
added tests for Word48.

Revision 1.2  1995/03/09  22:43:11  esb
adapted to new vendor.sig.

Revision 1.1  1995/03/07  20:23:56  esb
Initial revision

		1.	functor Test_Word
*)

functor Test_Word (structure Test_Word: FOXWORD
		   val bits: Word.word
		   val comprehensive: bool
		   structure V: VENDOR
		   structure Debug: DEBUG
		   structure Test: TEST): TEST_STRUCTURE =
 struct
  val name = "Word" ^ Integer.toString (Word.toInt bits)
  val shift_step = if comprehensive then 0w1 else Word.div (bits, 0w4)

  val zero = Test_Word.fromInt 0
  val one = Test_Word.fromInt 1
  val two = Test_Word.fromInt 2
  val three = Test_Word.fromInt 3
  val four = Test_Word.fromInt 4
  val five = Test_Word.fromInt 5
  val minus_one = Test_Word.- (zero, one)
  val max_positive = Test_Word.>> (minus_one, 0w1)

  fun step_shift (n, 0w0, shift) = n
    | step_shift (n, shift_count, shift) =
       step_shift (shift (n, 0w1), shift_count - 0w1, shift)

  fun check_shift (n, shift_count, shift, shift_name) =
       let val plain = shift (n, shift_count)
	   val check = step_shift (n, shift_count, shift)
       in if check = plain then check
	  else
	   (Test.test (shift_name ^ " (0x" ^ Test_Word.toString n ^ ", " ^
		       Word.toString shift_count ^ ") = " ^
		       Test_Word.toString plain ^ " instead of 0x" ^
		       Test_Word.toString check, fn _ => false);
	    check)
       end

  fun do_shifts (n, shift_count, shift, shift_name) =
       if shift_count > bits then []
       else
	check_shift (n, shift_count, shift, shift_name)
	:: do_shifts (n, shift_count + shift_step, shift, shift_name)

  fun all_shifts n =
       do_shifts (n, 0w0, Test_Word.~>>, "~>>") @
       do_shifts (n, 0w0, Test_Word.>>, ">>") @
       do_shifts (n, 0w0, Test_Word.<<, "<<")

  fun elim_member (member, []) = []
    | elim_member (member, head :: tail) =
       if member = head then elim_member (member, tail)
       else head :: elim_member (member, tail)

  fun uniq_sort [] = []
    | uniq_sort (head :: tail) =
       let val least = V.List.fold Test_Word.min tail minus_one
       in if Test_Word.<= (head, least) then
	   head :: (uniq_sort (elim_member (head, tail)))
	  else least :: (uniq_sort (head :: (elim_member (least, tail))))
       end

  fun pseudo_random () =
       let val all = [zero, one, two, three, four, five, minus_one] @
		     all_shifts one @ all_shifts minus_one @ all_shifts four
	   val sorted = uniq_sort all
       in V.Print.print ("original length " ^ 
			 Integer.toString (length all) ^
			 ", sorted length " ^
			 Integer.toString (length sorted) ^ "\n");
	  sorted
       end

  fun all_pairs (_, []) = []
    | all_pairs (element, head :: tail) =
       (element, head) :: all_pairs (element, tail)

  fun cart_prod ([], _) = []
    | cart_prod (head :: tail, right) =
       all_pairs (head, right) @ cart_prod (tail, right)

  fun test_unop (real_op, test_op, argument, result_makestring, op_name) =
       let fun test argument =
	        if ((real_op argument = test_op argument)
		    handle _ => false) then
		 true
		else
		 (Test.test (op_name ^ " (0x" ^
			     Test_Word.toString argument ^ ") is " ^
			     ((result_makestring (real_op argument))
			      handle _ => "exception") ^
			     " instead of " ^
			     ((result_makestring (test_op argument))
			      handle _ => "exception"),
			     fn _ => false);
		  false)
	   fun fold_all (_, false) = false
	     | fold_all (x, true) = x
       in Test.test (op_name,
		     fn _ => V.List.fold fold_all (map test argument) true)
       end

  fun test_bin (real_op, test_op, left, right, result_makestring, op_name) =
       let val pairs = cart_prod (left, right)
	   fun test (arguments as (l, r)) =
	        if ((real_op arguments = test_op arguments)
		    handle _ => false) then
		 true
		else
		 (Test.test (op_name ^ " (0w" ^ Test_Word.toString l ^ ", 0x" ^
			     Test_Word.toString r ^ ") is " ^
			     ((result_makestring (real_op arguments))
			      handle _ => "exception") ^
			     " instead of " ^
			     ((result_makestring (test_op arguments))
			      handle _ => "exception"),
			     fn _ => false);
		  false)
	   fun fold_all (_, false) = false
	     | fold_all (x, true) = x
       in Test.test (op_name,
		     fn _ => V.List.fold fold_all (map test pairs) true)
       end

  fun pseudo_add (left, right) =
       if left = zero then right
       else if right = zero then left
       else
	let val shifted_left = Test_Word.>> (left, 0w1)
	    val shifted_right = Test_Word.>> (right, 0w1)
	    val shifted_sum = pseudo_add (shifted_left, shifted_right)
	    val upper_part = Test_Word.* (shifted_sum, two)
	    val odd_left = Test_Word.andb (left, one)
	    val odd_right = Test_Word.andb (right, one)
	in Test_Word.+ (Test_Word.+ (odd_left, odd_right), upper_part)
	end

  fun pseudo_minus (left, right) =
       if left = right then zero
       else if right = zero then left
       else if right = one then Test_Word.- (left, one)
       else
	let val half = Test_Word.>> (right, 0w1)
	in pseudo_minus (Test_Word.- (left, half), Test_Word.- (right, half))
	end

  fun pseudo_mult (left, right) =
       if left = zero orelse right = zero then zero
       else
        let val least = Test_Word.andb (left, one)
	    val half = pseudo_mult (Test_Word.>> (left, 0w1), right)
	in Test_Word.+ (if least = one then right else zero,
			 Test_Word.+ (half, half))
	end

  fun pseudo_div (left, right) =
       if Test_Word.< (left, right) then zero
       else if Test_Word.< (max_positive, right) orelse
               Test_Word.< (left, Test_Word.* (right, two)) then one
       else
	let val double = Test_Word.* (right, two)
	    val high_div = Test_Word.* (pseudo_div (left, double), two)
	    val rest = Test_Word.- (left, Test_Word.* (high_div, right))
	    val low_div = pseudo_div (rest, right)
	    val result = Test_Word.+ (high_div, low_div)
	in result
	end

  fun pseudo_mod (left, right) =
       Test_Word.- (left, Test_Word.* (Test_Word.div (left, right), right))

  fun pseudo_less (left, right) =
       if Test_Word.~>> (left, bits - 0w1) = zero andalso
          Test_Word.~>> (right, bits - 0w1) = zero then
        Test_Word.~>> (Test_Word.- (left, right), bits - 0w1) <> zero
       else if Test_Word.~>> (left, bits - 0w1) = zero then true
       else if Test_Word.~>> (right, bits - 0w1) = zero then false
       else
	if max_positive <> zero then
	 pseudo_less (Test_Word.- (left, max_positive),
		      Test_Word.- (right, max_positive))
	else
	 pseudo_less (Test_Word.- (left, one), Test_Word.- (right, one))
  fun pseudo_greater (left, right) = pseudo_less (right, left)
  fun pseudo_le (left, right) = not (pseudo_greater (left, right))
  fun pseudo_ge (left, right) = not (pseudo_less (left, right))

  fun pseudo_min (left, right) =
       if pseudo_less (left, right) then left else right
  fun pseudo_max (left, right) =
       if pseudo_less (left, right) then right else left

  fun explode (n, 0w0) = []
    | explode (n, bits) =
       (if Test_Word.<< (Test_Word.>> (n, 0w1), 0w1) = n then zero else one) ::
       explode (Test_Word.>> (n, 0w1), bits - 0w1)
  fun implode [] = zero
    | implode (head :: tail) =
       Test_Word.+ (head, Test_Word.* (implode tail, two))
  exception Unmatched
  fun and_pair ([], []) = []
    | and_pair (head_a :: tail_a, head_b :: tail_b) =
       (if head_a = head_b andalso head_a = one then one else zero) ::
       and_pair (tail_a, tail_b)
    | and_pair _ = raise Unmatched
  fun pseudo_and (left, right) =
       implode (and_pair (explode (left, bits), explode (right, bits)))
  fun or_pair ([], []) = []
    | or_pair (head_a :: tail_a, head_b :: tail_b) =
       (if head_a = head_b andalso head_a = zero then zero else one) ::
       or_pair (tail_a, tail_b)
    | or_pair _ = raise Unmatched
  fun pseudo_or (left, right) =
       implode (or_pair (explode (left, bits), explode (right, bits)))
  fun xor_pair ([], []) = []
    | xor_pair (head_a :: tail_a, head_b :: tail_b) =
       (if head_a = head_b then zero else one) :: xor_pair (tail_a, tail_b)
    | xor_pair _ = raise Unmatched
  fun pseudo_xor (left, right) =
       implode (xor_pair (explode (left, bits), explode (right, bits)))
  fun not_single n = if n = zero then one else zero
  fun pseudo_not argument =
       implode (map not_single (explode (argument, bits)))

  val data = pseudo_random ()

  fun get_rid_of_zeros [] = []
    | get_rid_of_zeros (head :: tail) =
       if head = zero then get_rid_of_zeros tail
       else head :: get_rid_of_zeros tail

  val non_zero = get_rid_of_zeros data

  fun boolstring true = "true"
    | boolstring false = "false"

  fun makestring w = "0x" ^ Test_Word.toString w

  fun run_tests () =
       (test_bin (Test_Word.+, pseudo_add, data, data, makestring, "plus");
        test_bin (Test_Word.-, pseudo_minus, data, data, makestring, "minus");
        test_bin (Test_Word.*, pseudo_mult, data, data, makestring, "times");
        test_bin (Test_Word.div, pseudo_div, data, non_zero, makestring,
		  "div");
        test_bin (Test_Word.mod, pseudo_mod, data, non_zero, makestring,
		  "mod");
        test_bin (Test_Word.<, pseudo_less, data, data, boolstring, "less");
        test_bin (Test_Word.>, pseudo_greater, data, data, boolstring,
		  "greater");
        test_bin (Test_Word.<=, pseudo_le, data, data, boolstring, "le");
        test_bin (Test_Word.>=, pseudo_ge, data, data, boolstring, "ge");
        test_bin (Test_Word.min, pseudo_min, data, data, makestring, "min");
        test_bin (Test_Word.max, pseudo_max, data, data, makestring, "max");
        test_bin (Test_Word.andb, pseudo_and, data, data, makestring, "andb");
        test_bin (Test_Word.orb, pseudo_or, data, data, makestring, "orb");
        test_bin (Test_Word.xorb, pseudo_xor, data, data, makestring, "xorb");
        test_unop (Test_Word.notb, pseudo_not, data, makestring, "notb");
	())

  fun run () =
       Test.tests (name, 15, run_tests)

  val _ = if ! Debug.do_tests then run () else ()

 end (* struct *)

(*
		2.	structure Test_Word1
*)

structure Test_Word1 = Test_Word (structure Test_Word = Word1
				  val bits = 0w1
				  structure V = Fox_Basis.V
				  structure Debug = Fox_Basis.Debug
				  structure Test = Fox_Basis.Test
				  val comprehensive = true)

(*
		3.	structure Test_Word2
*)

structure Test_Word2 = Test_Word (structure Test_Word = Word2
				  val bits = 0w2
				  structure V = Fox_Basis.V
				  structure Debug = Fox_Basis.Debug
				  structure Test = Fox_Basis.Test
				  val comprehensive = true)

(*
		4.	structure Test_Word4
*)

structure Test_Word4 = Test_Word (structure Test_Word = Word4
				  val bits = 0w4
				  structure V = Fox_Basis.V
				  structure Debug = Fox_Basis.Debug
				  structure Test = Fox_Basis.Test
				  val comprehensive = true)

(*
		5.	structure Test_Word8
*)

structure Test_Word8 = Test_Word (structure Test_Word = Word8
				  val bits = 0w8
				  structure V = Fox_Basis.V
				  structure Debug = Fox_Basis.Debug
				  structure Test = Fox_Basis.Test
				  val comprehensive = true) 

(*
		6.	structure Test_Word16
*)
  
structure Test_Word16 = Test_Word (structure Test_Word = Word16
				   val bits = 0w16
				   structure V = Fox_Basis.V
				   structure Debug = Fox_Basis.Debug
				   structure Test = Fox_Basis.Test
				   val comprehensive = true)


(*
		7.	structure Test_Word24
*)
  
structure Test_Word24 = Test_Word (structure Test_Word = Word24
				   val bits = 0w24
				   structure V = Fox_Basis.V
				   structure Debug = Fox_Basis.Debug
				   structure Test = Fox_Basis.Test
				   val comprehensive = true)


(*
		8.	structure Test_Word32
*)

structure Test_Word32 = Test_Word (structure Test_Word = Word32
				   val bits = 0w32
				   structure V = Fox_Basis.V
				   structure Debug = Fox_Basis.Debug
				   structure Test = Fox_Basis.Test
				   val comprehensive = true)
(*
		9.	structure Test_Word48

	Because this test can be quite slow, it is generally
	recommended that you run it as comprehensive = false.
	On rare occasions you may wish to run comprehensive = true.
*)


structure Test_Word48 = Test_Word (structure Test_Word = Word48
				   val bits = 0w48
				   structure V = Fox_Basis.V
				   structure Debug = Fox_Basis.Debug
				   structure Test = Fox_Basis.Test
				   val comprehensive = false)

(*
		10.	structure Test_Word64

	Because this test can be quite slow, it is generally
	recommended that you run it as comprehensive = false.
	On rare occasions you may wish to run comprehensive = true.
*)

structure Test_Word64 = Test_Word (structure Test_Word = Word64
				   val bits = 0w64
				   structure V = Fox_Basis.V
				   structure Debug = Fox_Basis.Debug
				   structure Test = Fox_Basis.Test
				   val comprehensive = false)


(*
		11.	structure Test_Word128

	Because this test can be quite slow, it is generally
	recommended that you run it as comprehensive = false.
	On rare occasions you may wish to run comprehensive = true.
*)

structure Test_Word128 = Test_Word (structure Test_Word = Word128
				    val bits = 0w128
				    structure V = Fox_Basis.V
				    structure Debug = Fox_Basis.Debug
				    structure Test = Fox_Basis.Test
				    val comprehensive = false)


(*
		12.	structure Test_Word256

	Because this test can be quite slow, it is generally
	recommended that you run it as comprehensive = false.
	On rare occasions you may wish to run comprehensive = true.
*)

structure Test_Word256 = Test_Word (structure Test_Word = Word256
				    val bits = 0w256
				    structure V = Fox_Basis.V
				    structure Debug = Fox_Basis.Debug
				    structure Test = Fox_Basis.Test
				    val comprehensive = false)


