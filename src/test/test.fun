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

	A functor that creates a set of test coding utilities.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test

		iii.	RCS Log
	
$Log: test.fun,v $
Revision 1.14  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.13  1995/06/20  16:28:17  esb
minor improvements.

Revision 1.12  1995/03/24  01:38:10  esb
added handling and signaling of exceptions within tests.

Revision 1.11  1995/03/09  22:44:16  esb
adapted to new vendor.sig.

Revision 1.10  1995/02/04  21:46:38  robby
updated to 107

Revision 1.9  1995/01/18  20:46:06  esb
made results print the length of the passed and failed lists.

Revision 1.8  1994/11/07  21:31:48  cline
use V.String

Revision 1.7  1994/09/29  21:04:36  cline
remove "signature VENDOR = VENDOR"

Revision 1.6  1994/09/29  20:14:46  cline
added "signature VENDOR = VENDOR" to prevent SC confusion

Revision 1.5  1994/08/02  19:28:37  esb
uncurried functions.

Revision 1.4  1994/02/08  14:14:39  esb
changed print_print_output_when to be Always by default.

Revision 1.3  1993/10/21  20:08:15  esb
merged passed_modules and failed_modules into results.

Revision 1.2  1993/06/23  15:22:18  esb
fixed a minor bug (printing of PASS when the result was FAIL)

Revision 1.1  1993/06/10  22:31:39  milnes
Initial revision


		1.	functor Test
*)

functor Test (structure V: VENDOR): TEST =
 struct
  (* The V.Print.print passed in is the actual system V.Print.print that will
     V.Print.print to the screen. *)

  (* lists of modules that have passed or failed tests *)
  val passed_list = ref ([]: string list)
  val failed_list = ref ([]: string list)

  datatype print_when = Pass | Bug | Always | Never
  val tests_print_when = ref Always

  val tests_performed = ref 0;
  val bugs_detected = ref 0;

  fun tests (name, expected_number_of_tests, f) =
    (tests_performed := 0;
     bugs_detected   := 0;
     if ! tests_print_when = Always then
      (V.Print.print "Starting ";
       V.Print.print name;
       V.Print.print " tests.\n")
     else ();
     f ();
     if ! tests_performed <> expected_number_of_tests then
      (bugs_detected := ! bugs_detected +
                        Integer.abs (! tests_performed -
				     expected_number_of_tests);
       if ! tests_print_when = Bug orelse ! tests_print_when = Always then
	V.Print.print ("BUG in " ^ name ^ ": executed " ^
		       (Integer.toString (! tests_performed)) ^
		       " tests but expected " ^
		       (Integer.toString expected_number_of_tests) ^
		       " tests.\n")
       else ())
     else ();
     if ! bugs_detected <> 0 andalso
        (! tests_print_when = Bug orelse ! tests_print_when = Always) then
       (V.Print.print "BUG in ";
	V.Print.print name;
	V.Print.print " tests, ";
        V.Print.print (Integer.toString(!bugs_detected));
        V.Print.print " bugs found.\n")
     else ();
     if ! bugs_detected = 0 andalso
        (! tests_print_when = Pass orelse ! tests_print_when = Always) then
      V.Print.print ("PASS for " ^ name ^ " tests.\n")
     else ();
     if ! bugs_detected = 0 then
      passed_list := (! passed_list) @ [name]
     else
      failed_list := (! failed_list) @ [name];
     if ! tests_print_when = Always then
      V.Print.print ("Ending " ^ name ^ " tests.\n")
     else ())

  val test_print_when = ref Always

  fun test (name, f) =
   let val passed = ((f ())
		     handle x => 
		             (V.Print.print ("EXCEPTION " ^
					     V.Control.exnName x ^
					     " raised by test " ^
					     name ^ ".\n");
			      false))
       fun pass () =
	    V.Print.print ("PASS for test " ^ name ^ ".\n")
       fun bug () = (bugs_detected := ! bugs_detected + 1;
		     V.Print.print "BUG in test ";
		     V.Print.print name;
		     V.Print.print ".\n")
   in tests_performed := ! tests_performed + 1;
      case ! test_print_when of
        Pass =>   if passed then pass () else ()
      | Bug =>    if not passed then bug () else ()
      | Always => if passed then pass () else bug ()
      | Never => ()
   end

  val print_print_output_when = ref Always

  (* Store any output printed on a list of strings. *)
  val output_collector = ref [] : string list ref

  fun print string = 
   (output_collector := string :: ! output_collector;
    if ! print_print_output_when = Always then 
     V.Print.print string
    else ())

  val output_test_print_when = ref Always
  val output_test_print_output_when = ref Bug

  fun compare_character_lists ([], [],n) = NONE
    | compare_character_lists ([], a :: b, n) = SOME n
    | compare_character_lists (a :: b, [], n) = SOME n
    | compare_character_lists (a :: b, c :: d, n) =
       if a = c then compare_character_lists (b, d, n + 1) else SOME n

  fun compare_string_list_with_string (string_list, string) = 
       compare_character_lists
         (V.List.fold op@ (map V.String.explode string_list) [],
	  V.String.explode string, 0)

  fun output_test (name, f, expected_output) = 
       (output_collector := [];
        f ();
        let val output = rev (!output_collector)
            val passed = compare_string_list_with_string (output,
							  expected_output)
	    fun print_output_chars [] = ()
	      | print_output_chars (#"\n" :: n) =
		 (V.Print.print "\\n\\\n\\";
		  print_output_chars n)
	      | print_output_chars (s :: n) =
		 (V.Print.print (V.String.str s);
		  print_output_chars n)
	    fun print_output_strings_aux [] = V.Print.print "\"\n"
	      | print_output_strings_aux (s :: r) =
		 (print_output_chars (V.String.explode s);
		  print_output_strings_aux r)
	    fun print_output_strings (SOME i) found = 
	         (V.Print.print "Difference in ";
		  V.Print.print name;
		  V.Print.print "'s output, starts at character ";
		  V.Print.print (Integer.toString i);
		  V.Print.print ".\n";
		  V.Print.print "Found: \n";
		  V.Print.print "\"";
		  print_output_strings_aux found;
		  V.Print.print "Expected: \n";
		  V.Print.print "\"";
		  V.Print.print expected_output;
		  V.Print.print "\"\n")
	      | print_output_strings NONE found =
		 V.Print.print "No difference in name's output\n."
	    fun pass () =
	         (V.Print.print "PASS for output_test ";
		  V.Print.print name;
		  V.Print.print ".\n";
		  if ! output_test_print_output_when = Pass orelse
		     ! output_test_print_output_when = Always then 
		   print_output_strings passed output
		  else ())
	    fun bug () =
	         (bugs_detected := ! bugs_detected + 1;
		  V.Print.print "BUG in output_test ";
		  V.Print.print name;
		  V.Print.print ".\n";
		  if ! output_test_print_output_when = Bug orelse
		     ! output_test_print_output_when = Always then 
		   print_output_strings passed output
		  else ())
	in tests_performed := ! tests_performed + 1;
	   case ! test_print_when of
	      Pass =>   if passed = NONE then pass() else ()
	    | Bug =>    if passed <> NONE then bug() else ()
	    | Always => if passed = NONE then pass() else bug ()
	    | Never => ()
	end)

  fun reset () =
       (passed_list := [];
        failed_list := [])

  fun results () =
       (V.Print.print ("passed " ^
		       Integer.toString (length (! passed_list)) ^
		       ", failed " ^
		       Integer.toString (length (! failed_list)) ^
		       "\n");
	{passed = ! passed_list, failed = ! failed_list})

 end

