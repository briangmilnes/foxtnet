(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This file constructs a signature for a set of testing utilities.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TEST


		iii.	RCS Log
	
$Log: test.sig,v $
Revision 1.3  1994/08/02  19:28:37  esb
uncurried functions.

Revision 1.2  1993/10/21  20:08:15  esb
merged passed_modules and failed_modules into results.

Revision 1.1  1993/06/10  22:31:39  milnes
Initial revision


*)

(*
		1.	signature TEST

 The testing utilities provide for automated testing with flexible bug
reporting.

*)

signature TEST =
 sig

  val tests: string * int * (unit -> unit) -> unit

(* The tests funtion takes a name for a set of tests, the expected
   number of test to be run, and a unit to unit function that runs them.
   It applies the function to run the test and prints results based upon
   the variable tests_print_when. If tests_print_when is Pass, it only
   prints if it passed all of the tests. If tests_print_when is Bug it
   only prints if it found a bug. Always causes it to always print and
   Never causes it to print nothing. 
 *)

  datatype print_when = Pass | Bug | Always | Never
  val tests_print_when: print_when ref

(* Each individual test is run in a test or output test expression.
   Test is for predicate based tests, if the unit->bool function
   evaluates to true, the test passes, otherwise it fails.  
   test_print_when controls when the test prints its results.
 *)

  val test: string * (unit -> bool) -> unit
  val test_print_when: print_when ref

(* Output_test captures the output from the unit -> unit function in a
   string, and compares it with the second string argument. The two flags
   control when the results versus the captured output of the string.  A
   common way to use this is a write output based tests and give them an
   empty string with output_test_print_output_when := Always.  When the
   test is run under an editor it will print the output that it captured,
   if it is right install kill it and install it in the source code.
 *)
  val output_test: string * (unit -> unit) * string -> unit
  val output_test_print_when: print_when ref
  val output_test_print_output_when: print_when ref

(* The tests can only capture the output if they call print for
   all of their output. *)

  val print: string -> unit

(* Sometimes a test will loop infinitely, and to facilitate catching this,
   print can be coaxed into also printing the output to the screen if 
   print_print_output_when is Always. *)

  val print_print_output_when: print_when ref
  val output_collector: string list ref

(* test also keeps track of what tests have failed and succeeded, and will
   return a list of the files that have and have not passed tests since the
   last time reset was called.
 *)

  val reset: unit -> unit
  val results: unit -> {passed: string list, failed: string list}

 end (* struct *)

