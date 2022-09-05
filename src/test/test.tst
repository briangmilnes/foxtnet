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
	
       Some non-automatic tests for test.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: test.tst,v $
Revision 1.1  1993/06/10  22:31:39  milnes
Initial revision


		1.	...
*)

(*
Test.tests "Hi" 0 (fn () => ());
Starting Hi tests.
PASS for Hi tests.
Ending Hi tests.

Test.tests "Hi" 1 (fn () => ());
Starting Hi tests.
BUG in Hi executed 0 tests but expected 1 tests.
PASS for Hi tests.
Ending Hi tests.

Test.tests "Hi" 1 (fn () => Test.test "Hi There" (fn () => true));
Starting Hi tests.
PASS for test Hi There.
PASS for Hi tests.
Ending Hi tests.

Test.tests "Hi" 0 (fn () => Test.test "Hi There" (fn () => true));
Starting Hi tests.
PASS for test Hi There.
BUG in Hi executed 1 tests but expected 0 tests.
PASS for tests Hi.
Ending Hi tests.

Test.tests "Hi" 0 (fn () => Test.test "Hi There" (fn () => false)); 
Starting Hi tests.
BUG in test Hi There.
BUG in Hi executed 1 tests but expected 0 tests.
BUG in Hi tests, 1 bugs found.
Ending Hi tests.

Test.output_test "Hi" (fn () => Test.print "Yo") "Hey";
BUG in output_test Hi.
Difference in Hi's output, starts at character 0.
Found: 
"Yo"
Expected: 
"Hey"

Test.output_test "Hi" (fn () => Test.print "HEYo") "Hey";
BUG in output_test Hi.
Difference in Hi's output, starts at character 1.
Found: 
"HEYo"
Expected: 
"Hey"

Test.output_test "Hi" (fn () => Test.print "Heyo") "Hey";
BUG in output_test Hi.
Difference in Hi's output, starts at character 3.
Found: 
"Heyo"
Expected: 
"Hey"

Test.output_test "Hi" (fn () => Test.print "Hey") "Heyo";
BUG in output_test Hi.
Difference in Hi's output, starts at character 3.
Found: 
"Hey"
Expected: 
"Heyo"

Test.output_test "Hi" (fn () => Test.print "Hey") "Hey";
PASS for output_test Hi.
*)
