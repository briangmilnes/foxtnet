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

	Some utilities to help use files. 


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: useutils.sml,v $
Revision 1.17  1995/02/09  19:48:24  esb
added comments.

Revision 1.16  1995/01/17  21:00:50  esb
brought vars_reset up to date.

Revision 1.15  1994/09/30  16:57:40  esb
removed references to those variables which are no longer in 1.05

Revision 1.14  1994/05/10  07:39:20  esb
minor change.

Revision 1.13  94/01/17  19:23:35  milnes
Removed automatic loading of the disassembler.

Revision 1.12  1994/01/09  03:54:05  esb
now make raises the exception that stopped it.

Revision 1.11  1993/12/16  15:31:51  esb
added the disassembler.

Revision 1.10  1993/10/14  18:14:39  milnes
Used implicit sequencing in let bodied.

Revision 1.9  1993/10/08  05:32:08  esb
added a reminder.

Revision 1.8  1993/09/20  16:57:04  cline
*** empty log message ***

Revision 1.7  1993/09/01  13:47:04  esb
added the defintion of save_heap.

Revision 1.6  1993/08/27  18:08:00  esb
added vars_reset for my convenience.

Revision 1.5  1993/07/13  18:29:40  esb
made the make process less verbose

Revision 1.4  1993/06/21  01:37:55  esb
improved the user interface of make_from

Revision 1.3  1993/06/14  21:19:16  esb
added shortcut "m" for "make_from"

Revision 1.2  1993/06/11  20:46:13  esb
*** empty log message ***

Revision 1.1  1993/06/10  21:30:03  milnes
Initial revision


		1.	...
*)

local
 val make_start_from = ref ""

 val make_default_group = ref ([]: string list)

in
 fun make_from () =
      let (* val old_len = ! System.Control.Print.printLength
          val old_sig = ! System.Control.Print.signatures
          val _ = System.Control.Print.printLength := 50
          val _ = System.Control.Print.signatures := 0
*)
          val skipped = ref 0
          val compiled = ref 0
	  val raised = ref (NONE: exn option)
          fun record [] = (make_start_from := ""; [])
            | record (f :: r) =
	       if ! make_start_from <> "" andalso
		  f = (! make_start_from) then
		(make_start_from := "";
		 record (f :: r))
	       else if (! make_start_from) = "" then
		if ((use f; false)
		    handle x =>
		            (raised := SOME x;
			     make_start_from := f;
			     true)) then
		 []
		else (compiled := ! compiled + 1;
		      f :: (record r))
	       else (skipped := ! skipped + 1;
		     record r)
	  val g = ! make_default_group
	  val in_group = (length g - (! skipped))
      in record g;
         (if ! skipped = 0 then
	   print ("compiled " ^ (makestring (! compiled)) ^ " of " ^
		  (makestring in_group) ^ "\n")
          else if (! skipped + (! compiled)) = in_group then
	   print ("completed " ^ (makestring in_group) ^ " files\n")
          else
	   print ("compiled " ^ (makestring (! compiled)) ^ " of " ^
		  (makestring in_group) ^ " (of which " ^
		  (makestring (! skipped)) ^
		  " were compiled earlier)\n"));
(*
         System.Control.Print.printLength := old_len;
         System.Control.Print.signatures := old_sig;
*)
	 case ! raised of
	    NONE => ()
	  | SOME x => raise x
      end

 fun make () =
  (make_start_from := "";
   make_from ())

 fun make_group g =
  (make_default_group := g;
   make_start_from := "";
   make ())

 val m = make_from

(*
 fun vars_reset_093 () =
      (System.Control.indexing := false;
       System.Print.printLength := 100;
       System.Control.secondaryPrompt := "")
*)

(*
 fun vars_reset_105 () =
      (Compiler.Control.indexing := false;
       Compiler.Control.Print.printLength := 100;
       Compiler.Control.secondaryPrompt := "")
*)

(* this function is commented because it cannot be declared
   until after the Test module has been defined. Use cut-and-paste
   or any other suitable mechanism to declare it at the appropriate
   time if so desired.

 fun save_heap name =
  (vars_reset ();
   exportML name;
   (Test.failed_modules (), Test.passed_modules ()))

 *)

end; (* local *)

(* 
vars_reset ();
use "/afs/cs.cmu.edu/project/fox/FoxML/wrk/mips/use.sml";
val _ = use_mips ();
val disassemble = Mips_Code.disassemble;

Under SML/NJ 1.05:
fun display_assembly () = Compiler.Control.CG.misc4 := 4;
fun dont_display_assembly () = Compiler.Control.CG.misc4 := 0;
*)
