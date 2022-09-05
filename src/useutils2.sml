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


	Utilities to build a "standard" heap.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	load
	2.	buildimage

		iii.	RCS Log
	
$Log: useutils2.sml,v $
Revision 1.5  1994/10/19  21:45:54  milnes
made the heap alpha-specific.

Revision 1.4  1994/09/30  16:57:40  esb
removed references to those variables which are no longer in 1.05

Revision 1.3  1994/08/03  19:51:01  esb
got rid of '#'s in RCS log

Revision 1.2  1994/01/30  18:38:00  milnes
changed run to foxrun.

Revision 1.1  1993/10/25  18:41:19  esb
Initial revision

		1.	load 
*)

(*
  System.Control.Print.printLength := 200;
*)

  fun loadcount ([], n, l, f) = (n, l, f)
    | loadcount (first_file :: rest_of_the_files, n, l, f) =
      (use first_file;
       loadcount (rest_of_the_files, n + 1, l, f))
      handle _ => loadcount (rest_of_the_files, n, l, first_file :: f)

  fun load files = loadcount (files, 0, length files, [])


(*
		2.	buildimage
*)

  fun buildimage (name, startupmessage) =
   (
    let val file = open_out name;
       (* This builds a shell script to run with the latest runtime.
         When that runtime changes then the script will have to have
         this textual link snapped by hand. *)
    in 
     (case Compiler.architecture of
        ".mipsLittle" => 
          output (file,
                  "#!/bin/csh -f\n/afs/cs/project/fox/FoxML/bin/foxrun -s " ^
                  name ^ ".heap\n")
      | ".alpha32" => 
         output (file,
           "#!/bin/csh -f\n/afs/cs/misc/sml/alpha_osf1/alpha/bin/.run/run.alpha32-osf1 @SMLload=" ^
                 name ^ ".heap @SMLdebug=/dev/null\n")
      | _ => (print "can't save image, can't match this architecture.\n";
              raise Match));
       close_out file;
       execute ("chmod", ["a + x", name])
    end;
    if exportML(name ^ ".heap") then 
     output (std_out, startupmessage)
    else 
     output (std_out,
	     "Buildimage: Saved " ^ name ^ " and " ^ name ^ ".heap.\n")
  )
