(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A functor to provide tracing and printing functions.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Debug_Trace

		iii.	RCS Log
	
$Log: trace.fun,v $
Revision 1.4  1995/06/20  16:41:24  esb
added print_{raise,handled,raise_again}

Revision 1.3  1995/03/12  18:00:07  esb
adapted to new trace.sig.

Revision 1.2  1995/03/10  03:53:15  esb
adapted to new vendor.sig.

Revision 1.1  1995/03/08  00:02:33  esb
Initial revision


		1.	functor Trace
*)

functor Trace (structure V: VENDOR
	       val debug_level: int ref option
	       (* debug_level: NONE =>        only local_print,
			       SOME (ref 0) = only local_print,
			       SOME (ref 1) = local_print and trace,
			       SOME (ref 2) = local_print, trace and debug *)
	       val module_name: string
	       val makestring: exn -> string option): TRACE =
 struct
  local
   val extended_name = module_name ^ ": "
   fun noop _ = ()
  in
   fun local_print s = V.Print.print (extended_name ^ s ^ "\n")
  end

  local
   fun test_trace level () = ! level > 0
   fun test_debug level () = ! level > 1

   fun cannot _ = false

   fun noop _ = ()

   fun conditional_constant_string conditional s =
        if conditional () then local_print s else ()

   fun conditional_exec conditional f =
        if conditional () then f () else ()

   fun conditional_print conditional f =
        if conditional () then local_print (f ()) else ()

  in
   val (trace_on, debug_on) =
        case debug_level of
	   NONE => (cannot, cannot)
         | (SOME level_ref) => (test_trace level_ref, test_debug level_ref)

   val (trace_print, debug_print) =
        case debug_level of
	   NONE => (noop, noop)
         | (SOME level_ref) =>
	    (conditional_print (test_trace level_ref),
	     conditional_print (test_debug level_ref))

   val (trace_constant_string, debug_constant_string) =
        case debug_level of
	   NONE => (noop, noop)
         | (SOME level_ref) =>
	    (conditional_constant_string (test_trace level_ref),
	     conditional_constant_string (test_debug level_ref))

   val (do_if_trace, do_if_debug) =
        case debug_level of
	   NONE => (noop, noop)
         | (SOME level_ref) =>
	    (conditional_exec (test_trace level_ref),
	     conditional_exec (test_debug level_ref))

   fun makestring_exn exn =
        "'" ^
        (case makestring exn of SOME s => s | NONE => V.Control.exnName exn) ^
        "'"

   fun makestring_message NONE = ""
     | makestring_message (SOME s) = "in " ^ s ^ ", "

   fun print_raise (exn, message) =
        (local_print (makestring_message message ^ "raising exception " ^
		      makestring_exn exn);
	 raise exn)

   fun print_raise_again (exn, message) =
        (local_print (makestring_message message ^ "seen exception " ^
		      makestring_exn exn ^ ", raising again");
	 raise exn)

   fun print_handled (exn, message) =
        local_print (makestring_message message ^ "handling exception " ^
		     makestring_exn exn);

  end (* local *)
 end (* struct *)




