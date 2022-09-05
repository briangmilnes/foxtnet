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

	A signature for a structure to control printing/tracing prints
	in modules of the system.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TRACE
	2.	print functions

		iii.	RCS Log
	
$Log: trace.sig,v $
Revision 1.3  1995/06/20  16:28:48  esb
added print_{raise,handled,raise_again}

Revision 1.2  1995/03/12  17:58:59  esb
changed the types of debug_print and trace_print to make it harder
to code really inefficient things inadvertently (like formatting
an entire byte array every time we send/receive a packet).

Revision 1.1  1995/03/08  00:02:33  esb
Initial revision


		1.	signature TRACE
*)

signature TRACE =
 sig
  val local_print: string -> unit
  val trace_print: (unit -> string) -> unit
  val debug_print: (unit -> string) -> unit

  (* the following two functions should only be used when the argument is
     a constant-valued string.  This way there is no performance
     penalty for argument evaluation if debugging/tracing is off.
     If the argument is computed, use trace/debug_print instead. *)
  val trace_constant_string: string -> unit
  val debug_constant_string: string -> unit

  val do_if_trace: (unit -> unit) -> unit
  val do_if_debug: (unit -> unit) -> unit

  val trace_on: unit -> bool
  val debug_on: unit -> bool

(*
		2.	print functions

        Print_raise prints a message appropriate to the raising of an
	exception; print_raise_again prings a message appropriate to
	catching and re-raising an exception.  Both functions then
	raise the exception.  Print_handled prints a message about an
	otherwise uncaught exception being caught and handled.
*)

  val print_raise: exn * string option -> 'a
  val print_handled: exn * string option -> unit
  val print_raise_again: exn * string option -> 'a
 end 


