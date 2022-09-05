(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	protoexn.str: protocol exceptions and exception handling procedures.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PROTOCOL_EXCEPTIONS

	iii.	RCS Log

$Log: protoexn.fun,v $
Revision 1.2  1995/08/29  14:11:06  esb
added exception Receive.

Revision 1.1  1995/06/20  17:09:33  esb
Initial revision


	1.	functor Protocol_Exceptions
*)

functor Protocol_Exceptions (structure B: FOX_BASIS): PROTOCOL_EXCEPTIONS =
 struct
  exception Session of string    (* failure in session *)
  exception Passive of string    (* failure in passive or stop *)
  exception Connection of string (* failure in connect or abort *)
  exception Send of string       (* failure in send *)
  exception Receive of string		(* failure in packet delivery *)

  fun print ({module, exn}, message) =
       let val name =
	        case exn of
		   Session s => "Session (" ^ s ^ ")"
		 | Passive s => "Passive (" ^ s ^ ")"
		 | Connection s => "Connection (" ^ s ^ ")"
		 | Send s => "Send (" ^ s ^ ")"
		 | Receive s => "Receive (" ^ s ^ ")"
		 | Internal_Error s => "unexpected internal error (" ^ s ^ ")"
		 | _ => System.exn_name exn
	   val tail = case message of
	                 NONE => "\n"
		       | SOME s => ", " ^ s ^ "\n"
	   val string = module ^ ": exception " ^ name ^ tail
       in B.V.Print.print string
       end

  fun print_exception arg = print (arg, NONE)
       
  fun print_raise (arg as {module, exn}) =
       (print (arg, NONE);
	raise exn)

  fun print_raise_again (arg as {module, exn}) =
       (print (arg, SOME "raised by lower protocol");
	raise exn)

 end (* struct *)

