(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Sidd Puri (puri@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	Definition of data to be sent and received by telnet.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TELNET_DATA
	2.	substructure Option
	3.	type data

		iii.	RCS Log
	
$Log: telnetdata.sig,v $
Revision 1.1  1996/07/05  17:43:35  esb
Initial revision


		1.	signature TELNET_DATA

	The TELNET_DATA signature defines the interface of the
	Telnet_Data structure, which is used by the telnet functor to
	provide Incoming and Outgoing structures.  This signature
	includes (inherits from) the EXTERNAL signature.
*)

signature TELNET_DATA =
 sig

  include EXTERNAL

(*
		2.	substructure Option
*)

  structure Option: TELNET_OPTION

(*
		3.	type data
*)

  datatype element =
      Text of string
    | Data of Word_Array.T	(* output only, not returned by unmarshal *)
    | Synch
    | No_Op			(* telnet NUL: output only *)
    | Break			(* telnet BRK *)
    | Interrupt_Process		(* telnet IP *)
    | Abort_Output		(* telnet AO *)
    | Are_You_There		(* telnet AYT *)
    | Erase_Char		(* telnet EC *)
    | Erase_Line		(* telnet EL *)
    | Go_Ahead			(* telnet GA *)
    | Will of Option.option_type
    | Won't of Option.option_type
    | Do of Option.option_type
    | Don't of Option.option_type
    | Option_Value of Option.option_value
    | Incomplete_Parse of Word_Array.T	(* incoming only *)

  type data = element list
  sharing type T = data

 end
