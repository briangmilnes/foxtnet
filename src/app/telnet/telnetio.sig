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
	1.	signature TELNET_IO
	2.	substructure Data
	3.	types outgoing, incoming

		iii.	RCS Log
	
$Log: telnetio.sig,v $
Revision 1.2  1996/07/10  21:22:39  esb
added a parameter to unmarshal, recording the state of CR processing.

Revision 1.1  1996/07/05  17:44:37  esb
Initial revision


		1.	signature TELNET_IO

	The TELNET_IO signature defines the interface of the TelnetIO
	structure, which is used by telnet to marshal outgoing data
	and unmarshal incoming data.  The format of data is defined
	by Telnet_Data.  TELNET_IO inherits from EXTERN.
*)

signature TELNET_IO =
 sig

  (* TELNET_IO inherits from EXTERN *)
  include EXTERN

(*
		2.	substructure Data
*)

  structure Data: TELNET_DATA
    sharing type T = Data.T

(*
		3.	types outgoing, incoming
*)

  type lower_extern_out
  type lower_extern_in

  datatype urgency = Regular | Urgent
  datatype cr_state = No_CR | CR_Was_Last_Char

  type outgoing = lower_extern_out * urgency ref
  type incoming = lower_extern_in * urgency * cr_state ref *
                  Word_Array.T option	(* any unparsed prior data *)

  sharing type extern_out = outgoing
  sharing type extern_in = incoming

 end
