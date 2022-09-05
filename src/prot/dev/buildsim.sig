(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a signature for ethernet device simulators.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature SIMULATORS

		iii.	RCS Log
	
$Log: buildsim.sig,v $
Revision 1.3  1995/09/25  16:51:50  cline
added Sly_Meter and Snow_Meter

Revision 1.2  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.1  1994/06/07  16:31:56  robby
Initial revision


		1.	signature SIMULATORS
*)

signature SIMULATORS =
 sig
  structure Basic: DEVICE_PROTOCOL
  structure Cobol: DEVICE_PROTOCOL
  structure Crafty: DEVICE_PROTOCOL
  structure Fortran: DEVICE_PROTOCOL
  structure Maclisp: DEVICE_PROTOCOL
  structure Quick: DEVICE_PROTOCOL
  structure Sly: DEVICE_PROTOCOL
  structure Snow: DEVICE_PROTOCOL
  structure Vixen: DEVICE_PROTOCOL
  structure Wagosh: DEVICE_PROTOCOL
  structure Sly_Meter: DEVICE_PROTOCOL
  structure Snow_Meter: DEVICE_PROTOCOL
 end 
