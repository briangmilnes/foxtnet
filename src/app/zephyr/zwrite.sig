(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robby Findler (Robert.Findler@cs.cmu.edu)
	Daniel Wang (Daniel.Wang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	ZWRITE is a signature for a simple zephyr sender.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ZWRITE

		iii.	RCS Log
	
$Log: zwrite.sig,v $
Revision 1.2  1994/09/03  21:10:12  danwang
Changed zwrite to take a record as an argument.

Revision 1.1  1994/07/14  16:27:24  milnes
Initial revision


		1.	signature ZWRITE
*)


signature ZWRITE =
sig
 (* Send a zephyr message. *)
    val zwrite : {to:string,from:string,message:string} -> unit 
end

