(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	This is used for externalizing remote procedures.

	ii.	Table of Contents

        1.      signature WRAP

	iii.	RCS Log

$Log: wrap.sig,v $
Revision 1.1  1994/10/14  12:00:02  kcchiang
Initial revision

*)
(*
        1.      signature WRAP
*)
signature WRAP =
sig
  structure Arg : XDR
  structure Res : XDR

  val externalize : (Arg.T -> Res.T) -> (Arg.extern -> Res.extern)
end
