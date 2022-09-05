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

	Implements the externalizing of remote procedures
	for installation on Sun RPC server
		
	ii.	Table of Contents

	1.      functor Wrap

	iii.	RCS Log

$Log: wrap.fun,v $
Revision 1.1  1994/10/14  12:00:01  kcchiang
Initial revision

*)
(*
	1.      functor Wrap
*)
functor Wrap (structure Arg : XDR
	      structure Res : XDR
	      sharing type Arg.extern = Res.extern = bytestring) : WRAP =
struct
  structure Arg = Arg
  structure Res = Res

  fun externalize f arg =
      let val (arguments,_)  = Arg.unmarshall arg
	  val result         = f arguments
      in
	  Res.marshall (result, (ByteArray.array (Res.size result,0), 0))
      end
end
