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

	A signature for a subset of FTP (see RFC 959).  This signature
	describes a module intended to be used by other programs, not
	directly by the user.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FTP

		iii.	RCS Log

$Log: ftp.sig,v $
Revision 1.2  1994/10/27  10:25:28  cokasaki
better dns support, added Remote_Operation_Failed exception

Revision 1.1  94/08/25  11:48:19  cokasaki
Initial revision
	

*)

(*
	1.	signature FTP
*)

signature FTP =
sig

  exception Initialization_Failed of string

  val initialize : unit -> int
  val finalize   : unit -> int

  exception Connection_Failed of string

  exception Remote_Operation_Failed of string

  val connect :
      string (* hostname *)
        -> {get   : string (* filename *) -> outstream -> unit,
	    ls    : string option (* dirname *) -> outstream -> unit,
	    cd    : string (* dirname *) -> unit,
	    quit  : unit -> unit}

end

