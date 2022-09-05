(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (stone+@cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

		i.	Abstract
		ii.	Table of Contents
		iii.	RCS Log
		1.	signature SERVER

		iii.	RCS Log
	
$Log: httpd.sig,v $
# Revision 1.2  1995/03/08  20:23:28  cstone
# Added functions to get server statistics.
#

*)

(*
------------------------------------------------------------------------
		1.	signature SERVER
*)

signature SERVER =
sig
    type server_id

    val install    : unit -> server_id
    val uninstall  : server_id -> int
    val install_fn : string -> (string->(string*string) list->string) -> unit

    val uptime     : unit -> int
    val bytesout   : unit -> FoxWord64.word
    val accesses   : unit -> int
end
