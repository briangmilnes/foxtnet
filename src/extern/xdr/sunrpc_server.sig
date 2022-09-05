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
	
	signature for SUN RPC server

	ii.	Table of Contents

	1.      signature SUNRPC_SERVER

	iii.	RCS Log

$Log: sunrpc_server.sig,v $
Revision 1.1  1994/10/14  12:00:00  kcchiang
Initial revision

*)

(*
        1.      signature SUNRPC_SERVER
*)
signature SUNRPC_SERVER =
sig
    type Proc_ID
    type Remote_Function

    exception ProcAlreadyExists
    exception NoSuchProc

    (* for debugging purposes ONLY *)
    val print_table     : unit -> unit
    val print_ver_table : unit -> unit

    exception InitializationFailed

    val initialize : unit -> int
    val finalize   : unit -> int
    val start      : int  -> unit

    val install   : Proc_ID -> Remote_Function -> unit
    val uninstall : Proc_ID -> unit
end
