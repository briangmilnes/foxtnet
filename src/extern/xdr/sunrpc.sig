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
		
	This is the big XDR structure for the SUN RPC protocol.

	ii.	Table of Contents

	1.      signature SUN_RPC_XDR

	iii.	RCS Log

$Log: sunrpc.sig,v $
Revision 1.2  1995/05/02  23:36:59  cstone
no change.

Revision 1.1  1994/10/14  11:59:59  kcchiang
Initial revision

*)

(*
        1.      signature SUN_RPC_XDR
*)
signature SUN_RPC_XDR =
sig
    include XDR
    sharing type extern = bytestring

    val version : int

    structure Body              : UNION2_XDR
    structure RejectedReply     : UNION2_XDR
    structure ReplyData         : UNION3d_XDR
    structure ReplyBody         : UNION2_XDR

    structure accept_stat :
	sig
	    val GARBAGE_ARGS : int
	    val PROC_UNAVAIL : int
	    val PROG_MISMATCH: int
	    val PROG_UNAVAIL : int
	    val SUCCESS      : int
	end

    structure auth_flavor :
	sig
	    val AUTH_DES  : int
	    val AUTH_NULL : int
	    val AUTH_SHORT: int
	    val AUTH_UNIX : int
	end

    structure auth_stat :
	sig
	    val AUTH_BADCRED : int
	    val AUTH_BADVERF : int
	    val AUTH_REJECTEDCRED : int
	    val AUTH_REJECTEDVERF : int
	    val AUTH_TOOWEAK : int
	end

    structure msg_type :
	sig
	    val CALL  : int
	    val REPLY : int
	end

    structure reject_stat :
	sig
	    val AUTH_ERROR   : int
	    val RPC_MISMATCH : int
	end

    structure reply_stat :
	sig
	    val MSG_ACCEPTED : int
	    val MSG_DENIED : int
	end
end
