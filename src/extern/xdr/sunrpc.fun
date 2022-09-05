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

	XDR structure for SUN RPC version 2 message protocol.

	ii.	Table of Contents

	1.      functor SunRPC

	iii.	RCS Log

$Log: sunrpc.fun,v $
Revision 1.2  1995/05/02  23:37:14  cstone
Changed parameters for XUnion3_Default.

Revision 1.1  1994/10/14  11:59:58  kcchiang
Initial revision

*)

(*
        1.      functor SunRPC
*)
functor SunRPC (structure B : FOX_BASIS
                structure Remote_Proc_Params : EXTERN
		structure Remote_Proc_Result : EXTERN
                sharing type Remote_Proc_Params.extern =
                             Remote_Proc_Result.extern =
                             B.Dyn_Array.T
                sharing type Remote_Proc_Params.cursor =
                             Remote_Proc_Result.cursor =
                             B.Dyn_Array.T)
               :SUN_RPC_XDR  =
struct

(* SUN RPC Version number *)
    val version = 2


(* Basic XDR structures declaration
   -------------------------------- *)
    structure D = B.Dyn_Array

    structure XInt = XInt (structure D = D
                           structure B4 = B.Order.B4)
    structure XVoid = XVoid (structure D = D)


(* Enumerations declarations
   ------------------------- *)
(* 
       1. RPC message type 
*)
    structure msg_type =
	struct
	    val range = (0,1)
	    val CALL  = 0
	    val REPLY = 1
	end

    structure XMsgType = XEnum (structure Int = XInt
				val range = msg_type.range)


(*
       2. RPC reply message status
*)
    structure reply_stat =
	struct
	    val range        = (0,1)
	    val MSG_ACCEPTED = 0
	    val MSG_DENIED   = 1
	end

    structure XReplyStat = XEnum (structure Int = XInt
				  val range = reply_stat.range)


(*
       3. RPC message accept status
*)
    structure accept_stat =
	struct
	    val range         = (0,4)
	    val SUCCESS       = 0	 (* RPC executed successfully *)
	    val PROG_UNAVAIL  = 1	 (* remote hasn't exported progam *)
	    val PROG_MISMATCH = 2	 (* remote can't support version *)
	    val PROC_UNAVAIL  = 3	 (* program can't support proc *)
	    val GARBAGE_ARGS  = 4	 (* procedure can't decode params *)
	end

    structure XAcceptStat = XEnum (structure Int = XInt
				   val range = accept_stat.range)


(*
        4. RPC message reject status
*)
    structure reject_stat =
	struct
	    val range        = (0,1)
	    val RPC_MISMATCH = 0	 (* RPC version != 2 *)
	    val AUTH_ERROR   = 1	 (* remote can't authenticate caller *)
	end

    structure XRejectStat = XEnum (structure Int = XInt
				   val range = reject_stat.range)


(*
        5. RPC Authentication status
*)
    structure auth_stat =
	struct
	    val range             = (1,5)
	    val AUTH_BADCRED      = 1	   (* bad credentials *)
	    val AUTH_REJECTEDCRED = 2	   (* client must begin new session *)
	    val AUTH_BADVERF      = 3	   (* bad verifier *)
	    val AUTH_REJECTEDVERF = 4	   (* verifier expired or replayed *)
	    val AUTH_TOOWEAK      = 5	   (* rejected for security reasons *)
	end

    structure XAuthStat = XEnum (structure Int = XInt
				 val range = auth_stat.range)


(*
        6. RPC Authentication flavor
*)
    structure auth_flavor =
	struct
	    val range      = (0,3)
	    val AUTH_NULL  = 0
	    val AUTH_UNIX  = 1
	    val AUTH_SHORT = 2
	    val AUTH_DES   = 3
	end

    structure XAuthFlavor = XEnum (structure Int = XInt
				   val range = auth_flavor.range)


(* RPC structures declaration 
   -------------------------- *)
    structure XBody1        = XOpaque (structure Int = XInt
                                       structure D = D)

    structure XOpaqueAuth   = XStruct2 (structure X1 = XAuthFlavor
				        structure X2 = XBody1)

    structure XMismatchInfo = XStruct2 (structure X1 = XInt  (* low  *)
				        structure X2 = XInt) (* high *)

    structure XReplyData = 
      XUnion3_Default (structure Variant_1 = Remote_Proc_Result
		       structure Variant_2 = XMismatchInfo
		       structure Variant_3 = XVoid
		       structure Enum      = XAcceptStat
		       val discriminants   = (accept_stat.SUCCESS,
					      accept_stat.PROG_MISMATCH))

    structure XAcceptedReply = 
       XStruct2 (structure X1 = XOpaqueAuth
		 structure X2 = XReplyData)

    structure XRejectedReply = 
       XUnion2 (structure Variant_1 = XMismatchInfo
		structure Variant_2 = XAuthStat
		structure Enum      = XRejectStat
		val discriminants = (reject_stat.RPC_MISMATCH,
			             reject_stat.AUTH_ERROR))

    structure XReplyBody = 
       XUnion2 (structure Variant_1 = XAcceptedReply
	        structure Variant_2 = XRejectedReply
		structure Enum = XReplyStat
		val discriminants = (reply_stat.MSG_ACCEPTED,
				     reply_stat.MSG_DENIED))

    structure XCallBody = XStruct7 (structure X1 = XInt (* rpcvers = 2 *)
				    structure X2 = XInt        (* prog *)
				    structure X3 = XInt        (* vers *)
				    structure X4 = XInt        (* proc *)
				    structure X5 = XOpaqueAuth (* cred *)
				    structure X6 = XOpaqueAuth (* verf *)
				    structure X7 = Remote_Proc_Params)
	
    structure XBody = XUnion2 (structure Variant_1 = XCallBody
			       structure Variant_2 = XReplyBody
			       structure Enum = XMsgType
			       val discriminants = (msg_type.CALL, 
                                                    msg_type.REPLY))
	
    structure XRpcMsg = XStruct2 (structure X1 = XInt (* xid *)
				  structure X2 = XBody)

   (* end of SunRPC declarations *)

    open XRpcMsg

    (* aliases *)
    structure Body = XBody
    structure ReplyBody = XReplyBody
    structure ReplyData = XReplyData
    structure RejectedReply = XRejectedReply


end (* functor *)















