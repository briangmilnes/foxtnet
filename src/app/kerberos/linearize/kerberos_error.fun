(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This provides the ability to marshall and unmarshall the
	error codes provided by the Kerberos server.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Kerberos_Error
	2.	functor Kerberos_Error_Linearize

		iii.	RCS Log
	
$Log: kerberos_error.fun,v $
Revision 1.1  1994/08/25  10:24:38  robby
Initial revision

Revision 1.1  1994/07/14  20:29:32  robby
Initial revision

Revision 1.1  94/07/13  18:45:32  robby
Initial revision


	1.	functor Kerberos_Error
*)
functor Kerberos_Error():KERBEROS_ERROR=
struct
  datatype kerberos_error=
    OK | Unknown_Error |

    KDC_Name_Exp | KDC_Service_Exp | KDC_Auth_Exp | KDC_Pkt_Ver | 
    KDC_P_MKey_Ver | KDC_S_MKey_Ver | KDC_Byte_Order | 
    KDC_Pr_Unknown | KDC_Pr_N_Unique | KDC_Null_Key | 
    KDC_Gen_Err |

    GC_Tkt_Fil | GC_No_Tkt |

    MK_AP_Tgt_Exp |

    RD_AP_Undec | RD_AP_Exp | RD_AP_NYV | RD_AP_Repeat | RD_AP_Not_Us | 
    RD_AP_Incon | RD_AP_Time | RD_AP_Badd | RD_AP_Version | RD_AP_Msg_Type |
    RD_AP_Modified | RD_AP_Order | RD_AP_Unauthor |
    
    GT_PW_Null | GT_PW_Bad_PW | GT_PW_Prot | GT_PW_KDC_Err | GT_PW_Null_Tkt |

    SKDC_Retry | SKDC_Cant |

    INTK_W_Not_All | INTK_Bad_PW | INTK_INTK_Prot | INTK_INTK_Err |
    
    AD_No_Tgt |
    
    No_TKT_FIL | TKT_FIL_Acc | TKT_FIL_Lck | TKT_FIL_Fmt | TKT_FIL_Ini |

    KNAME_Fmt | 

    SAFE_PRIV_Error

    fun makestring OK="request ok"
      | makestring KDC_Name_Exp="principal expired"
      | makestring KDC_Service_Exp="service expired"
      | makestring KDC_Auth_Exp="authorization expired"
      | makestring KDC_Pkt_Ver="protocol version unknown"
      | makestring KDC_P_MKey_Ver="wrong master key version"
      | makestring KDC_S_MKey_Ver="wrong master key version"
      | makestring KDC_Byte_Order="byte order unknown"
      | makestring KDC_Pr_Unknown="principal unknown"
      | makestring KDC_Pr_N_Unique="principal not unique"
      | makestring KDC_Null_Key="principal has a null key"
      | makestring KDC_Gen_Err="generic error from KDC"
      | makestring GC_Tkt_Fil="can't read ticket file"
      | makestring GC_No_Tkt="can't find ticket or ticket granting ticket"
      | makestring MK_AP_Tgt_Exp="ticket granting ticket expired"
      | makestring RD_AP_Undec="can't decode authenticator"
      | makestring RD_AP_Exp="ticket expired"
      | makestring RD_AP_NYV="ticket not yet valid"
      | makestring RD_AP_Repeat="repeted request"
      | makestring RD_AP_Not_Us="ticket sent to wrong KDC"
      | makestring RD_AP_Incon="request inconsistant"
      | makestring RD_AP_Time="delta_t too big"
      | makestring RD_AP_Badd="incorrect net address"
      | makestring RD_AP_Version="protocol version mismatch"
      | makestring RD_AP_Msg_Type="invalid message type"
      | makestring RD_AP_Modified="message stream modified"
      | makestring RD_AP_Order="message out of order"
      | makestring RD_AP_Unauthor="unauthorized request"
      | makestring GT_PW_Null="current password is null"
      | makestring GT_PW_Bad_PW="incorrect current password"
      | makestring GT_PW_Prot="protocol error"
      | makestring GT_PW_KDC_Err="error returned by KDC"
      | makestring GT_PW_Null_Tkt="null ticket returned by KDC"
      | makestring SKDC_Retry="retry count exceeded"
      | makestring SKDC_Cant="can't send request"
      | makestring INTK_W_Not_All="not all tickets returned"
      | makestring INTK_Bad_PW="incorrect password"
      | makestring INTK_INTK_Prot="protocol error"
      | makestring INTK_INTK_Err="other error from get_intkt"
      | makestring AD_No_Tgt="don't have ticket granting ticket"
      | makestring No_TKT_FIL="no ticket file found"
      | makestring TKT_FIL_Acc="couldn't access ticket file"
      | makestring TKT_FIL_Lck="couldn't lock ticket file"
      | makestring TKT_FIL_Fmt="bad ticket file format"
      | makestring TKT_FIL_Ini="tf_init not called first"
      | makestring KNAME_Fmt="bad kerberos name format"
      | makestring SAFE_PRIV_Error="syscall error"
      | makestring Unknown_Error="unrecognised error"
end

(*
	2.	functor Kerberos_Error_Linearize
*)
functor Kerberos_Error_Linearize(structure Error:KERBEROS_ERROR
				 structure Int:INT_LINEARIZE)
  :KERBEROS_ERROR_LINEARIZE=
struct

  datatype extern=Extern of ByteArray.bytearray * int
  type incoming=extern
  type outgoing=extern
  exception Does_Not_Match

  fun size _=4

  open Error
  type T=kerberos_error

  fun krb_err_to_int OK=0
    | krb_err_to_int KDC_Name_Exp=1
    | krb_err_to_int KDC_Service_Exp=2
    | krb_err_to_int KDC_Auth_Exp=3
    | krb_err_to_int KDC_Pkt_Ver=4
    | krb_err_to_int KDC_P_MKey_Ver=5
    | krb_err_to_int KDC_S_MKey_Ver=6
    | krb_err_to_int KDC_Byte_Order=7
    | krb_err_to_int KDC_Pr_Unknown=8
    | krb_err_to_int KDC_Pr_N_Unique=9
    | krb_err_to_int KDC_Null_Key=10
    | krb_err_to_int KDC_Gen_Err=20
    | krb_err_to_int GC_Tkt_Fil=21
    | krb_err_to_int GC_No_Tkt=22
    | krb_err_to_int MK_AP_Tgt_Exp=26
    | krb_err_to_int RD_AP_Undec=31
    | krb_err_to_int RD_AP_Exp=32
    | krb_err_to_int RD_AP_NYV=33
    | krb_err_to_int RD_AP_Repeat=34
    | krb_err_to_int RD_AP_Not_Us=35
    | krb_err_to_int RD_AP_Incon=36
    | krb_err_to_int RD_AP_Time=37
    | krb_err_to_int RD_AP_Badd=38
    | krb_err_to_int RD_AP_Version=39
    | krb_err_to_int RD_AP_Msg_Type=40
    | krb_err_to_int RD_AP_Modified=41
    | krb_err_to_int RD_AP_Order=42
    | krb_err_to_int RD_AP_Unauthor=43
    | krb_err_to_int GT_PW_Null=51
    | krb_err_to_int GT_PW_Bad_PW=52
    | krb_err_to_int GT_PW_Prot=53
    | krb_err_to_int GT_PW_KDC_Err=54
    | krb_err_to_int GT_PW_Null_Tkt=55
    | krb_err_to_int SKDC_Retry=56
    | krb_err_to_int SKDC_Cant=57
    | krb_err_to_int INTK_W_Not_All=61
    | krb_err_to_int INTK_Bad_PW=62
    | krb_err_to_int INTK_INTK_Prot=63
    | krb_err_to_int INTK_INTK_Err=70
    | krb_err_to_int AD_No_Tgt=71
    | krb_err_to_int No_TKT_FIL=76
    | krb_err_to_int TKT_FIL_Acc=77
    | krb_err_to_int TKT_FIL_Lck=78
    | krb_err_to_int TKT_FIL_Fmt=79
    | krb_err_to_int TKT_FIL_Ini=80
    | krb_err_to_int KNAME_Fmt=81
    | krb_err_to_int SAFE_PRIV_Error= ~1
    | krb_err_to_int Unknown_Error=raise Does_Not_Match

  fun int_to_krb_err 0=OK
    | int_to_krb_err 1=KDC_Name_Exp
    | int_to_krb_err 2=KDC_Service_Exp
    | int_to_krb_err 3=KDC_Auth_Exp
    | int_to_krb_err 4=KDC_Pkt_Ver
    | int_to_krb_err 5=KDC_P_MKey_Ver
    | int_to_krb_err 6=KDC_S_MKey_Ver
    | int_to_krb_err 7=KDC_Byte_Order
    | int_to_krb_err 8=KDC_Pr_Unknown
    | int_to_krb_err 9=KDC_Pr_N_Unique
    | int_to_krb_err 10=KDC_Null_Key
    | int_to_krb_err 20=KDC_Gen_Err
    | int_to_krb_err 21=GC_Tkt_Fil
    | int_to_krb_err 22=GC_No_Tkt
    | int_to_krb_err 26=MK_AP_Tgt_Exp
    | int_to_krb_err 31=RD_AP_Undec
    | int_to_krb_err 32=RD_AP_Exp
    | int_to_krb_err 33=RD_AP_NYV
    | int_to_krb_err 34=RD_AP_Repeat
    | int_to_krb_err 35=RD_AP_Not_Us
    | int_to_krb_err 36=RD_AP_Incon
    | int_to_krb_err 37=RD_AP_Time
    | int_to_krb_err 38=RD_AP_Badd
    | int_to_krb_err 39=RD_AP_Version
    | int_to_krb_err 40=RD_AP_Msg_Type
    | int_to_krb_err 41=RD_AP_Modified
    | int_to_krb_err 42=RD_AP_Order
    | int_to_krb_err 43=RD_AP_Unauthor
    | int_to_krb_err 51=GT_PW_Null
    | int_to_krb_err 52=GT_PW_Bad_PW
    | int_to_krb_err 53=GT_PW_Prot
    | int_to_krb_err 54=GT_PW_KDC_Err
    | int_to_krb_err 55=GT_PW_Null_Tkt
    | int_to_krb_err 56=SKDC_Retry
    | int_to_krb_err 57=SKDC_Cant
    | int_to_krb_err 61=INTK_W_Not_All
    | int_to_krb_err 62=INTK_Bad_PW
    | int_to_krb_err 63=INTK_INTK_Prot
    | int_to_krb_err 70=INTK_INTK_Err
    | int_to_krb_err 71=AD_No_Tgt
    | int_to_krb_err 76=No_TKT_FIL
    | int_to_krb_err 77=TKT_FIL_Acc
    | int_to_krb_err 78=TKT_FIL_Lck
    | int_to_krb_err 79=TKT_FIL_Fmt
    | int_to_krb_err 80=TKT_FIL_Ini
    | int_to_krb_err 81=KNAME_Fmt
    | int_to_krb_err ~1=SAFE_PRIV_Error
    | int_to_krb_err _=Unknown_Error

  fun marshall (err,Extern (array,pos))=
    let val int=krb_err_to_int err
      val (Int.Extern(array,pos))=Int.marshall (int,Int.Extern (array,pos))
    in
      Extern(array,pos)
    end

  fun unmarshall (Extern (array,pos))=
    let val (int,Int.Extern(array,pos))=Int.unmarshall(Int.Extern (array,pos))
      handle Int.Does_Not_Match => raise Does_Not_Match
      val err=int_to_krb_err int
    in
      (err,Extern(array,pos))
    end

end
