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

	This signature specializes LINEARIZE to the Kerberos
	server's errors.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature KERBEROS_ERROR_LINEARIZE

		iii.	RCS Log
	
$Log: kerberos_error.sig,v $
Revision 1.1  1994/08/25  10:24:39  robby
Initial revision

Revision 1.1  1994/07/14  20:29:37  robby
Initial revision

Revision 1.1  94/07/13  18:45:33  robby
Initial revision


	1.	signature KERBEROS_ERROR_LINEARIZE
*)

signature KERBEROS_ERROR=
sig
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

  val makestring : kerberos_error -> string
end

signature KERBEROS_ERROR_LINEARIZE=
sig
  include LINEARIZE
  include KERBEROS_ERROR

  sharing type T=kerberos_error
end

(* from /usr/src/usr/misc/.kerberos/src/include/krb.h revision 4.9.2.3
/* Error codes returned from the KDC */
#define		KDC_OK		0	/* Request OK */
#define		KDC_NAME_EXP	1	/* Principal expired */
#define		KDC_SERVICE_EXP	2	/* Service expired */
#define		KDC_AUTH_EXP	3	/* Auth expired */
#define		KDC_PKT_VER	4	/* Protocol version unknown */
#define		KDC_P_MKEY_VER	5	/* Wrong master key version */
#define		KDC_S_MKEY_VER 	6	/* Wrong master key version */
#define		KDC_BYTE_ORDER	7	/* Byte order unknown */
#define		KDC_PR_UNKNOWN	8	/* Principal unknown */
#define		KDC_PR_N_UNIQUE 9	/* Principal not unique */
#define		KDC_NULL_KEY   10	/* Principal has null key */
#define		KDC_GEN_ERR    20	/* Generic error from KDC */


/* Values returned by get_credentials */
#define		GC_OK		0	/* Retrieve OK */
#define		RET_OK		0	/* Retrieve OK */
#define		GC_TKFIL       21	/* Can't read ticket file */
#define		RET_TKFIL      21	/* Can't read ticket file */
#define		GC_NOTKT       22	/* Can't find ticket or TGT */
#define		RET_NOTKT      22	/* Can't find ticket or TGT */


/* Values returned by mk_ap_req	 */
#define		MK_AP_OK	0	/* Success */
#define		MK_AP_TGTEXP   26	/* TGT Expired */

/* Values returned by rd_ap_req */
#define		RD_AP_OK	0	/* Request authentic */
#define		RD_AP_UNDEC    31	/* Can't decode authenticator */
#define		RD_AP_EXP      32	/* Ticket expired */
#define		RD_AP_NYV      33	/* Ticket not yet valid */
#define		RD_AP_REPEAT   34	/* Repeated request */
#define		RD_AP_NOT_US   35	/* The ticket isn't for us */
#define		RD_AP_INCON    36	/* Request is inconsistent */
#define		RD_AP_TIME     37	/* delta_t too big */
#define		RD_AP_BADD     38	/* Incorrect net address */
#define		RD_AP_VERSION  39	/* protocol version mismatch */
#define		RD_AP_MSG_TYPE 40	/* invalid msg type */
#define		RD_AP_MODIFIED 41	/* message stream modified */
#define		RD_AP_ORDER    42	/* message out of order */
#define		RD_AP_UNAUTHOR 43	/* unauthorized request */

/* Values returned by get_pw_tkt */
#define		GT_PW_OK	0	/* Got password changing tkt */
#define		GT_PW_NULL     51	/* Current PW is null */
#define		GT_PW_BADPW    52	/* Incorrect current password */
#define		GT_PW_PROT     53	/* Protocol Error */
#define		GT_PW_KDCERR   54	/* Error returned by KDC */
#define		GT_PW_NULLTKT  55	/* Null tkt returned by KDC */


/* Values returned by send_to_kdc */
#define		SKDC_OK		0	/* Response received */
#define		SKDC_RETRY     56	/* Retry count exceeded */
#define		SKDC_CANT      57	/* Can't send request */

/*
 * Values returned by get_intkt
 * (can also return SKDC_* and KDC errors)
 */

#define		INTK_OK		0	/* Ticket obtained */
#define		INTK_W_NOTALL  61	/* Not ALL tickets returned */
#define		INTK_BADPW     62	/* Incorrect password */
#define		INTK_PROT      63	/* Protocol Error */
#define		INTK_ERR       70	/* Other error */

/* Values returned by get_adtkt */
#define         AD_OK           0	/* Ticket Obtained */
#define         AD_NOTGT       71	/* Don't have tgt */

/* Error codes returned by ticket file utilities */
#define		NO_TKT_FIL	76	/* No ticket file found */
#define		TKT_FIL_ACC	77	/* Couldn't access tkt file */
#define		TKT_FIL_LCK	78	/* Couldn't lock ticket file */
#define		TKT_FIL_FMT	79	/* Bad ticket file format */
#define		TKT_FIL_INI	80	/* tf_init not called first */

/* Error code returned by kparse_name */
#define		KNAME_FMT	81	/* Bad Kerberos name format */

/* Error code returned by krb_mk_safe */
#define		SAFE_PRIV_ERROR	-1	/* syscall error */
*)
