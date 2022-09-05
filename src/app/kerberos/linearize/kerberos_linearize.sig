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

	A specialization of the signature LINEARIZE to
	Kerberos packets

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature KERBEROS_LINEARIZE

		iii.	RCS Log
	
$Log: kerberos_linearize.sig,v $
Revision 1.1  1994/08/25  10:24:39  robby
Initial revision

Revision 1.2  1994/08/08  15:14:36  danwang
Made changes so authentication with zephyr works.

Revision 1.1  1994/07/14  20:29:40  robby
Initial revision

Revision 1.1  94/07/13  18:48:48  robby
Initial revision


		1.	signature KERBEROS_LINEARIZE
*)

signature KERBEROS_LINEARIZE=
sig
  include LINEARIZE

  structure Error:KERBEROS_ERROR

  (* These types are from
     v4draft-rfc.txt about Kerberos 
     and
     Section E.2.1, Kerberos Authentication and Authorization System
     from the Project Athena Technical Plan *)
  datatype kerberos_message =

    Auth_Request of {name_p:string, instance_p:string, realm_p:string,
		     time_sec:ubyte4,lifetime_s:ubyte1,
		     name_s:string,instance_s:string}  

  | Auth_Reply of {name_p:string, instance_p:string, realm_p:string,
		   time_sec:ubyte4, exp_date_p:ubyte4,n_tickets:ubyte1,
		   kvno_p:ubyte1, credentials:ByteArray.bytearray,
		   swap_bytes:bool} 

  | Err_Reply of {name_p:string, instance_p:string, realm_p:string,
		  time_sec:ubyte4, err_code:Error.kerberos_error,
		  err_text:string} 

  | Appl_Request of {kvno_s:ubyte1, realm_s:string,
		     ticket:ByteArray.bytearray,
		     authenticator:ByteArray.bytearray}

  | Appl_Err of {err_code:Error.kerberos_error, err_text:string}
    
  | Tgs_Request of {kvno_s:ubyte1, realm_s:string, ticket:ByteArray.bytearray,
		    authenticator:ByteArray.bytearray, time_sec:ubyte4,
	            lifetime_s:ubyte1,name_s:string, instance_s:string}

  sharing type T=kerberos_message
end

