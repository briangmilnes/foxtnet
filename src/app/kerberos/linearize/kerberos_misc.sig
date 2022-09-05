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

	These are specializations of the LINEARIZE signature for the
	different Kerberos packet types, Kerberos authenticators and
	Kerberos ciphers.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature KERBEROS_AUTHENTICATOR_LINEARIZE
	2.	signature KERBEROS_CREDENTIALS_LINEARIZE
	3.	signature KERBEROS_AUTH_REQUEST_LINEARIZE
	4.	signature KERBEROS_AUTH_REPLY_LINEARIZE
	5.	signature KERBEROS_ERR_REPLY_LINEARIZE
	6.	signature KERBEROS_APPL_REQUEST_LINEARIZE
	7.	signature KERBEROS_APPL_REQUEST_LINEARIZE
	8.	signature KERBEROS_TICKET_GRANTING_REQUEST_LINEARIZE 

		iii.	RCS Log
	
$Log: kerberos_misc.sig,v $
Revision 1.1  1994/08/25  10:24:40  robby
Initial revision

Revision 1.2  1994/08/08  15:16:32  danwang
Made changes so authentication for zephyr works.

Revision 1.1  1994/07/14  20:29:44  robby
Initial revision

Revision 1.1  94/07/13  18:48:51  robby
Initial revision


	1.	signature KERBEROS_AUTHENTICATOR_LINEARIZE
*)


signature KERBEROS_AUTHENTICATOR_LINEARIZE =
sig
    include LINEARIZE

    datatype authenticator = Authenticator of {name_p:string,
					   instance_p:string,
					      realm_p:string,
					     checksum:ubyte4, 
					      time5ms:ubyte1,
					     time_sec:ubyte4}
    sharing type T = authenticator
end

(*
	2.	signature KERBEROS_CREDENTIALS_LINEARIZE
*)
signature KERBEROS_CREDENTIALS_LINEARIZE =
sig
    include LINEARIZE
    structure Des:DES'

    datatype credential = Credentials of {session_key:Des.key,
				               name_s:string, 
				           instance_s:string,
				              realm_s:string, 
				           lifetime_s:ubyte1,
				               kvno_s:ubyte1,
				               ticket:ByteArray.bytearray,
			                time_sec_kkds:ubyte4}
    sharing type T = credential
end
(*
	3.	signature KERBEROS_AUTH_REQUEST_LINEARIZE
*)

signature KERBEROS_AUTH_REQUEST_LINEARIZE =
sig
    include LINEARIZE

    datatype kerberos_auth_request = Auth_Request of {name_p:string, 
						  instance_p:string, 
						     realm_p:string,
						    time_sec:ubyte4,
						  lifetime_s:ubyte1,
						      name_s:string,
						  instance_s:string}  
    sharing type T = kerberos_auth_request

end
(*
	4.	signature KERBEROS_AUTH_REPLY_LINEARIZE
*)

signature KERBEROS_AUTH_REPLY_LINEARIZE =
sig
    include LINEARIZE

    datatype kerberos_auth_reply = Auth_Reply of {name_p:string, 
					      instance_p:string, 
					         realm_p:string,
					        time_sec:ubyte4, 
					       n_tickets:ubyte1,
					      exp_date_p:ubyte4,
					      	  kvno_p:ubyte1, 
					     credentials:ByteArray.bytearray} 
    sharing type T = kerberos_auth_reply
end

(*
	5.	signature KERBEROS_ERR_REPLY_LINEARIZE
*)
signature KERBEROS_ERR_REPLY_LINEARIZE =
sig
    include LINEARIZE
    structure Error:KERBEROS_ERROR

    datatype kerberos_err_reply =  Err_Reply of {name_p:string,
					     instance_p:string,
					        realm_p:string,
					       time_sec:ubyte4,
					       err_code:Error.kerberos_error,
					       err_text:string}
    sharing type T = kerberos_err_reply
end

(*
	6.	signature KERBEROS_APPL_REQUEST_LINEARIZE
*)
signature KERBEROS_APPL_REQUEST_LINEARIZE =
sig
    include LINEARIZE

    datatype kerberos_appl_request =
	Appl_Request of {kvno_s:ubyte1,
			realm_s:string,
			 ticket:ByteArray.bytearray,
		  authenticator:ByteArray.bytearray}

  sharing type T = kerberos_appl_request
end

(*
	7.	signature KERBEROS_APPL_REQUEST_LINEARIZE
*)
signature KERBEROS_APPL_ERR_LINEARIZE=
sig
    include LINEARIZE
    structure Error:KERBEROS_ERROR

    datatype kerberos_appl_err = Appl_Err of {err_code:Error.kerberos_error,
					      err_text:string}
    sharing type T = kerberos_appl_err
end


(*
	8.	signature KERBEROS_TICKET_GRANTING_REQUEST_LINEARIZE 
*)
signature KERBEROS_TICKET_GRANTING_REQUEST_LINEARIZE =
sig
    include LINEARIZE

    datatype kerberos_tgs_request =
	Tgs_Request of {kvno_s:ubyte1,
		       realm_s:string,
			ticket:ByteArray.bytearray,
		 authenticator:ByteArray.bytearray,
		      time_sec:ubyte4,
	            lifetime_s:ubyte1,
			name_s:string,
	  	    instance_s:string}

    sharing type T = kerberos_tgs_request

end



