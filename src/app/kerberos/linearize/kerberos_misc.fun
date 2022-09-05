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

	This is a host of functors to linearize each of the
	different packet types of Kerberos, as well as 
	Kerberos authenticators and ciphers.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Authenticator
	2.	functor Credentials
	3.	functor Auth_Request
	4.	functor Auth_Reply
	5.	functor Err_Reply
	6.	functor Appl_Request
	7.	functor Appl_Err
	8.	Ticket_Granting_Request

		iii.	RCS Log
	
$Log: kerberos_misc.fun,v $
Revision 1.1  1994/08/25  10:24:40  robby
Initial revision

Revision 1.2  1994/08/08  15:15:31  danwang
Made changes so authentication with zephyr works.

Revision 1.1  1994/07/14  20:29:42  robby
Initial revision

Revision 1.1  94/07/13  18:48:49  robby
Initial revision

	1.	functor Authenticator
*)

functor Authenticator (structure String:STRING_LINEARIZE
		       structure Ubyte4:UBYTE4_LINEARIZE
		       structure Ubyte1:UBYTE1_LINEARIZE):
    KERBEROS_AUTHENTICATOR_LINEARIZE=
struct
    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern
    exception Does_Not_Match

    datatype authenticator = Authenticator of {name_p:string,
				 	   instance_p:string,
					      realm_p:string,
					     checksum:ubyte4, 
					      time5ms:ubyte1,
					     time_sec:ubyte4}
    type T=authenticator

    structure Mesh =
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=Ubyte4 structure P2=
	Pair(structure P1=Ubyte1
	     structure P2=Ubyte4)))))

    fun marshall_args (Authenticator {name_p,instance_p,realm_p,
				      checksum,time5ms,time_sec}) =
	(name_p,(instance_p,(realm_p,(checksum,(time5ms,time_sec)))))

    fun unmarshall_args	(name_p,(instance_p,(realm_p,
				 (checksum,(time5ms,time_sec))))) =
	(Authenticator {name_p=name_p,instance_p=instance_p,realm_p=realm_p,
			checksum=checksum,time5ms=time5ms,time_sec=time_sec}) 

    val size = Mesh.size o marshall_args
	
    fun marshall (a, Extern (array,p)) =
	let
	    val (Mesh.Extern (array,p)) =
		Mesh.marshall (marshall_args a, Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end
    
    fun unmarshall (Extern (array,p))=
	let
	    val (a, Mesh.Extern (array,p)) =
		Mesh.unmarshall (Mesh.Extern (array,p))
	in
	    (unmarshall_args a, Extern (array,p))
	end
    end

(*
	2.	functor Credentials
 *)

functor Credentials (structure String:STRING_LINEARIZE
		     structure Des:DES'
		     structure Key:DES_KEY_LINEARIZE
		     structure Ubyte1:UBYTE1_LINEARIZE
		     structure Ubyte4:UBYTE4_LINEARIZE
		     structure B:FOX_BASIS
		     val do_prints:bool
		     sharing type Des.key=Key.Des.key):
    KERBEROS_CREDENTIALS_LINEARIZE =
struct
    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern
    exception Does_Not_Match
    
    datatype credential = Credentials of {session_key:Des.key,
				               name_s:string, 
				           instance_s:string,
				              realm_s:string, 
				           lifetime_s:ubyte1,
				               kvno_s:ubyte1,
				               ticket:ByteArray.bytearray,
			                time_sec_kkds:ubyte4}
    type T = credential
    structure Des = Des
    structure ByteArray4 = ByteArray4_Linearize(structure Ubyte1 = Ubyte1
						structure B = B
						val do_prints = do_prints)

    structure Mesh =
	Pair(structure P1=Key structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=Ubyte1 structure P2=
	Pair(structure P1=Ubyte1 structure P2=
	Pair(structure P1=ByteArray4
	     structure P2=Ubyte4)))))))

    fun marshall_args (Credentials {session_key,name_s,instance_s,realm_s,
			       lifetime_s,kvno_s,ticket,time_sec_kkds}) =
	(session_key,(name_s,(instance_s,(realm_s,
		     (lifetime_s,(kvno_s,(ticket,time_sec_kkds)))))))

    fun unmarshall_args (session_key,(name_s,(instance_s,(realm_s,
				     (lifetime_s,(kvno_s,(ticket,
				      time_sec_kkds))))))) =
	(Credentials {session_key=session_key,name_s=name_s,instance_s=instance_s,
		 realm_s=realm_s,lifetime_s=lifetime_s,kvno_s=kvno_s,
		 ticket=ticket,time_sec_kkds=time_sec_kkds}) 

    val size =  Mesh.size o marshall_args

    fun marshall (a,Extern (array,p)) =
	let
	    val (Mesh.Extern(array,p)) =
		Mesh.marshall (marshall_args a, Mesh.Extern (array,p))
		handle Mesh.Does_Not_Match => raise Does_Not_Match
	in 
	    Extern (array,p)
	end

    fun unmarshall (Extern (array,p))=
	let val (a,Mesh.Extern (array,p))=
	    Mesh.unmarshall (Mesh.Extern (array,p))
	    handle Mesh.Does_Not_Match => raise Does_Not_Match
	in
	    (unmarshall_args a, Extern (array,p))
	end
end

(*
	3.	functor Auth_Request
 *)

functor Auth_Request(structure Ubyte1:UBYTE1_LINEARIZE
		     structure String:STRING_LINEARIZE
		     structure Ubyte4:UBYTE4_LINEARIZE):
    KERBEROS_AUTH_REQUEST_LINEARIZE =
struct
    datatype extern = Extern of ByteArray.bytearray * int
    type incoming = extern
    type outgoing = extern
	
    exception Does_Not_Match
    
    datatype kerberos_auth_request = Auth_Request of {name_p:string, 
						  instance_p:string, 
						     realm_p:string,
						    time_sec:ubyte4,
						  lifetime_s:ubyte1,
						      name_s:string,
						  instance_s:string}  

    type T = kerberos_auth_request

    structure Mesh =
	Pair(structure P1=String structure P2=
        Pair(structure P1=String structure P2=
        Pair(structure P1=String structure P2=
        Pair(structure P1=Ubyte4 structure P2=
        Pair(structure P1=Ubyte1 structure P2=
        Pair(structure P1=String
             structure P2=String))))))

    fun marshall_args (Auth_Request {name_p,instance_p,realm_p,
				     time_sec,lifetime_s,name_s,instance_s}) =
	(name_p,(instance_p,(realm_p,
	(time_sec,(lifetime_s,(name_s,instance_s))))))
	
    fun unmarshall_args (name_p,(instance_p,(realm_p,
			 (time_sec,(lifetime_s,(name_s,instance_s)))))) =
	(Auth_Request {name_p=name_p,instance_p=instance_p,realm_p=realm_p,
		       time_sec=time_sec,lifetime_s=lifetime_s,
		       name_s=name_s,instance_s=instance_s}) 

    val size =  Mesh.size o marshall_args 

    fun marshall (a,Extern (array,p)) =
	let 
	    val (Mesh.Extern (array,p)) = 
		Mesh.marshall (marshall_args a,Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end
    
    fun unmarshall (Extern (array,p)) =
	let
	    val (a,Mesh.Extern (array,p)) =
		Mesh.unmarshall (Mesh.Extern (array,p))
	in
	    (unmarshall_args a,Extern (array,p))
	end
end

(*
	4.	functor Auth_Reply
*)
functor Auth_Reply(structure Ubyte1:UBYTE1_LINEARIZE
		   structure Ubyte2:UBYTE2_LINEARIZE
		   structure String:STRING_LINEARIZE
		   structure Ubyte4:UBYTE4_LINEARIZE
		   structure B:FOX_BASIS
		   val do_prints:bool):KERBEROS_AUTH_REPLY_LINEARIZE=
struct
    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern
	
    exception Does_Not_Match
    
    datatype kerberos_auth_reply = Auth_Reply of {name_p:string, 
					      instance_p:string, 
					         realm_p:string,
					        time_sec:ubyte4, 
					       n_tickets:ubyte1,
					      exp_date_p:ubyte4,
						  kvno_p:ubyte1, 
					     credentials:ByteArray.bytearray} 
    type T = kerberos_auth_reply
	
    structure ByteArray2_Linearize =
	ByteArray2_Linearize (structure B = B
			      structure Ubyte2 = Ubyte2
			      val do_prints=do_prints)
    structure Mesh =
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
	Pair(structure P1=Ubyte4 structure P2=
	Pair(structure P1=Ubyte1 structure P2=
	Pair(structure P1=Ubyte4 structure P2=
	Pair(structure P1=Ubyte1
	     structure P2=ByteArray2_Linearize)))))))

    fun marshall_args (Auth_Reply {name_p, instance_p, realm_p,
				   time_sec,n_tickets,exp_date_p,
				   kvno_p,credentials}) =
	(name_p,(instance_p,(realm_p,
                (time_sec,(n_tickets,(exp_date_p,(kvno_p,credentials)))))))
	
    fun unmarshall_args (name_p,(instance_p,(realm_p,
				(time_sec,(n_tickets,(exp_date_p,
  			        (kvno_p,credentials))))))) =
	(Auth_Reply {name_p=name_p,instance_p=instance_p,realm_p=realm_p,
		     time_sec=time_sec,n_tickets=n_tickets,
		     exp_date_p=exp_date_p,kvno_p=kvno_p,
		     credentials=credentials}) 
	
    val size = Mesh.size o marshall_args
	
    fun marshall (a,Extern (array,p))=
	let 
	    val (Mesh.Extern (array,p)) = 
		Mesh.marshall(marshall_args a,Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end
    
    fun unmarshall (Extern (array,p))=
	let 
	    val (a,Mesh.Extern (array,p)) = 
		Mesh.unmarshall(Mesh.Extern (array,p))
	in
	    (unmarshall_args a,Extern (array,p))
	end
end

(*
	5.	functor Err_Reply
*)

functor Err_Reply (structure Error:KERBEROS_ERROR
		   structure Error_Linearize:KERBEROS_ERROR_LINEARIZE
		   structure Ubyte4:UBYTE4_LINEARIZE
		   structure String:STRING_LINEARIZE
		   sharing type Error.kerberos_error=
		       Error_Linearize.kerberos_error):
    KERBEROS_ERR_REPLY_LINEARIZE =
struct
    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern
    exception Does_Not_Match
    
    structure Error = Error
	
    datatype kerberos_err_reply =  Err_Reply of {name_p:string,
					     instance_p:string,
					        realm_p:string,
					       time_sec:ubyte4,
					       err_code:Error.kerberos_error,
					       err_text:string}
	
    type T=kerberos_err_reply

    structure Mesh = 
	Pair(structure P1=String structure P2=
	Pair(structure P1=String structure P2=
        Pair(structure P1=String structure P2=
        Pair(structure P1=Ubyte4 structure P2=
        Pair(structure P1=Error_Linearize
   	     structure P2=String)))))

    fun marshall_args (Err_Reply {name_p,instance_p,realm_p,
				  time_sec,err_code,err_text}) =
	(name_p,(instance_p,(realm_p,
		(time_sec,(err_code,err_text)))))

    fun unmarshall_args (name_p,(instance_p,(realm_p,
  			        (time_sec,(err_code,err_text))))) =
	(Err_Reply {name_p=name_p,instance_p=instance_p,realm_p=realm_p,
		    time_sec=time_sec,err_code=err_code,err_text=err_text})

    val size = Mesh.size o marshall_args

    fun marshall (a,Extern (array,p)) =
	let
	    val (Mesh.Extern (array,p))	=
		Mesh.marshall (marshall_args a,Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end
    handle Mesh.Does_Not_Match => raise Does_Not_Match

    fun unmarshall (Extern (array,p)) =
	let
	    val (a,Mesh.Extern (array,p)) =
		Mesh.unmarshall (Mesh.Extern (array,p))
	in
	    (unmarshall_args a,Extern (array,p))
	end
    handle Mesh.Does_Not_Match => raise Does_Not_Match

end

(*
	6.	functor Appl_Request
 *)
functor Appl_Request (structure Ubyte1:UBYTE1_LINEARIZE
		      structure Ubyte4:UBYTE4_LINEARIZE
		      structure String:STRING_LINEARIZE
		      structure B:FOX_BASIS
		      val do_prints:bool):KERBEROS_APPL_REQUEST_LINEARIZE =
struct
    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern

    exception Does_Not_Match

    datatype kerberos_appl_request =
	Appl_Request of {kvno_s:ubyte1,
			realm_s:string,
			 ticket:ByteArray.bytearray,
		  authenticator:ByteArray.bytearray}

    type T = kerberos_appl_request

    structure ByteArray3 = ByteArray3_Linearize(structure Ubyte1 = Ubyte1
						structure B = B
						val do_prints = do_prints)

    structure Mesh =
	Pair(structure P1=Ubyte1 structure P2=
        Pair(structure P1=String 
	     structure P2=ByteArray3))

    fun marshall_args (Appl_Request {kvno_s,realm_s,ticket,authenticator}) =
	(kvno_s,(realm_s,ByteArray3.Pair(ticket,authenticator))):Mesh.T

    fun unmarshall_args (kvno_s,(realm_s,
				ByteArray3.Pair(ticket,authenticator))) = 
	(Appl_Request {kvno_s=kvno_s,realm_s=realm_s,
		       ticket=ticket,authenticator=authenticator})

    val size = Mesh.size o marshall_args

    fun marshall (a,Extern (array,p)) =
	let
	    val (Mesh.Extern (array,p)) =
		Mesh.marshall (marshall_args a,Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end

    fun unmarshall (Extern (array,p)) =
	let
	    val (a,Mesh.Extern (array,p)) =
		Mesh.unmarshall (Mesh.Extern (array,p))
	in
	    (unmarshall_args a,Extern (array,p))
	end
    end

(*
	7.	functor Appl_Err
 *)
functor Appl_Err(structure Error:KERBEROS_ERROR
		 structure Error_Linearize:KERBEROS_ERROR_LINEARIZE
		 structure String:STRING_LINEARIZE
		 sharing type Error.kerberos_error=
		     Error_Linearize.kerberos_error):
    KERBEROS_APPL_ERR_LINEARIZE=
struct

    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern
    exception Does_Not_Match

    structure Error = Error
	
    datatype kerberos_appl_err = Appl_Err of {err_code:Error.kerberos_error,
					      err_text:string}
    type T = kerberos_appl_err

    structure Mesh =
	Pair(structure P1=Error_Linearize
	     structure P2=String)

    fun marshall_args (Appl_Err {err_code,err_text}) =
	(err_code,err_text)

    fun unmarshall_args (err_code,err_text) =
	(Appl_Err {err_code=err_code,err_text=err_text}) 
	
    val size = Mesh.size o marshall_args
	
    fun marshall (a,Extern (array,p))=
	let
	    val (Mesh.Extern (array,p)) =
		Mesh.marshall (marshall_args a,Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end

    fun unmarshall (Extern (array,p))=
	let
	    val (a,Mesh.Extern (array,p)) =
		Mesh.unmarshall (Mesh.Extern (array,p))
	in
	    (unmarshall_args a ,Extern (array,p))
	end
end


(*
	8.	Ticket_Granting_Request
*)

functor Ticket_Granting_Request(structure Appl_Request:
				    KERBEROS_APPL_REQUEST_LINEARIZE
				structure Ubyte4:UBYTE4_LINEARIZE
				structure Ubyte1:UBYTE1_LINEARIZE
				structure String:STRING_LINEARIZE):
    KERBEROS_TICKET_GRANTING_REQUEST_LINEARIZE =
struct

    datatype extern = Extern of ByteArray.bytearray * int
    type outgoing = extern
    type incoming = extern

    exception Does_Not_Match

    datatype kerberos_tgs_request =
	Tgs_Request of {kvno_s:ubyte1,
		       realm_s:string,
			ticket:ByteArray.bytearray,
		 authenticator:ByteArray.bytearray,
		      time_sec:ubyte4,
	            lifetime_s:ubyte1,
			name_s:string,
	  	    instance_s:string}

    type T = kerberos_tgs_request


    structure Mesh =
	Pair(structure P1=Appl_Request structure P2=
	Pair(structure P1=Ubyte4 structure P2=
	Pair(structure P1=Ubyte1 structure P2=
        Pair(structure P1=String
	     structure P2=String))))
		 
    fun marshall_args (Tgs_Request {kvno_s,realm_s,ticket,authenticator,
				    time_sec,lifetime_s,name_s,instance_s}) =
	(Appl_Request.Appl_Request {kvno_s=kvno_s,realm_s=realm_s,
				     ticket=ticket,
				     authenticator=authenticator},
	 (time_sec,(lifetime_s,(name_s,instance_s))))

    fun unmarshall_args ((Appl_Request.Appl_Request
			 {kvno_s,realm_s,ticket,authenticator}),
			 (time_sec,(lifetime_s,(name_s,instance_s)))) =
	(Tgs_Request {kvno_s=kvno_s,realm_s=realm_s,ticket=ticket,
		      authenticator=authenticator,time_sec=time_sec,
		      lifetime_s=lifetime_s,name_s=name_s,
		      instance_s=instance_s})
			 
    val size = Mesh.size o marshall_args

    fun marshall (a,Extern (array,p)) =
	let
	    val (Mesh.Extern (array,p)) =
		Mesh.marshall (marshall_args a,Mesh.Extern (array,p))
	in 
	    Extern (array,p)
	end

    fun unmarshall (Extern (array,p)) =
	let
	    val (a,Mesh.Extern (array,p)) =
		Mesh.unmarshall (Mesh.Extern (array,p))
	in
	    (unmarshall_args a,Extern (array,p))
	end

end




