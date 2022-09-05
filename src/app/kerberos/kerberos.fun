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

	This provides an interface to the Kerberos library.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Kerberos
	2.	fun build_authenticator
	3.	fun get_ticket_granting_credential
	4.	fun build_authenticator
	5.	fun get_credential
	6.	fun authenticator_to_bytearray
	7.	fun destroy_ticket_granting_ticket

		iii.	RCS Log
	
$Log: kerberos.fun,v $
Revision 1.3  1994/08/09  20:44:00  danwang
Made changes to make zephyr authentication work correctly.

Revision 1.3  1994/08/08  15:09:01  danwang
Made changes so authentication for zephyr works.

Revision 1.3  1994/08/08  14:51:02  danwang
Made changes to make zephyr authentication work correctly.

Revision 1.2  1994/07/16  22:50:27  robby
added get_checksum

Revision 1.1  94/07/13  19:11:41  robby
Initial revision



	1.	functor Kerberos
*)


functor Kerberos(structure K:KERBEROS_RPC_CALL
	         structure Auth:KERBEROS_AUTHENTICATOR_LINEARIZE
		 structure Creds:KERBEROS_CREDENTIALS_LINEARIZE
		 structure Rev_Creds:KERBEROS_CREDENTIALS_LINEARIZE
                 structure B:FOX_BASIS
		 val do_prints:bool
		 sharing type K.Arg.T=K.K.kerberos_message=K.Res.T
		 sharing Creds.Des=Rev_Creds.Des):KERBEROS=
struct

  structure Des = Creds.Des
  datatype principal = Principal of {name_p:string,
				 instance_p:string,
				     realm_p:string}

  datatype service = Service of {name_s:string,
			     instance_s:string,
				realm_s:string}

  datatype credential = Credentials of {session_key:Des.key,
				           name_s:string, 
				       instance_s:string,
				          realm_s:string, 
				       lifetime_s:ubyte1,
				           kvno_s:ubyte1,
				           ticket:ByteArray.bytearray,
			            time_sec_kkds:ubyte4}

  datatype authenticator = Authenticator of ByteArray.bytearray

  val lifetime = 1u200   (* the lifetime of tickets, in 5 minute units *)
  val version  = 1u4      (* the version of kerberos we're using *)
  val debug_print = if do_prints 
			then
			    fn x=>B.V.print x
		    else fn _=>()
			
  (* define a cache that maps from a principal service pair to credential *)

  type cache_key = service
  type cache_val = credential

  local
      fun cache_key_hash (Service {name_s,instance_s,realm_s}) =
	  let
	      val key_s = name_s ^ instance_s ^ realm_s
	      (* A function to hash strings with (from the SML/NJ compiler) *)
	      (* COPYRIGHT (c) 1992 by AT&T Bell Laboratories *)
	      val prime = 8388593 (* largest prime less than 2^23 *)
	      (* val base = 128 *)
	      fun scale i = Bits.lshift(i, 7)  (* multiply by base (128) *)     
	      fun hashString str =
		  let
		      val l = size str
		  in
		      case l
			  of 0 => 0
			| 1 => ord str
			| 2 => ordof(str,0) + scale(ordof(str, 1))
			| 3 => ordof(str,0) + scale((ordof(str, 1) +
						     scale(ordof(str, 2))))
			| _ => let
				   fun loop (0,n) = n
				     | loop (i,n) =
				       let
					   val i = i-1
				       val n' = ((scale n) + ordof(str,i))
				       in
					   loop (i, (n' - prime *
						     (n' quot prime)))
				       end
			       in
			       loop (l,0)
			       end
		  end
	  in
	      hashString key_s
	  end

      val cache = ref ((B.Store.new (cache_key_hash,op =)):
		       (cache_key,cache_val) B.Store.T)
      val tgtkt_cache = ref (NONE:{p:principal,creds:credential} option) 
  in
      fun cache_del_tgtkt () = (tgtkt_cache := NONE)
      fun cache_set_tgtkt {p,creds} =
	  (tgtkt_cache := (SOME{p=p,creds=creds}))
      fun cache_get_tgtkt () = !tgtkt_cache
      fun cache_add key value = cache := B.Store.add (!cache,key,value)
      fun cache_delete key = cache := B.Store.remove (!cache,key)
      fun cache_find key  =
	  case B.Store.look (!cache,key) of
	      NONE => NONE
	    | SOME (new_cache,value) => (cache := new_cache;
					 SOME value)
  end 
      
  exception Kerberos_Error of string
  
  fun decipher_credentials sesion_key creds swap_bytes =
      if swap_bytes then
	  let
	      val creds = Des.pcbc_decrypt sesion_key creds
	      val _ = debug_print ("kerberos.fun: credential size is "^
				   (makestring (ByteArray.length creds))^"\n")
		  
	      val (Rev_Creds.Credentials record,_) =
		  Rev_Creds.unmarshall (Rev_Creds.Extern (creds,0))
		  handle Rev_Creds.Does_Not_Match => 
		      raise Kerberos_Error
			  "could not decrypt/unmarshall credentail"
	  in
	      record
	  end
      else
	  let
	      val creds = Des.pcbc_decrypt sesion_key creds
	      val _ = debug_print ("kerberos.fun: credential size is "^
				   (makestring (ByteArray.length creds))^"\n")
	      val (Creds.Credentials record,_) =
		  Creds.unmarshall (Creds.Extern (creds,0))
		  handle Creds.Does_Not_Match => 
		      raise Kerberos_Error
			  "could not decrypt/unmarshall credentails"
	  in
	      record
	  end
      
  (*
	2.	fun build_authenticator'
   *)
  fun build_authenticator' (Credentials{name_s,instance_s,realm_s,kvno_s,
					session_key,ticket,...}, checksum ) =
	  let
	      val (Principal{name_p,instance_p,realm_p}) =
		  case (cache_get_tgtkt()) of
		      (SOME {p,...}) =>  p
		    | NONE  =>
			  raise Kerberos_Error "no ticket granting ticket"
	      val B.Time.Time{sec, usec} = B.Time.time_of_day ()
	      val auth = Auth.Authenticator
		  ({name_p=name_p,instance_p=instance_p,
		    realm_p=realm_p, checksum=checksum,
		    time5ms=Byte1.from_int usec,
		    time_sec=Byte4.from_int sec})
	      val size = Auth.size auth
	      val array = B.Create.create size
	      val (Auth.Extern(array,p)) =
		   Auth.marshall (auth,Auth.Extern(array,0))
	  in
	      debug_print ("kerberos.fun: built authenticator:\n"^
			   B.Format.makestring_bytearray array^"\n");
	      Des.pcbc_encrypt session_key array 
	  end    

  (*
	3.	fun get_ticket_granting_credential
   *)

  fun get_ticket_granting_credential read_password
      (p as (Principal{name_p,instance_p,realm_p})) =
      
      let
	  val B.V.Misc.TIME {sec,...} = B.V.Misc.gettimeofday ()
	  val request = K.K.Auth_Request {name_p=name_p,
					 instance_p=instance_p,
					 realm_p=realm_p,
					 time_sec=Byte4.from_int sec,
					 lifetime_s=lifetime,
					 name_s="krbtgt",
					 instance_s=realm_p}
	  val _ = K.initialize ()
	  val reply = (K.call (K.Realm realm_p) request
		       handle K.Call_Failed (_,s) =>
			   (K.finalize ();raise Kerberos_Error s)
			    | e => (K.finalize ();raise e))
	  val _ = K.finalize ()
    in
	case reply of
	    K.K.Auth_Reply {name_p,instance_p,realm_p,
			    time_sec,kvno_p,exp_date_p,n_tickets,
			    credentials,swap_bytes} =>
	    let val user_key = (Des.string_to_key ( read_password ()))
		val {session_key,name_s,instance_s,realm_s,
		     lifetime_s,kvno_s,ticket,time_sec_kkds} =
		    (decipher_credentials user_key credentials swap_bytes
		    handle (Kerberos_Error _)=>
			raise Kerberos_Error "incorrect password")
		val _ = debug_print("kerberos.fun: get_ticket_granting_ticket: "
				    ^ "kvno_s is "^(makestring kvno_s)^"\n")
		val credentials = (Credentials {session_key=session_key,
						name_s=name_s,
						instance_s=instance_s,
						realm_s=realm_s,
						lifetime_s=lifetime_s,
						kvno_s=kvno_s,
						ticket=ticket,
						time_sec_kkds=time_sec_kkds} )
		val _ = cache_set_tgtkt {p=p,creds=credentials}
	    in
		credentials
	    end
	
      | K.K.Err_Reply {err_code,err_text,...} => 
	    raise Kerberos_Error ((K.K.Error.makestring err_code)^" "^err_text)
      | _ => raise Kerberos_Error "unexpected response from server"
    end
  
  

(*
	4.	fun build_authenticator
*)

  fun build_authenticator (creds as (Credentials{name_s,instance_s,realm_s,
				     kvno_s,session_key,ticket,lifetime_s,...}),
			   checksum ) =
	  let
	      val (Principal{name_p,instance_p,realm_p}) =
		  case (cache_get_tgtkt ()) of
		      SOME {p,...} => p
		    | NONE =>
			  raise Kerberos_Error
			      "no ticket granting ticket"
	      val authenticator = build_authenticator' (creds,4u0)
	      val _= debug_print
		  ("keberos.fun: get_credential: kvno_s is "^
		   (makestring kvno_s)^"\n")
	      val request = K.K.Appl_Request
		  {kvno_s=kvno_s,
		   realm_s=realm_s,
		   ticket=ticket,
		   authenticator=authenticator}
	      val size = K.K.size request
	      val array = B.Create.create size
	      val (K.K.Extern(array,p)) =
		  K.K.marshall(request,K.K.Extern(array,0))
	  in
	      (Authenticator array)
	  end

  (*
	5.	fun get_credential
   *)
  fun get_credential (s as Service {name_s,instance_s,realm_s})=
      case (cache_find s) of
	  SOME cred => cred
	| NONE => (let
		       val B.V.Misc.TIME {sec,...} = B.V.Misc.gettimeofday ()
		       val (Principal{name_p,instance_p,realm_p},
			   tgc as
			   (Credentials{session_key,ticket,
					lifetime_s,kvno_s,...})) =
			   case (cache_get_tgtkt ()) of
			       SOME {p,creds} => (p,creds)
			     | NONE =>
				   raise Kerberos_Error
				       "no ticket granting ticket"
		      val authenticator = build_authenticator' (tgc,4u0)
		      val _= debug_print
			  ("keberos.fun: get_credential: kvno_s is "^
			   (makestring kvno_s)^"\n")
		      val request = K.K.Tgs_Request
			  {kvno_s=kvno_s,
			   realm_s=realm_s,
			   ticket=ticket,
			   authenticator=authenticator,
			   lifetime_s=lifetime_s,
			   time_sec=Byte4.from_int sec,
			   name_s=name_s,
			   instance_s=instance_s}
		      val _= K.initialize ()
		      val reply = K.call (K.Realm realm_s) request
			  handle K.Call_Failed (_,s) =>
			      (K.finalize ();raise Kerberos_Error s)
			       | e =>(K.finalize ();raise e)
		      val _= K.finalize ()
		  in
		      case reply of 
			  K.K.Auth_Reply {name_p,instance_p,realm_p,
					  time_sec,kvno_p,exp_date_p,n_tickets,
					  credentials,swap_bytes} =>
			  (let
			      val {session_key,name_s,instance_s,realm_s,
				   lifetime_s,kvno_s,ticket,time_sec_kkds} =
				  decipher_credentials session_key
				                       credentials swap_bytes
				  handle (Kerberos_Error _)=>
				      raise Kerberos_Error "incorrect password"
			      val credentials =
				  (Credentials {session_key=session_key,
						name_s=name_s,
						instance_s=instance_s,
						realm_s=realm_s,
						lifetime_s=lifetime_s,
						kvno_s=kvno_s,
						ticket=ticket,
						time_sec_kkds=time_sec_kkds} )
			      val _ = cache_add s credentials  
			  in
			      credentials
			  end)
		       | K.K.Err_Reply {name_p,instance_p,realm_p,
						time_sec,err_code,err_text} => 
			 raise Kerberos_Error
			     ((K.K.Error.makestring  err_code)^" "
			      ^err_text)
		       | K.K.Appl_Err {err_code,err_text} => 
				     raise Kerberos_Error
					 ((K.K.Error.makestring
					   err_code)^" " ^err_text)
		       | _ => raise Kerberos_Error
				     "unexpected response from server"
		  end)

  (*
	6.	fun authenticator_to_bytearray
  *)
  fun authenticator_to_bytearray (Authenticator a) = a

  (*
	7.	fun destroy_ticket_granting_ticket
  *)
  val destroy_ticket_granting_ticket = cache_del_tgtkt 

  (*
	8. 	get_session_key
  *)
  fun get_session_key (Credentials {session_key,...}) = session_key

end


