(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robby Findler (Robert.Findler@cs.cmu.edu)
	Daniel Wang (Daniel.Wang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	This layer acts a higher level interface to the lower zephyr
	layers. It encodes and decodes the various data formats so the
	client level zephyr applications can carry out the basic
	operations needed to utilize the zephyr protocol. It also
	handles issues of authentication.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor ZEPHYR_CLIENT
	2.	conversion functions
	3.	functions to build and convert messages
	4.	zlogin
	5.	zlogout
	6.	send
	7.	receive
	8.	subscribe
	9.	unsubscribe
	10.	list_subscriptions
	11.	list_default_subscriptions
	12.	locate_user


	iii.	RCS Log

$Log: zephyr_client.fun,v $
Revision 1.1  1994/09/03  21:15:09  danwang
Initial revision


 	iv.	Lacunae

	Right, now we do not check the authenticity of messages
	received.

		1.	functor ZEPHYR_CLIENT
*)
functor Zephyr_Client(structure K : KERBEROS
		      structure Z : ZEPHYR
		      structure B : FOX_BASIS
		      val default_realm : string
		      val do_prints: bool) : ZEPHYR_CLIENT =
struct

    datatype message_kind =
	UNSAFE
      | UNACKED
      | ACKED
      | SERVACK
      | SERVNACK
      | CLIENTACK
      | STAT

    datatype exposure_level =
	NO_EXPOSURE
      | OPSTAFF
      | REALM_VISIBLE
      | REALM_ANNOUNCED
      | NET_VISIBLE
      | NET_ANNOUNCED

    datatype zid = Zid of {name:string,
			  realm:string option}

    datatype zephyr_message =
	Message of {kind:message_kind,
		    auth:bool,
	           class:string,
	        instance:string,
	        recipient:zid,
		  sender:zid,
		    zsig:string,
		 message:string}

      | Login of {kind:message_kind,
		  who:zid,
	     exposure:exposure_level,
	     hostname:string,
	         time:string,
	     location:string}

      | Logout of {kind:message_kind,
		   who:zid,
	      hostname:string,
	          time:string,
	      location:string}

      | Notice of {kind:message_kind,
		   auth:bool,
		  class:string,
	       instance:string,
		 opcode:string,
		 sender:string,
	       recipient:string,
	 default_format:string,
		   data:string list}

(*
    All the state associated with a user, a credential or NONE if
    unauthenticated, a "client" abstraction to send and receive
    messages with, and mutable reference to a handler.
*)

    datatype zuser = Zuser of {credential:K.credential option,
			       me:zid,
			       client:Z.client,
			       handler:(zephyr_message -> unit) ref}

    exception Zephyr_Client_Error of string

    val debug_print=if do_prints then
	(fn x =>(B.V.print("zephyr_client.fun: " ^ x ^"\n")))
		    else (fn _=>())
    fun draise s =
	(B.V.print ("zephyr_client.fun: exception " ^ s ^ "\n");
	 raise  Zephyr_Client_Error s)

(*
	2.	conversion functions
*)

    fun zid_to_string (Zid {name,realm}) =
	let
	    val name = name
	    val realm =
		case realm of
		    NONE => default_realm
		  | SOME x => x
	in
	    name^"@"^realm
	end

    fun string_to_zid str =
	let
	    fun parse (name,realm,[]) =
		(Zid {name=name,realm=
		      (case realm of
			  ""=> NONE
			| _  => SOME realm)})
	      | parse (name,realm,x::"@"::xs) =
		 parse(name^x,(implode xs),[])
	      | parse (name,realm,x::xs) =
		 parse(name^x,realm,xs)
	in
		 parse("","", explode str)
	end

    fun zuser_to_zid (Zuser {me,...}) = me

(*
	3.	functions to build and convert messages
*)

    fun build_authenticator (SOME credential) =
	let
	    val authenticator =
		(K.authenticator_to_bytearray
		 (K.build_authenticator (credential,4u0)))

	    val authent_len = Byte4.from_int(ByteArray.length
					     authenticator)
	    val _ = debug_print("built authenticator.")
	in
	    {auth=4u1,
	     authent_len=authent_len,
	     authenticator=authenticator}
	end
      | build_authenticator NONE =
	{auth=4u0,
	 authent_len=4u0,
	 authenticator=ByteArray.array(0,0)}

    fun check_authentication authenticator = true

    fun message_kind_to_notice_kind UNSAFE = Z.UNSAFE
      | message_kind_to_notice_kind UNACKED = Z.UNACKED
      | message_kind_to_notice_kind ACKED = Z.ACKED
      | message_kind_to_notice_kind SERVACK = Z.SERVACK
      | message_kind_to_notice_kind SERVNACK = Z.SERVNACK
      | message_kind_to_notice_kind CLIENTACK = Z.CLIENTACK
      | message_kind_to_notice_kind STAT = Z.STAT

    fun notice_kind_to_message_kind Z.UNSAFE = UNSAFE
      | notice_kind_to_message_kind Z.UNACKED = UNACKED
      | notice_kind_to_message_kind Z.ACKED = ACKED
      | notice_kind_to_message_kind Z.SERVACK = SERVACK
      | notice_kind_to_message_kind Z.SERVNACK = SERVNACK
      | notice_kind_to_message_kind Z.CLIENTACK = CLIENTACK
      | notice_kind_to_message_kind Z.STAT = STAT

    fun message_to_notice zuser	(Message {kind,auth,class,instance,
					  recipient,zsig,message,...}) =
	let
	    val kind = message_kind_to_notice_kind kind
	    val (Zuser {me,credential,...}) = zuser
	    val {auth,authent_len,authenticator} =
		build_authenticator credential
	    val sender = zid_to_string me
	    val data = [zsig,message]
	in
	    (Z.Notice {kind=kind,
		       auth=auth,
		       authent_len=authent_len,
		       authenticator=authenticator,
		       class=class,
		       instance=instance,
		       opcode="",
		       sender=sender,
		       recipient=zid_to_string recipient,
		       default_format=
		       "Class $class, Instance $instance:\n" ^
		       "To: @bold($recipient)\n" ^
		       "@bold($1) <$sender>\n\n$2",
		       checksum=4u0,
		       data=data})
	end
      | message_to_notice zuser	(Login {kind,who,
					exposure,hostname,time,location}) =
	let
	    val kind = message_kind_to_notice_kind kind
	    val (Zuser {me,credential,...}) = zuser
	    val {auth,authent_len,authenticator} =
		 build_authenticator credential
	    val zid = zid_to_string me
	    val data = [hostname,time,location]
	    val exposure = case exposure of
		NO_EXPOSURE => "NONE"
	      | OPSTAFF => "OPSTAFF"
	      | REALM_VISIBLE => "REALM-VISIBLE"
	      | REALM_ANNOUNCED => "REALM-ANNOUNCED"
	      | NET_VISIBLE => "NET-VISIBLE"
	      | NET_ANNOUNCED => "NET-ANNOUNCED"
	in
	    (Z.Notice {kind=kind,
		       auth=auth,
		       authent_len=authent_len,
		       authenticator=authenticator,
		       class="LOGIN",
		       instance=zid,
		       opcode=exposure,
		       sender=zid,
		       recipient="",
		       default_format="$sender logged in to $1 on $3 at $2",
		       checksum=4u0,
		       data=data})
	end
      | message_to_notice zuser (Logout {kind,who,hostname,time,location}) =
	let
	    val kind = message_kind_to_notice_kind kind
	    val (Zuser {me,credential,...}) = zuser
	    val {auth,authent_len,authenticator} =
		build_authenticator credential
	    val zid = zid_to_string me
	    val data = [hostname,time,location]
	in
	    (Z.Notice {kind=kind,
		       auth=auth,
		       authent_len=authent_len,
		       authenticator=authenticator,
		       class="LOGIN",
		       instance=zid,
		       opcode="USER_LOGOUT",
		       sender=zid,
		       recipient="",
		       default_format="$sender logged out from $1 on $3 at $2",
		       checksum=4u0,
		       data=data})
	end
      | message_to_notice zuser (Notice {kind,auth,class,instance,
				        opcode,sender,recipient,
				        default_format,data}) =
	let
	    val kind = message_kind_to_notice_kind kind
	    val (Zuser {credential,...}) = zuser
	    val {auth,authent_len,authenticator} =
		build_authenticator credential
	in
	    (Z.Notice {kind=kind,
		       auth=auth,
		       authent_len=authent_len,
		       authenticator=authenticator,
		       class=class,
		       instance=instance,
		       opcode=opcode,
		       sender=sender,
		       recipient=recipient,
		       default_format=default_format,
		       checksum=4u0,
		       data=data})
	end

    fun notice_to_message (Z.Notice
			   {kind,authenticator,class,instance,
			    opcode,sender,recipient,default_format,
			    data,...}) =
	let
	    fun nth i =	((B.V.List.nth (data,i-1)) handle Nth => "")
	    val auth = check_authentication authenticator
	    val kind = notice_kind_to_message_kind kind
	    val Class =  B.V.String.upcase class
	    val Opcode = B.V.String.upcase opcode
	    val zmessage =
		(case Class of
		    "LOGIN" =>
			(case Opcode of
			     "USER_LOGOUT" =>
				 (Logout {kind=kind,
					  who=(string_to_zid instance),
					  hostname=nth 1,
					  time=nth 2,
					  location=nth 3})
			   |  _ =>
				  (Login {kind=kind,
					  who=(string_to_zid instance),
					  exposure=NET_ANNOUNCED,
					  hostname=nth 1,
					  time=nth 2,
					  location=nth 3}))
		  | _ =>
			(case Opcode of
			    "" =>
				 (Message
				  {kind=kind,
				   auth=auth,
				   class=class,
				   instance=instance,
				   sender=(string_to_zid sender),
				   recipient=(string_to_zid recipient),
				   zsig=nth 1,
				   message=nth 2})
			   | _  =>
				 (Notice
				  {kind=kind,
				   auth=auth,
				   class=class,
				   instance=instance,
				   opcode=opcode,
				   sender=sender,
				   recipient=recipient,
				   default_format=default_format,
				   data=data})))
	in
	    zmessage
	end

(*
	4.	zlogin
*)
    fun zlogin {me,auth} =
	let
	    val _ = Z.initialize ()
	    val handler = ref (fn _ => debug_print("Tossing packet"))
	    val client =
		Z.new_client ((fn msg => (!handler) msg) o notice_to_message)
	    val (Zid {realm,...}) = me
	    val realm =	case realm of
		SOME r => r
	      | NONE  => default_realm
	    val credential =
		if auth then
		     (SOME (K.get_credential
			    (K.Service {name_s="zephyr",
					instance_s="zephyr",
					realm_s=realm}))
		      handle (K.Kerberos_Error _) => NONE)
		else
		    NONE
	in
	    (Zuser  {credential=credential,
		     me=me,
		     client=client,
		     handler=handler})
	end

(*
	5.	zlogout
*)
    fun zlogout (Zuser {client,...}) =
	let
	    val _ = Z.kill_client client
	    val _ = Z.finalize ()
	in
	    ()
	end

    fun send_notice (Zuser {client,...}) notice =
	Z.client_send client notice

    fun zephyr_rpc (Zuser {client,...}) notice =
	Z.client_call client notice

(*
	6.	send
*)

    fun send zuser msg =
	(send_notice zuser (message_to_notice zuser msg);
	 ())

(*
	7.	receive
*)

    fun receive zuser new_handler =
	let
	    val (Zuser {handler,...}) =  zuser
	in
	    debug_print("Installing new message handler.");
	    handler := new_handler
	end

(*
	Conversion functions for (un)subscribe.
*)

    fun subs_list_to_string_list subs =
	let
	    fun loop (data,[]) = data
	      | loop (data,{class,instance,recipient}::rest) =
		loop (class::instance::recipient::data,rest)
	in
	    loop([],subs)
	end

    fun string_list_to_subs_list data =
      	let
	    fun loop (subs,[]) = subs
	      | loop (subs,c::i::r::rest) =
		loop({class=c,instance=i,recipient=r}::subs,rest)
	      | loop (subs,_) = subs
	in
	    loop([],data)
	end

(*
	8.	subscribe
*)
    fun subscribe (zuser as (Zuser {me,...})) {default_subs,subs} =
	let
	    val opcode = if default_subs then "SUBSCRIBE"
			 else "SUBSCRIBE_NODEFS"
	    val sender = zid_to_string me
	    val data = subs_list_to_string_list subs
	    val message =
		(Notice {kind=ACKED,
			 auth=true,
			 class="ZEPHYR_CTL",
			 instance="CLIENT",
			 opcode=opcode,
			 sender=sender,
			 recipient="",
			 default_format="",
			 data=data})
	    val znotice = message_to_notice zuser message
	    val ack = send_notice zuser znotice
	in
		case ack of
		    NONE => draise("No reply while subscribing.")
		  | SOME (Z.Notice {kind,...}) =>
			(case kind of
			     Z.SERVACK => ()
			   | Z.SERVNACK =>
				 draise("Got a SERVNACK while subscribing.")
			   | _ =>
				 draise("Unknown reply while subscribing."))

	end

(*
	9.	unsubscribe
*)

    fun unsubscribe (zuser as (Zuser {me,...})) unsubs =
	let
	    val sender = zid_to_string me
	    val data = subs_list_to_string_list unsubs
	    val message =
		(Notice {kind=ACKED,
			 auth=true,
			 class="ZEPHYR_CTL",
			 instance="CLIENT",
			 opcode="UNSUBSCRIBE",
			 sender=sender,
			 recipient="",
			 default_format="",
			 data=data})
	    val znotice = message_to_notice zuser message
	    val ack = send_notice zuser znotice
	in
		case ack of
		    NONE =>
			draise("Didn't receive a reply while unsubscribing.")
		  | SOME (Z.Notice {kind,...}) =>
			(case kind of
			     Z.SERVACK => ()
			   | Z.SERVNACK =>
				 draise("Got a SERVNACK while unsubscribing.")
			   | _ =>
				 draise("Unknown reply while unsubscribing."))
	end

(*
	10.	list_subscriptions
*)

    fun list_subscriptions (zuser as (Zuser {me,...})) =
	let
	    val opcode = "GIMME"
	    val sender = zid_to_string me
	    val message =
		(Notice {kind=ACKED,
			 auth=true,
			 class="ZEPHYR_CTL",
			 instance="CLIENT",
			 opcode=opcode,
			 sender=sender,
			 recipient="",
			 default_format="",
			 data=([]:string list)})
	    val znotice = message_to_notice zuser message
	    val reply = zephyr_rpc zuser znotice
	in
		case reply of
		    NONE =>
			draise("Didn't receive a reply while requesting subs.")
		  | SOME (Z.Notice {data,...}) =>
			(string_list_to_subs_list data)
	end

(*
	11.	list_default_subscriptions
*)

    fun list_default_subscriptions (zuser as (Zuser {me,...}))=
	let
	    val opcode = "GIMMEDEFS"
	    val sender = zid_to_string me
	    val message =
		(Notice {auth=true,
			 kind=ACKED,
			 class="ZEPHYR_CTL",
			 instance="CLIENT",
			 opcode=opcode,
			 sender=sender,
			 recipient="",
			 default_format="",
			 data=([]:string list)})
	    val znotice = message_to_notice zuser message
	    val reply = zephyr_rpc zuser znotice
	in
		case reply of
		    NONE =>
			draise("Didn't receive a reply while requesting subs.")
		  | SOME (Z.Notice {data,...}) =>
			(string_list_to_subs_list data)
	end


(*
	Conversion function for locate_user
*)
    fun string_list_to_location_list data =
      	let
	    fun loop (locs,[]) = locs
	      | loop (locs,h::t::l::rest) =
		loop({hostname=h,time=t,location=l}::locs,rest)
	      | loop (locs,_) = locs
	in
	    loop([],data)
	end

(*
	12.	locate_user
*)

    fun locate_user (zuser as (Zuser {me,...})) zid =
	let
	    val sender = zid_to_string me
	    val instance = zid_to_string zid
	    val message =
		(Notice {auth=true,
			 kind=ACKED,
			 class="USER_LOCATE",
			 instance=instance,
			 opcode="LOCATE",
			 sender=sender,
			 recipient="",
			 default_format="",
			 data=([]:string list)})
	    val znotice = message_to_notice zuser message
	    val reply = zephyr_rpc zuser znotice
	in
		case reply of
		    NONE =>
			draise("Didn't receive a reply while " ^
			       "requesting user location.")
		  | SOME (Z.Notice {data,...}) =>
			string_list_to_location_list data
	end

end
