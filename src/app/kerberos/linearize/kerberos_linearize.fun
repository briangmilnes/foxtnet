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

	This linearizes the union of different Kerberos packets
	into one datatype.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Kerberos_Linearize
	2.	datatype kerberos_message
	3.	dispatch_unmarshall
	4.	fun size
	5.	fun marshall
	6.	fun unmarshall

		iii.	RCS Log
	
$Log: kerberos_linearize.fun,v $
Revision 1.2  1994/08/25  22:43:05  robby
removed about 10 linefeed from the middle of the
argument list for the functor.

Revision 1.1  1994/08/25  10:24:39  robby
Initial revision

Revision 1.3  1994/08/16  13:05:55  robby
added toc

Revision 1.2  94/08/08  15:14:01  danwang
Made changes so authentication for zephyr works.

Revision 1.1  1994/07/14  20:29:39  robby
Initial revision

Revision 1.1  94/07/13  18:48:46  robby
Initial revision


	1.	functor Kerberos_Linearize
*)

functor Kerberos_Linearize
  (structure Tag:KERBEROS_TAG_LINEARIZE

   structure Auth_Request:KERBEROS_AUTH_REQUEST_LINEARIZE
   structure Auth_Reply:KERBEROS_AUTH_REPLY_LINEARIZE
   structure Err_Reply:KERBEROS_ERR_REPLY_LINEARIZE

   structure Appl_Request:KERBEROS_APPL_REQUEST_LINEARIZE
   structure Appl_Err:KERBEROS_APPL_ERR_LINEARIZE
   structure Tgs_Request:KERBEROS_TICKET_GRANTING_REQUEST_LINEARIZE
       
   structure Rev_Auth_Request:KERBEROS_AUTH_REQUEST_LINEARIZE
   structure Rev_Auth_Reply:KERBEROS_AUTH_REPLY_LINEARIZE
   structure Rev_Err_Reply:KERBEROS_ERR_REPLY_LINEARIZE

   structure Rev_Appl_Request:KERBEROS_APPL_REQUEST_LINEARIZE
   structure Rev_Appl_Err:KERBEROS_APPL_ERR_LINEARIZE

   structure B:FOX_BASIS
   val do_prints:bool
   sharing type Rev_Appl_Err.Error.kerberos_error=Appl_Err.Error.kerberos_error
     = Rev_Err_Reply.Error.kerberos_error=Err_Reply.Error.kerberos_error):
     KERBEROS_LINEARIZE=
struct
  val version = 1u4
  val endian  =
      case Byte4.endian of
	  Byte4.Big => Tag.Big
	| Byte4.Little => Tag.Little

  structure Error = Appl_Err.Error

  datatype extern = Extern of ByteArray.bytearray * int
  type outgoing = extern
  type incoming = extern

  val debug_print = if do_prints then (fn x=>B.V.print x)
		    else (fn _=>())

  exception Does_Not_Match

(*
	2.	datatype kerberos_message
*)

(* the cipher_reversed field of an Auth_Reply is only used when the
   Auth_Reply is unmarshalled *)

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

  type T = kerberos_message

  (* Properly marshall a kerberos message sans tag info *)
  fun dispatch_marshall (Auth_Request v,Extern(array,p)) =
      (let
	  val Auth_Request.Extern(array,p) =
	      Auth_Request.marshall ((Auth_Request.Auth_Request v),
				     Auth_Request.Extern(array,p))
      in
	  Extern(array,p)
      end
	   handle Auth_Request.Does_Not_Match => raise Does_Not_Match)
    | dispatch_marshall (Auth_Reply {name_p, instance_p, realm_p,
				     time_sec, exp_date_p,n_tickets,
				     kvno_p, credentials,...},
			 Extern(array,p)) =
      (let
	   val v = {name_p=name_p, instance_p=instance_p, realm_p=realm_p,
		    time_sec=time_sec, exp_date_p=exp_date_p,
		    n_tickets=n_tickets,kvno_p=kvno_p,
		    credentials=credentials}
	   val Auth_Reply.Extern(array,p) =
	       Auth_Reply.marshall ((Auth_Reply.Auth_Reply v),
				    Auth_Reply.Extern(array,p))
      in
	  Extern(array,p)
      end
	   handle Auth_Reply.Does_Not_Match => raise Does_Not_Match)
    | dispatch_marshall (Err_Reply v,Extern(array,p)) =
      (let
	  val Err_Reply.Extern(array,p) =
	      Err_Reply.marshall ((Err_Reply.Err_Reply v),
				     Err_Reply.Extern(array,p))
      in
	  Extern(array,p)
      end
	   handle Err_Reply.Does_Not_Match => raise Does_Not_Match)
    | dispatch_marshall (Appl_Request v,Extern(array,p)) =
      (let
	  val Appl_Request.Extern(array,p) =
	      Appl_Request.marshall ((Appl_Request.Appl_Request v),
				     Appl_Request.Extern(array,p))
      in
	  Extern(array,p)
      end
	   handle Appl_Request.Does_Not_Match => raise Does_Not_Match)
    | dispatch_marshall (Appl_Err v,Extern(array,p)) =
      (let
	  val Appl_Err.Extern(array,p) =
	      Appl_Err.marshall ((Appl_Err.Appl_Err v),
				     Appl_Err.Extern(array,p))
      in
	  Extern(array,p)
      end
	   handle Appl_Err.Does_Not_Match => raise Does_Not_Match)
    | dispatch_marshall (Tgs_Request v,Extern(array,p)) =
      (let
	  val Tgs_Request.Extern(array,p) =
	      Tgs_Request.marshall ((Tgs_Request.Tgs_Request v),
				     Tgs_Request.Extern(array,p))
      in
	  Extern(array,p)
      end
	   handle Tgs_Request.Does_Not_Match => raise Does_Not_Match)

(*
	3.	dispatch_unmarshall
*)
  (* unmarshall a kerberos message of a given type, and optionally swap bytes *)
  fun dispatch_unmarshall Tag.Auth_Request false (Extern(array,p)) =
      (let
	   val (Auth_Request.Auth_Request v,Auth_Request.Extern(array,p)) =
	       Auth_Request.unmarshall (Auth_Request.Extern(array,p))
       in
	   (Auth_Request v, Extern(array,p))
       end
	   handle Auth_Request.Does_Not_Match => raise Does_Not_Match)
    |  dispatch_unmarshall Tag.Auth_Reply false (Extern(array,p)) =
      (let
	  val (Auth_Reply.Auth_Reply {name_p, instance_p, realm_p,
				      time_sec, exp_date_p,n_tickets,
				      kvno_p, credentials},
	       Auth_Reply.Extern(array,p)) =
	      Auth_Reply.unmarshall (Auth_Reply.Extern(array,p))
	  val v = {name_p=name_p, instance_p=instance_p, realm_p=realm_p,
		   time_sec=time_sec, exp_date_p=exp_date_p,
		   n_tickets=n_tickets,kvno_p=kvno_p,
		   credentials=credentials,swap_bytes=false}
       in
	   (Auth_Reply v, Extern(array,p))
       end
	   handle Auth_Reply.Does_Not_Match => raise Does_Not_Match)
    |  dispatch_unmarshall Tag.Err_Reply false (Extern(array,p)) =
      (let
	  val (Err_Reply.Err_Reply v,Err_Reply.Extern(array,p)) =
	      Err_Reply.unmarshall (Err_Reply.Extern(array,p))
       in
	   (Err_Reply v, Extern(array,p))
       end
	   handle Err_Reply.Does_Not_Match => raise Does_Not_Match)
    |  dispatch_unmarshall Tag.Appl_Request false (Extern(array,p)) =
      (let
	  val (Appl_Request.Appl_Request v,Appl_Request.Extern(array,p)) =
	      Appl_Request.unmarshall (Appl_Request.Extern(array,p))
       in
	   (Appl_Request v, Extern(array,p))
       end
	   handle Appl_Request.Does_Not_Match => raise Does_Not_Match)
    |  dispatch_unmarshall Tag.Appl_Err false (Extern(array,p)) =
      (let
	  val (Appl_Err.Appl_Err v,Appl_Err.Extern(array,p)) =
	      Appl_Err.unmarshall (Appl_Err.Extern(array,p))
       in
	   (Appl_Err v, Extern(array,p))
       end
	   handle Appl_Err.Does_Not_Match => raise Does_Not_Match)
    | dispatch_unmarshall Tag.Auth_Request true (Extern(array,p)) =
      (let
	   val (Rev_Auth_Request.Auth_Request v,
		Rev_Auth_Request.Extern(array,p)) =
	       Rev_Auth_Request.unmarshall (Rev_Auth_Request.Extern(array,p))
       in
	   (Auth_Request v, Extern(array,p))
       end
   handle Rev_Auth_Request.Does_Not_Match => raise Does_Not_Match)
    | dispatch_unmarshall Tag.Auth_Reply true (Extern(array,p)) =
      (let
	   val (Rev_Auth_Reply.Auth_Reply {name_p, instance_p, realm_p,
					   time_sec, exp_date_p,n_tickets,
					   kvno_p, credentials},
		Rev_Auth_Reply.Extern(array,p)) =
	       Rev_Auth_Reply.unmarshall (Rev_Auth_Reply.Extern(array,p))
       	   val v = {name_p=name_p, instance_p=instance_p, realm_p=realm_p,
		    time_sec=time_sec, exp_date_p=exp_date_p,
		    n_tickets=n_tickets,kvno_p=kvno_p,
		    credentials=credentials,swap_bytes=true}
       in
	   (Auth_Reply v, Extern(array,p))
       end
   handle Rev_Auth_Reply.Does_Not_Match => raise Does_Not_Match)
    | dispatch_unmarshall Tag.Err_Reply true (Extern(array,p)) =
      (let
	   val (Rev_Err_Reply.Err_Reply v,
		Rev_Err_Reply.Extern(array,p)) =
	       Rev_Err_Reply.unmarshall (Rev_Err_Reply.Extern(array,p))
       in
	   (Err_Reply v, Extern(array,p))
       end
   handle Rev_Err_Reply.Does_Not_Match => raise Does_Not_Match)
    | dispatch_unmarshall Tag.Appl_Request true (Extern(array,p)) =
      (let
	   val (Rev_Appl_Request.Appl_Request v,
		Rev_Appl_Request.Extern(array,p)) =
	       Rev_Appl_Request.unmarshall (Rev_Appl_Request.Extern(array,p))
       in
	   (Appl_Request v, Extern(array,p))
       end
   handle Rev_Appl_Request.Does_Not_Match => raise Does_Not_Match)
    | dispatch_unmarshall Tag.Appl_Err true (Extern(array,p)) =
      (let
	   val (Rev_Appl_Err.Appl_Err v,
		Rev_Appl_Err.Extern(array,p)) =
	       Rev_Appl_Err.unmarshall (Rev_Appl_Err.Extern(array,p))
       in
	   (Appl_Err v, Extern(array,p))
       end
   handle Rev_Appl_Err.Does_Not_Match => raise Does_Not_Match)
	   
(*
	4.	fun size
*)
  fun size k_msg =
      2 + (case k_msg of
	       (Auth_Request record) =>
		   Auth_Request.size (Auth_Request.Auth_Request record)
	     | (Auth_Reply {name_p, instance_p, realm_p,
			    time_sec, exp_date_p,n_tickets,
			    kvno_p, credentials,...}) =>
		   Auth_Reply.size (Auth_Reply.Auth_Reply
				    {name_p=name_p,
				     instance_p=instance_p,
				     realm_p=realm_p,
				     time_sec=time_sec,
				     exp_date_p=exp_date_p,
				     n_tickets=n_tickets,
				     kvno_p=kvno_p,
				     credentials=credentials})
	     | (Err_Reply record) =>
		   Err_Reply.size (Err_Reply.Err_Reply record)
	     | (Appl_Request record) =>
		   Appl_Request.size (Appl_Request.Appl_Request record)
	     | (Appl_Err record) =>
		   Appl_Err.size (Appl_Err.Appl_Err record)
      	     | (Tgs_Request record) =>
		  Tgs_Request.size (Tgs_Request.Tgs_Request record))

(*
	5.	fun marshall
*)
  fun marshall (k_msg,Extern(array,p)) =
      let
	  val msg_type =
	      (case k_msg of
	       (Auth_Request _) => Tag.Auth_Request
	     | (Auth_Reply   _) => Tag.Auth_Reply
	     | (Err_Reply    _) => Tag.Err_Reply
	     | (Appl_Request _) => Tag.Appl_Request
	     | (Appl_Err     _) => Tag.Appl_Err
	     | (Tgs_Request  _) => Tag.Appl_Request)
	  val Tag.Extern(array,p) =
	      (Tag.marshall (Tag.Kerberos_Tag
			    (version,msg_type,endian),
			     Tag.Extern(array,p))
	       handle Tag.Does_Not_Match => raise Does_Not_Match)
      in
	  dispatch_marshall (k_msg,Extern(array,p))
      end

  (*
     N.B.: We assume all messages with Tag.Appl_Request are
     Appl_Request messages, which means we can't unmarshall a
     Tgs_Request. This isn't a problem until we decided to write our
     own kerberos server. As clients should only send Tgs_Requests.
  *)

(*
	6.	fun unmarshall
*)
  fun unmarshall (Extern(array,p)) =
      let
	  val (Tag.Kerberos_Tag(msg_version,msg_type,msg_endian),
		Tag.Extern(array,p)) = (Tag.unmarshall (Tag.Extern(array,p))
	       handle Tag.Does_Not_Match => raise Does_Not_Match)
	  val swap = not (msg_endian = endian)
      in
	  if msg_version = version then
	      dispatch_unmarshall msg_type swap (Extern(array,p))
	  else raise Does_Not_Match
      end

end







