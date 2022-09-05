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
	layers. It encodes and decodes the various data formats so that
	client level zephyr applications can carry out the basic
	operations needed to utilize the zephyr protocol. It also
	handles issues of authentication.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ZEPHYR_CLIENT


	iii.	RCS Log

$Log: zephyr_client.sig,v $
Revision 1.1  1994/09/03  21:14:53  danwang
Initial revision


 	iv.	Lacunae

	Perhaps it would make sense to expose the zephyr noticed id,
	at this layer, so clients can interpret source ip addresses and
	message time stamps.


		1.	signature ZEPHYR_CLIENT
*)

signature ZEPHYR_CLIENT =
sig

(*
	Message types with the same semantics and meaning as those used in
	the zephyr specification.
*)

    datatype message_kind =
	UNSAFE
      | UNACKED
      | ACKED
      | SERVACK
      | SERVNACK
      | CLIENTACK
      | STAT

(*
	Values the determine how the zephyr server will advertise the location
	of a zephyr user. All have similar semantics as those in the zephyr
	specification.

	N.B. Because the exposure level "NONE", as mentioned in the zephyr
	specification, conflicts with the SML option value NONE,
	"NO_EXPOSURE" is used in it's place.
*)

    datatype exposure_level =
	NO_EXPOSURE
      | OPSTAFF
      | REALM_VISIBLE
      | REALM_ANNOUNCED
      | NET_VISIBLE
      | NET_ANNOUNCED

(*
	A "zid" is the cannoncial way to identify a zephyr user.
	Where "name" represents the users kerberos identity and "realm"
	specifies the user realm. A "realm" value of NONE should default to
	the current realm.
*)

    datatype zid = Zid of
	{name:string, realm:string option}

(*
	Vanilla zephyr messages are sent and received via the
	"Message" constructor. The "Login" and "Logout" constructors
	are used to notify the zephyr server of a users location, or
	to receive notification of the comings and goings of other
	zephyr users. The "Notice" constructor provides a way to
	send and receive messages that do not fall into any particular
	category.
*)
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
	The "zuser" type encapsulates all the state associated with a
	user.
*)

    type zuser

(*
	Creation and destruction functions. The "auth" parameter to "zlogin"
	determines if the layer should attempt to authenticate the
	user.

	N.B. If unauthenticated, the server server will only honor
	send and possibly zlocate requests. All subscription
	operations will fail.
*)

    val zlogin : {me:zid,auth:bool} -> zuser
    val zlogout : zuser -> unit

(*
 	Send does the obvious. Receive installs a handler that will be
	called when a message is received. Repeated invocations of
	"receive" replace the previous handler with the new handler.
*)

    val send : zuser -> zephyr_message -> unit
    val receive : zuser -> (zephyr_message -> unit) -> unit

(*
	Several zephyr related RPCs.
*)
    val unsubscribe : zuser -> {class:string,
		            instance:string,
		           recipient:string} list -> unit

    val subscribe : zuser -> {default_subs:bool,
			           subs:{class:string,
			              instance:string,
		                     recipient:string} list} -> unit

    val list_subscriptions : zuser -> {class:string,
				    instance:string,
		                   recipient:string} list

    val list_default_subscriptions : zuser -> {class:string,
			                    instance:string,
		                           recipient:string} list

    val locate_user : zuser -> zid -> {hostname:string,
				           time:string,
			               location:string} list
(*
	A function used to extract the "zid" from a "zuser", useful
	for filling in the "sender" and "who" fields of zephyr
	messages, before a call to send.
*)

    val zuser_to_zid : zuser -> zid

(*
	These functions convert strings of the form "user@realm" to and
	from zid datatypes.
*)
    val string_to_zid : string -> zid
    val zid_to_string : zid -> string

end
