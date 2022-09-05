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



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature KERBEROS

		iii.	RCS Log
	
$Log: kerberos.sig,v $
Revision 1.3  1994/08/08  14:51:48  danwang
Minor change to service and principal datatypes.

Revision 1.2  1994/07/16  22:51:22  robby
added get_session_key

Revision 1.1  94/07/13  19:11:43  robby
Initial revision



		1.	signature KERBEROS
*)

signature KERBEROS =
sig

    datatype principal = Principal of {name_p:string,
				   instance_p:string,
				      realm_p:string}

    datatype service = Service of {name_s:string,
			       instance_s:string,
			          realm_s:string}

  type authenticator
  type credential

  structure Des:DES'

  exception Kerberos_Error of string

  val get_ticket_granting_credential : (unit -> string) -> principal
                                             -> credential
  val destroy_ticket_granting_ticket : unit -> unit

  (* The ubyte4 is a checksum that is inserted into the authenticator.
     authenticators will expire. They should be created and then used
     shortly thereafter.*)
  val build_authenticator : credential * ubyte4 -> authenticator

  (* This function involves sending an Appl_Request to the kerberos server.
     An Appl_Request contains a potentially service dependant checksum.
     However, both zephyr and afs use zero as the checksum, so for now
     zero is always sent.*)
  val get_credential : service -> credential

  (* This is provided to allow a user of kerberos to insert an authenticator
     in a packet where ever the service requires it.*)
  val authenticator_to_bytearray : authenticator -> ByteArray.bytearray

  (* This is provided so that a service can use a session key
  explicitly if it needs to.*)
  val get_session_key : credential -> Des.key

end
