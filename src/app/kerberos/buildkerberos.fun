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

	This functor builds a Kerberos functor

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: buildkerberos.fun,v $
Revision 1.2  1994/08/08  14:49:49  danwang
Add changes to get zephyr authentication working correctly.



Revision 1.1  1994/07/16  22:50:42  robby
Initial revision


		1.	...
*)
functor BuildKerberos (structure B:FOX_BASIS
		       structure Udp_Stack:BUILDUDP
		       val do_prints:bool
		       sharing type Udp_Stack.Ip.ip_address=
			 Udp_Stack.Udp.lower_layer_address
		       and type B.Send_Packet.T=Udp_Stack.Udp.outgoing_message
		       and type B.Receive_Packet.T=
			 Udp_Stack.Udp.incoming_message):KERBEROS=
struct
local
  (*
    According to RFC 1340, Kerberos should use port 88.  However,
    750 is the port number found in the C code for the CMU version.
    Additionally, CMU Kerberos talks to MIT Kerberos (which therefore
    must also recognize port 750.
   *)
  val kerberos_port=2u750
  val udp_over_ip=1ux11
  val udp_local_port=2u7750
  val use_arp=true
  val ip_over_eth=2ux800
  val arp_over_eth=2ux806
  val sleep_time=10000
  val resend_count=3
  val udp_header_size=20
  val high_priority_filter=true

  structure Swap=Swap_Bytes()

  structure Ubyte1=Ubyte1(val do_prints=do_prints
			  structure B=B)

  structure Ubyte2=Ubyte2(val do_prints=do_prints
			  structure B=B
			  val swap=false
			  structure Swap=Swap)

  structure Ubyte4=Ubyte4(val do_prints=do_prints
			  structure B=B
			  val swap=false
			  structure Swap=Swap)

  structure Rev_Ubyte2=Ubyte2(val do_prints=do_prints
			      structure B=B
			      val swap=true
			      structure Swap=Swap)

  structure Rev_Ubyte4=Ubyte4(val do_prints=do_prints
			      structure B=B
			      val swap=true
			      structure Swap=Swap)

  structure Int=Int(structure Ubyte4=Ubyte4)

  structure Rev_Int=Int(structure Ubyte4=Rev_Ubyte4)

  structure String=CString(val do_prints=do_prints
			   structure B=B
			   structure Access=B.Access)

  structure Des=DES'(structure B=B
		     val do_prints=false)

  structure Key=Des_Key_Linearize(structure Des=Des
				  structure Ubyte1=Ubyte1
				  val reverse=false)

  structure Error=Kerberos_Error()
  structure Error_Linearize=Kerberos_Error_Linearize(structure Error=Error
						     structure Int=Int)
  structure Rev_Error_Linearize=Kerberos_Error_Linearize(structure Error=Error
							 structure Int=Rev_Int)

  structure Tag=Kerberos_Tag(structure Ubyte1=Ubyte1)

  structure Auth_Request=Auth_Request(structure Ubyte1=Ubyte1
				      structure String=String
				      structure Ubyte4=Ubyte4)

  structure Rev_Auth_Request=Auth_Request(structure Ubyte1=Ubyte1
					  structure String=String
					  structure Ubyte4=Rev_Ubyte4)

  structure Auth_Reply=Auth_Reply(structure Ubyte1=Ubyte1
				  structure Ubyte2=Ubyte2
				  structure Ubyte4=Ubyte4
				  structure String=String
				  structure B=B
				  val do_prints=do_prints)

  structure Rev_Auth_Reply=Auth_Reply(structure Ubyte1=Ubyte1
				      structure Ubyte2=Rev_Ubyte2
				      structure Ubyte4=Rev_Ubyte4
				      structure String=String
				      structure B=B
				      val do_prints=do_prints)
				    
  structure Err_Reply=Err_Reply(structure Error_Linearize=Error_Linearize
				structure Error=Error
				structure Ubyte4=Ubyte4
				structure String=String
				val do_prints=do_prints)

  structure Rev_Err_Reply=Err_Reply(structure Error_Linearize=
				      Rev_Error_Linearize
				    structure Error=Error
				    structure Ubyte4=Rev_Ubyte4
				    structure String=String
				    val do_prints=do_prints)

  structure Appl_Request=Appl_Request(structure Ubyte1=Ubyte1
				      structure Ubyte4=Ubyte4
				      structure String=String
				      val do_prints=do_prints
				      structure B=B)

  structure Rev_Appl_Request=Appl_Request(structure Ubyte1=Ubyte1
					  structure Ubyte4=Rev_Ubyte4
					  structure String=String
					  val do_prints=do_prints
					  structure B=B)

  structure Appl_Err=Appl_Err(structure Error=Error
			      structure Error_Linearize=Error_Linearize
			      structure String=String)

  structure Rev_Appl_Err=Appl_Err(structure Error=Error
				  structure Error_Linearize=Rev_Error_Linearize
				  structure String=String)

  structure Authenticator=Authenticator(structure String=String
					structure Ubyte4=Ubyte4
					structure Ubyte1=Ubyte1)  

  structure Rev_Authenticator=Authenticator(structure String=String
					    structure Ubyte4=Rev_Ubyte4
					    structure Ubyte1=Ubyte1)

  structure Tgs_Request =
      Ticket_Granting_Request( structure Appl_Request = Appl_Request
			       structure Ubyte4 = Ubyte4
			       structure Ubyte1 = Ubyte1
			       structure String = String)

  structure Creds=Credentials(structure String=String
			      structure Des=Des
			      structure Key=Key
			      structure Ubyte4=Ubyte4
			      structure Ubyte1=Ubyte1
			      val do_prints=do_prints
			      structure B=B)

  structure Rev_Creds=Credentials(structure String=String
				  structure Ubyte4=Rev_Ubyte4
				  structure Des=Des
				  structure Key=Key
				  structure Ubyte1=Ubyte1
				  structure B=B
				  val do_prints=do_prints)
	
  structure Kerberos_Linearize=Kerberos_Linearize
    (structure Tag=Tag
     structure Auth_Request=Auth_Request
     structure Auth_Reply=Auth_Reply
     structure Err_Reply=Err_Reply
     structure Appl_Request=Appl_Request
     structure Appl_Err=Appl_Err
     structure Tgs_Request=Tgs_Request
     structure Rev_Auth_Request=Rev_Auth_Request
     structure Rev_Auth_Reply=Rev_Auth_Reply
     structure Rev_Err_Reply=Rev_Err_Reply
     structure Rev_Appl_Request=Rev_Appl_Request
     structure Rev_Appl_Err=Appl_Err

     structure B=B
     val do_prints=do_prints)

  structure ByteArray_RPC=ByteArray_RPC_Call_Over_Udp
    (structure Udp_Stack=Udp_Stack
     structure Scheduler=B.Scheduler
     structure B=B
     val sleep_time=sleep_time
     val resend_count=resend_count
     val udp_header_size=udp_header_size
     val do_prints=do_prints)

  structure Krb_Realm=Krb_Realm(val config_file="/etc/krb.conf")

  structure Kerberos_RPC=Kerberos_RPC(structure Udp_Stack=Udp_Stack
				      structure K=Kerberos_Linearize
				      structure ByteArray_RPC=ByteArray_RPC
				      structure Realm=Krb_Realm
				      structure B=B
				      val do_prints=do_prints
				      val udp_over_ip=udp_over_ip
				      val udp_local_port=udp_local_port
				      val kerberos_port=kerberos_port)

  structure Kerberos=Kerberos(structure K=Kerberos_RPC
			      structure Auth = Authenticator
			      structure Creds=Creds
			      structure Rev_Creds=Rev_Creds
			      structure B=B
			      val do_prints=do_prints)

in
(*
	1.	structure Kerberos
*)
open Kerberos
end
end
