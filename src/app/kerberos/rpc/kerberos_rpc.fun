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

	This functor will generate an rpc call from Kerberos packets
	to Kerberos packets.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Kerberos_RPC

		iii.	RCS Log
	
$Log: kerberos_rpc.fun,v $
Revision 1.1  1994/08/25  12:13:35  robby
Initial revision

Revision 1.1  1994/07/13  18:48:52  robby
Initial revision


		1.	functor Kerberos_RPC
*)
functor Kerberos_RPC
  (structure Udp_Stack:BUILDUDP
   structure K:KERBEROS_LINEARIZE
   structure ByteArray_RPC:BYTEARRAY_RPC_CALL
   structure Realm:KRB_REALM
   structure B:FOX_BASIS

   val udp_over_ip:ubyte1
   val udp_local_port:ubyte2
   val kerberos_port:ubyte2
   val do_prints:bool

   sharing type Udp_Stack.Udp.incoming_message=B.Receive_Packet.T
   and type Udp_Stack.Udp.outgoing_message=B.Send_Packet.T
   and type Udp_Stack.Udp.lower_layer_address=Udp_Stack.Ip.ip_address
   and type ByteArray_RPC.address=Udp_Stack.Udp.address
   and type ByteArray_RPC.Arg.T=ByteArray_RPC.Res.T=ByteArray.bytearray):
     KERBEROS_RPC_CALL=
struct

  structure K=K
  structure Arg=K
  structure Res=K

  datatype Kerberos_Call_Error=No_Response | Other
  type error=Kerberos_Call_Error

  datatype realm=Realm of string
  type address=realm

  exception RPC_Not_Initialized of string
  exception Packet_Size of int
  exception Call_Failed of error * string
  exception Bad_Address of address * string

  type info=unit
  type control=unit
  fun query ()=()
  fun control ()=()

  val debug_print=if do_prints then (fn x=>B.V.print x)
		  else (fn _=>())      

  val initialize=Udp_Stack.Udp.initialize
  exception Initialization_Failed=Udp_Stack.Udp.Initialization_Failed
  val finalize=Udp_Stack.Udp.finalize

  fun kdc_address realm=
    let
      val kdc_ip  = Udp_Stack.Resolve.name_map
	(nth (Realm.kerberos_servers realm, 0))
	handle Nth => raise Call_Failed
	  (Other,"kerberos_rpc: No Kerberos servers found")
      val ip_addr = Udp_Stack.Ip.Address {ip=kdc_ip, proto=udp_over_ip}
    in
      Udp_Stack.Udp.Address {remote_peer = ip_addr,
			     local_port  = udp_local_port,
			     remote_port = kerberos_port}
    end

  fun call (Realm realm) outgoing=
    let val size=K.size outgoing
      val array=B.Create.create size
      val (K.Extern (array,_))=K.marshall (outgoing,K.Extern(array,0))
	handle K.Does_Not_Match => 
	  raise Call_Failed (Other,"kerberos_rpc: couldn't marshall the data")
      val address=kdc_address realm
      val incame=(ByteArray_RPC.call address array
		  handle ByteArray_RPC.Call_Failed (ByteArray_RPC.Other,x) => 
		    raise Call_Failed (Other,x)
		  | ByteArray_RPC.Call_Failed (ByteArray_RPC.No_Response,x) => 
		      raise Call_Failed (No_Response,x))
      val (answer,K.Extern (array,p))=K.unmarshall (K.Extern (incame,0))
	handle K.Does_Not_Match =>
	  raise Call_Failed (Other,"kerberos_rpc: couldn't unmarshall the data")
    in 
      answer
    end
end
