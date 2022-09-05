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

	These functors provide support for a generic RPC
	over UDP that sends and receives bytearrays.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor ByteArray_Extern
	2.	functor ByteArray_RPC_Call_Over_Udp

		iii.	RCS Log
	
$Log: bytearray_rpc.fun,v $
Revision 1.1  1994/08/25  12:13:35  robby
Initial revision

Revision 1.1  1994/07/13  18:44:08  robby
Initial revision


	1.	functor ByteArray_Extern
*)

(* This functor is intuively an identity in bytearrays.
   The reason for the option is so that you don't have to allocate anything,
   to initialize things. This doesn't really fit the EXTERN model for
   RPCs *)

functor ByteArray_Extern():EXTERN=
struct

  type T=ByteArray.bytearray
  type incoming=ByteArray.bytearray option
  type outgoing=ByteArray.bytearray option

  exception Does_Not_Match

  val size=ByteArray.length
  fun marshall (b,_)=SOME b
  fun unmarshall (SOME b)=(b,SOME b)
    | unmarshall NONE=raise Does_Not_Match

end

(*
	2.	functor ByteArray_RPC_Call_Over_Udp
*)

(* resend_count is the maximum number of times to send the packet, and
 sleep_time is the amount of time to wait between tries.*)
functor ByteArray_RPC_Call_Over_Udp
  (structure Udp_Stack:BUILDUDP
   structure Scheduler:COROUTINE
   structure B:FOX_BASIS
   sharing type Udp_Stack.Udp.incoming_message=B.Receive_Packet.T
   sharing type Udp_Stack.Udp.outgoing_message=B.Send_Packet.T
   sharing type Udp_Stack.Udp.lower_layer_address=Udp_Stack.Ip.ip_address
   val sleep_time:int
   val resend_count:int
   val udp_header_size:int
   val do_prints:bool):BYTEARRAY_RPC_CALL=
struct

  structure Arg=ByteArray_Extern()
  structure Res=Arg

  datatype ByteArray_Call_Error=No_Response | Other
  type error=ByteArray_Call_Error

  type address=Udp_Stack.Udp.address

  type control=Udp_Stack.Udp.control
  val control=Udp_Stack.Udp.control
  type info=Udp_Stack.Udp.info
  val query=Udp_Stack.Udp.query

  val do_if_debug = if B.Debug.include_prints andalso do_prints then
    (fn (f, x) => if ! B.Debug.do_prints then f x else ())
		    else (fn (f, x) => ())

  fun debug_print s = do_if_debug (B.V.print,s)

  exception RPC_Not_Initialized of string
  exception Packet_Size of int
  exception Call_Failed of error * string
  exception Bad_Address of address * string
  
  val (local_ip, local_interface, gateway_ip) = Udp_Stack.Resolve.local_info ()
  
  exception Initialization_Failed=Udp_Stack.Udp.Initialization_Failed

  val initialize=Udp_Stack.Udp.initialize
  val finalize=Udp_Stack.Udp.finalize
    
  fun call address request=
    let val response = ref (NONE:(Udp_Stack.Udp.incoming_message option))
      val outgoing_packet=B.Send_Packet.create (request,udp_header_size)
      fun handler connection message=
	(debug_print 
	 (let val array=B.Receive_Packet.read message
         in
	    ("bytearray_rpc: Receiving " ^ 
	     ByteArray.extract (array,0,ByteArray.length array) ^
	    "\n"^(B.Format.makestring_bytearray array)^"\n")
         end);
	    response := SOME message;
	    Udp_Stack.Udp.close connection)
      val connection=Udp_Stack.Udp.active_open (address,handler)
	handle (Udp_Stack.Udp.Protocol_Not_Initialized x)=>
	  raise RPC_Not_Initialized x
      fun send_request outgoing_packet=
	(debug_print ("bytearray_rpc: Sending " ^ 
		      ByteArray.extract (request,0,ByteArray.length request) ^
		      "\n"^(B.Format.makestring_bytearray request)^"\n");
	 Udp_Stack.Udp.send connection outgoing_packet
	 handle (Udp_Stack.Udp.Protocol_Not_Initialized s)=>
	   raise RPC_Not_Initialized s
	      | (Udp_Stack.Udp.Invalid_Connection (_,_,s)) => 
		  raise Call_Failed (Other,s)
	      | (Udp_Stack.Udp.Packet_Size s) => raise Packet_Size s)
      fun handle_resends count=
	case !response of
	  SOME response_bytearray =>
	    B.Receive_Packet.read response_bytearray
	| NONE => (if count <= 0 then
		     raise Call_Failed (No_Response,
					"no response from the server")
		   else
		     (send_request outgoing_packet;
		      Scheduler.sleep sleep_time;
		      handle_resends (count-1)))
		   
    in
      handle_resends resend_count
    end
end
