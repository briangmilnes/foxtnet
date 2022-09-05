(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

		ping.fun: a simple implementation of echo request/reply.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: ipping.tst,v $
Revision 1.6  1996/04/30  20:28:28  esb
adapted to new vendor.sig, ip.fun

Revision 1.5  1996/02/06  23:39:33  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.4  1995/11/12  16:34:52  esb
adapted to new Word_Array.

Revision 1.3  1995/10/17  21:48:06  esb
minor changes in the debugging statements.

Revision 1.2  1995/10/04  21:33:35  esb
improved reliability.

Revision 1.1  1995/09/21  14:10:37  esb
Initial revision


		1.	functor Simple_Ping
*)

functor Simple_Ping (structure B: FOX_BASIS
		     val interface: string
		     val gateways: Word32.word list
		     val debug_level: int ref option) =
 struct
  local
   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "ipping.tst"
			    val makestring = fn _ => NONE)
  
   structure Dev = Build_Eth_Dev (structure B = B
				  val debug_level = debug_level)

   fun get_local_address {send, local_address, packets_sent, packets_received,
			  read_timeouts, failed_sends, packets_rejected} =
        let val local_array48 = Word48_Array.to local_address
	    val local_48 = Word48_Array.U_Big.F.head local_array48
	    fun session () = local_48
	    fun receive _ = ()
        in (session, receive)
        end

   fun local_ip local_eth =
        case Test_Addresses.eth_ip local_eth of
	   NONE => Word32.fromInt 0
	 | SOME main_stack_ip =>
	    case Test_Addresses.ip_name main_stack_ip of
	       NONE => main_stack_ip
	     | SOME name =>
		case Test_Addresses.name_ip (name ^ ".foxnet") of
		   NONE => main_stack_ip
		 | SOME foxnet_ip => foxnet_ip

   structure Eth = Ethernet (structure Device = Dev.Dev
			     structure B = B
			     val debug_level = debug_level)

   structure Arp = Arp_Eth (structure Eth = Eth
			    val arp_protocol_number = Word16.fromInt 0x806
			    structure B = B
			    val debug_level = debug_level)

   structure Mux = Ip_Mux1 (structure Arp = Arp
			    val arp_setup = ()
			    val interface_to_arp_setup =
			           fn n => {interface_name = n}
			    val ip_protocol_number = Word16.fromInt 0x800
			    structure B = B)

   structure Ip = Ip (structure Lower = Mux
		      structure Host_Id = Ip_Host_Id
		      structure B = B
		      val icmp_protocol = Word8.fromInt 1
		      val gateway = false
		      val debug_level = debug_level)

   structure Icmp = Ip.Icmp

   val zero16 = Word16.fromInt 0
   val one16 = Word16.fromInt 1

   fun connect_fun (0, _, _, _) _ =
        B.Scheduler.sleep 5000 (* wait for stragglers *)
     | connect_fun (count, id, seq, pipe) (conn as (Icmp.C {send, ...})) =
        (B.Pipe.enqueue (pipe, (id, seq, B.V.Time.now ()));
	 send (Icmp.Echo_Request {id = id,
				  sequence = seq,
				  data = Ip.Outgoing.uninitialized 0w33});
	 B.Scheduler.sleep 1000;
	 connect_fun (count - 1, id, Word16.+ (seq, one16), pipe) conn)

   fun data_fun pipe (conn as (Icmp.C {send, ...}),
		      packet as (Icmp.Echo_Reply {id, sequence, data})) =
        let val rcv_time = B.V.Time.now ()
	    val (send_id, send_seq, send_time) = B.Pipe.dequeue pipe
	in if id = send_id andalso sequence = send_seq then
	    Trace.local_print (Word.toString (Ip.Incoming.size data) ^
			       " bytes: seq = " ^
			       Word16.toString sequence ^
			       ", time " ^
			       B.V.Time.toString (B.V.Time.- (rcv_time,
							      send_time)))
	   else if id = send_id then
	    if Word16.> (sequence, send_seq) then
	     (Trace.trace_print (fn _ => "skipping sequence number " ^
				 Word16.toString send_seq);
	      data_fun pipe (conn, packet))
	    else
	     (Trace.local_print ("duplicate or out-of-order sequence number " ^
				 Word16.toString sequence ^
				 ", expecting " ^
				 Word16.toString send_seq);
	      B.Pipe.requeue (pipe, (send_id, send_seq, send_time)))
	   else
	    Trace.local_print ("received reply with ID " ^
			       Word16.toString id ^ ", expected " ^
			       Word16.toString send_id)
	end
     | data_fun pipe (Icmp.C {send, ...}, data) =
        Trace.local_print ("received data " ^
			   Icmp.Incoming.makestring data)

   fun status_fun (Icmp.C {send, ...}, status) =
        Trace.local_print ("received status " ^
			   Icmp.Status.makestring status)

   fun handler (count, id, seq) key =
        let val pipe = B.Pipe.new ()
	in {connection_handler = connect_fun (count, id, seq, pipe),
	    data_handler = data_fun pipe,
	    status_handler = status_fun}
	end

   fun session_fun (remote_ip, count) (Icmp.S {connect, listen, ...}) =
        let val id = B.V.Time.toMilliseconds (B.V.Time.now ()) mod 256
	in connect (remote_ip,
		    Icmp.H (handler (count, Word16.fromInt id, zero16)))
	end

   exception Unable_To_Parse_Ip_Number

  in
   fun run (name, count) =
        case Ip.Host_Id.parse name of
	   NONE => Trace.print_raise (Unable_To_Parse_Ip_Number, NONE)
	 | SOME remote_ip =>
	    let val local_eth = Dev.Raw.session (interface, get_local_address)
	        val local_ip = local_ip local_eth
	        val setup = [{local_id = local_ip, interface = interface,
			      gateways = gateways, mtu = NONE,
			      mask = NONE}]
	    in Trace.local_print ("local IP " ^
				  Ip.Host_Id.makestring local_ip ^
				  ", remote IP " ^
				  Ip.Host_Id.makestring remote_ip);
	       Icmp.session (Ip.Network_Setup.Setup setup,
			     session_fun (remote_ip, count))
	    end

   fun serve sleep_time =
        let val local_eth = Dev.Raw.session (interface, get_local_address)
	    val local_ip = local_ip local_eth
	    val setup = [{local_id = local_ip, interface = interface,
			  gateways = gateways, mtu = NONE,
			  mask = NONE}]
	in Trace.local_print ("serving local IP " ^
			      Ip.Host_Id.makestring local_ip);
	   Icmp.session (Ip.Network_Setup.Setup setup,
			 fn _ => B.Scheduler.sleep (sleep_time * 1000))
	end

  end
 end

(* CMU values *)
structure Simple_Ping = Simple_Ping (structure B = Fox_Basis
				     val interface = "ln0"
				     val gateways =
				           [Test_Addresses.get_ip "gw"]
				     val debug_level = NONE)
