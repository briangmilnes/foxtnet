(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

		iptrace.tst: a simple implementation of traceroute.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Simple_Trace

		iii.	RCS Log
	
$Log: iptrace.tst,v $
Revision 1.2  1996/02/06  23:39:33  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.1  1995/10/04  21:34:24  esb
Initial revision


		1.	functor Simple_Trace
*)

functor Simple_Trace (structure B: FOX_BASIS
		      val interface: string
		      val gateways: Word32.word list
		      val debug_level: int ref option) =
 struct
  local
   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "iptrace.tst"
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
			     val debug_level = NONE)

   structure Arp = Arp_Eth (structure Eth = Eth
			    val arp_protocol_number = Word16.fromInt 0x806
			    structure B = B
			    val debug_level = NONE)

   structure Mux = Ip_Mux1 (structure Arp = Arp
			    val arp_setup = ()
			    val interface_name = interface
			    val ip_protocol_number = Word16.fromInt 0x800
			    structure B = B)

   structure Ip = Ip (structure Lower = Mux
		      structure B = B
		      val icmp_protocol = Word8.fromInt 1
		      val debug_level = NONE)

   val zero16 = Word16.fromInt 0
   val one16 = Word16.fromInt 1
   val udp_over_ip = Word8.fromInt 17

   fun connect_fun (30, _, _, _) _ =
        B.Scheduler.sleep 5000 (* wait for stragglers *)
     | connect_fun (_, _, ref true, _) _ = () (* done *)
     | connect_fun (count, pipe, done, data)
	           (conn as (Ip.C {send, extension, ...})) =
	let val Ip.Connection_Extension {set_time_to_live, ...} = extension
        in set_time_to_live count;
	   B.Pipe.enqueue (pipe, B.V.Time.gettimeofday ());
	   send data;
	   B.Scheduler.sleep 1000;
(*
           B.Pipe.enqueue (pipe, B.V.Time.gettimeofday ());
	   send data;
	   B.Scheduler.sleep 1000;
           B.Pipe.enqueue (pipe, B.V.Time.gettimeofday ());
	   send data;
	   B.Scheduler.sleep 1000;
*)
	   connect_fun (count + 1, pipe, done, data) conn
	end

   fun data_fun (pipe, done) (conn as (Ip.C {send, ...}), packet) =
        Trace.local_print ("got data packet " ^
			   Ip.Incoming.makestring packet)

   fun status_fun (Ip.C {send, ...}, status) =
        Trace.local_print ("received status " ^ Ip.Status.makestring status)

   fun handler key =
        let val pipe = B.Pipe.new ()
	    val done = ref false
	    fun convert_list [] = NONE
	      | convert_list (head :: rest) = SOME (Word8.fromInt head, rest)
	    val udp_head = [1, 2, 234, 235,
			    0, 8, 0, 0]
	    val raw_data = Word_Array.W8.U_Big.F.new convert_list udp_head
	    val data = Ip.Outgoing.new (Word_Array.from8 raw_data)
	in {connection_handler = connect_fun (1, pipe, done, data),
	    data_handler = data_fun (pipe, done),
	    status_handler = status_fun}
	end

   fun session_fun remote_ip (Ip.S {connect, listen, ...}) =
        let val id = (B.V.Time.timeusec (B.V.Time.gettimeofday ())
		      div 1000) mod 256
	in connect (remote_ip, Ip.H handler)
	end

   exception Unable_To_Parse_Ip_Number

  in
   fun run name =
        case Ip.Host_Id.parse name of
	   NONE => Trace.print_raise (Unable_To_Parse_Ip_Number, NONE)
	 | SOME remote_ip =>
	    let val local_eth = Dev.Raw.session ("ln0", get_local_address)
	        val local_ip = local_ip local_eth
	        val setup = [{local_id = local_ip, interface = interface,
			      gateways = gateways, mtu = NONE,
			      mask = NONE}]
		val addr = Ip.Network_Address.Address {peer = remote_ip,
						       proto = udp_over_ip}
	    in Trace.local_print ("local IP " ^
				  Ip.Host_Id.makestring local_ip ^
				  ", remote IP " ^
				  Ip.Host_Id.makestring remote_ip);
	       Ip.session (Ip.Network_Setup.Setup setup,
			   session_fun addr)
	    end

   fun serve sleep_time =
        let val local_eth = Dev.Raw.session ("ln0", get_local_address)
	    val local_ip = local_ip local_eth
	    val setup = [{local_id = local_ip, interface = interface,
			  gateways = gateways, mtu = NONE,
			  mask = NONE}]
	in Trace.local_print ("serving local IP " ^
			      Ip.Host_Id.makestring local_ip);
	   Ip.Icmp.session (Ip.Network_Setup.Setup setup,
			    fn _ => B.Scheduler.sleep sleep_time)
	end

  end
 end

(* CMU values *)
structure Simple_Trace = Simple_Trace (structure B = Fox_Basis
				       val interface = "ln0"
				       val gateways =
				             [Test_Addresses.get_ip "gw"]
				       val debug_level = NONE)
				     