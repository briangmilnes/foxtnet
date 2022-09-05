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

	A file of tests for IP options.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Ip_Options
	2.	structure Test_Ip_Options

		iii.	RCS Log
	
$Log: ipoptions.tst,v $
Revision 1.18  1995/03/12  17:50:04  esb
adapted to new trace.sig.

Revision 1.17  1995/03/10  03:46:34  esb
adapted to new vendor.sig.

Revision 1.16  1995/03/07  20:35:44  esb
updated tracing.

Revision 1.15  1995/02/21  13:04:11  esb
upgraded for SML/NJ 1.07.

Revision 1.14  1995/02/13  23:28:24  esb
adapted for 1.07

Revision 1.13  1995/01/18  21:02:13  esb
adapted to new coro.sig.

Revision 1.12  1995/01/06  16:55:08  esb
adapted to new Test_Addresses.

Revision 1.11  1994/11/22  13:56:09  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.10  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.9  1994/08/28  21:40:50  milnes
Added default gateway.

Revision 1.8  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.7  1994/07/01  02:28:03  danwang
Moved control structures into Fox_Basis.

Revision 1.6  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.5  1994/06/09  18:37:41  esb
adapted to new incoming_message type for IP.

Revision 1.4  1994/05/23  14:19:42  milnes
Installed Edo's naming changes in the old revision, plus changed
options to take the byte array of the message header, instead of
the packet.

Revision 1.3  1994/05/10  07:49:49  esb
adapted to new names in ipoptions.sig.

Revision 1.2  94/05/04  01:41:41  esb
brought up to date with the new coroutine and event signatures.

Revision 1.1  94/05/03  21:09:19  esb
Initial revision


		1.	functor Test_Ip_Options

  The tests for ip options are:

 1) can send out options
 2) can set a handler and receive all of the options that I sent
 3) options fragment and reassemble nicely
 4) Time_Stamp is installed if the last receiver address is me.
 5) a message to an icmp echo port with a source routing returns with
    the right data in it.

 other reasonable ones are:
  set options, send two messages, and they both arrive with the same options.
  clear those options, send again, and get no options.

*)

functor Test_Ip_Options (structure B: FOX_BASIS
			 val debug_level: int ref option): TEST_STRUCTURE  =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipoptions.tst")
  val local_print = Trace.local_print
  val no_debug_level = NONE

  structure Sim = Build_Simulators (structure B = B
				    val debug_level = no_debug_level)

  val ip_over_eth = FoxWord16.intToWord 0x888

  val quick = Test_Addresses.get_ip "quick"
  val snow = Test_Addresses.get_ip "snow"
  val loop = SW.n32 "0x7f000001"

  structure Build_Quick = Build_Ip (structure Device = Sim.Quick
				    structure B = B
				    val ip_protocol = ip_over_eth
				    val eth_debug_level = no_debug_level
				    val arp_debug_level = no_debug_level
				    val ip_debug_level = debug_level)

  structure Build_Snow = Build_Ip (structure Device = Sim.Snow
				   structure B = B
				   val ip_protocol = ip_over_eth
				   val eth_debug_level = no_debug_level
				   val arp_debug_level = no_debug_level
				   val ip_debug_level = debug_level)

  fun init_stacks () = 
       (Build_Quick.Ip_Mux.add ("SE0", Test_Addresses.get_ip "quick");
	Build_Quick.Ip.initialize ();
	Build_Quick.Ip.set_interface_address ("SE0",
					      Test_Addresses.get_ip "quick");
	Build_Snow.Ip_Mux.add ("SE0", Test_Addresses.get_ip "snow");
	Build_Snow.Ip.initialize ();
	Build_Snow.Ip.set_interface_address ("SE0",
					     Test_Addresses.get_ip "snow"))

  fun fin_stacks () = 
       (Build_Quick.Ip.finalize ();
        Build_Snow.Ip.finalize ();
	())

  structure Quick_Eth = Build_Quick.Eth
  structure Snow_Eth = Build_Snow.Eth

  structure Quick = Build_Quick.Ip
  structure Snow = Build_Snow.Ip

  val test_protocol = SW.n8 "66"

  fun fill_packet n = FoxWord8.intToWord (n mod 256)

  val quick_address_for_snow = Quick.Address {ip = snow, proto = test_protocol}

  val snow_address_for_quick = Snow.Address {ip = quick, proto = test_protocol}

  val quick_address_pattern_for_snow =
       Quick.Address {proto = test_protocol, ip = snow}

  val expected_result =
       B.Compare_Dyn_Array.create (B.Tabulate.tabulate (fill_packet, 40))

  fun compare_data (expected, packet) () =
       let val result = B.Compare_Dyn_Array.compare (expected, packet)
       in if result = [] then true
          else
	   (B.Compare_Dyn_Array.compare_and_print ("", expected) packet;
	    false)
       end

  fun test_send (packet, send) =
       (B.Dyn_Array.update (packet, 0,
			    B.Dyn_Array.read
			     (B.Dyn_Array.init1
			      (B.Dyn_Array.size packet, fill_packet)));
	send ())

  fun run_a_test (quick_handler, send, snow_handler) =
       (Quick.initialize ();
        Snow.initialize ();
        let val (stop, connections) =
                 Quick.start_passive (quick_address_for_snow, quick_handler,
				      SOME 100)
	    val snow_connection_to_quick =
	         Snow.connect (snow_address_for_quick, snow_handler)
            val sleep_time = if Trace.debug_on () then 30000 else 1000
        in send snow_connection_to_quick;
	   B.Scheduler.sleep sleep_time;
	   Snow.close snow_connection_to_quick;
	   map Quick.close (connections ());
	   stop ()
        end;
        Quick.finalize ();
        Snow.finalize ();
        ())

  local
   fun print_options (o1, o2) =
        (local_print ("compare options failed, " ^
		      Quick.Option.makestring_options o1 ^ " <> " ^
		      Quick.Option.makestring_options o2 ^ "\n");
         false)

   fun compare_triple ({length = l1, position = p1, route = r1},
		       {length = l2, position = p2, route = r2}) =
        l1 = l2 andalso p1 = p2 andalso List.length r1 = List.length r2

   fun compare_stamps (Quick.Option.Stamp list1, Quick.Option.Stamp list2) =
        list1 = list2
     | compare_stamps (Quick.Option.Record_Stamp list1,
		       Quick.Option.Record_Stamp list2) = list1 = list2
     | compare_stamps (Quick.Option.Given_Stamp list1,
		       Quick.Option.Given_Stamp list2) = list1 = list2
     | compare_stamps _ = false

   fun compare_time ({length = l1, position = p1, overflow = o1, stamps = s1},
		     {length = l2, position = p2, overflow = o2,
		      stamps = s2}) =
        l1 = l2 andalso p1 = p2 andalso o1 = o2 andalso compare_stamps (s1, s2)

  in	
   fun compare_options (o1 as [Quick.Option.Strict_Route triple1],
			o2 as [Quick.Option.Strict_Route triple2]) () =
        if compare_triple (triple1, triple2) then true
	else
	 (print_options (o1, o2);
	  false)
     | compare_options (o1 as [Quick.Option.Loose_Route triple1],
			o2 as [Quick.Option.Loose_Route triple2]) () =
        if compare_triple (triple1, triple2) then true
	else
	 (print_options (o1, o2);
	  false)
     | compare_options (o1 as [Quick.Option.Record_Route triple1],
			o2 as [Quick.Option.Record_Route triple2]) () =
        if compare_triple (triple1, triple2) then true
	else
	 (print_options (o1, o2);
	  false)
     | compare_options (o1 as [Quick.Option.Time_Stamp time1],
			o2 as [Quick.Option.Time_Stamp time2]) () =
        if compare_time (time1, time2) then true
	else
	 (print_options (o1, o2);
	  false)
     | compare_options (x, y) () = (print_options (x, y); false)
  end (* local *)

  fun status_handler (name, expected) (Quick.Option_Packet (options, _)) =
       B.Test.test (name ^ " options", compare_options (expected, options))
    | status_handler (name, expected) _ =
       B.Test.test (name ^ " options", fn _ => false)

  fun data_handler name (Quick.Ip_Packet {data, ...}) =
       B.Test.test (name ^ " packet", compare_data (expected_result, data))

  fun send_options options connection =
       (Snow.set_options (connection, options);
	let val allocated = Snow.allocate_send (connection, 40)
	in test_send allocated
	end)

  fun noop _ = ()			(* handlers for snow *)
       
  fun build_args (name, options_to_send, expected_options) =
       (Quick.Handler (fn _ => (data_handler name,
				status_handler (name, expected_options))),
	send_options options_to_send,
        Snow.Handler (fn _ => (noop, noop)))

  fun run_tests () =
    (* Full strict source route. *)
       (let val route = [SW.n32 "0xFFFFFFFF"]
            val send_option = {length = SW.n8 "7", position = SW.n8 "8",
			       route = route}
            val rcv_option = {length = SW.n8 "7", position = SW.n8 "8",
			      route = route}
	    val send = [Snow.Option.Strict_Route send_option]
	    val receive = [Quick.Option.Strict_Route rcv_option]
	in run_a_test (build_args ("strict source route", send, receive))
	end;
    (* Overflow Time_Stamp. *)
        let val send_option = {length = SW.n8 "4", position = SW.n8 "5",
			       overflow = SW.n8 "0",
			       stamps = Snow.Option.Stamp []}
	    val rcv_option = {length = SW.n8 "4", position = SW.n8 "5",
			      overflow = SW.n8 "1",
			      stamps = Quick.Option.Stamp []}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("overflow time stamp", send, receive))
	end;
    (* One slot for a Time_Stamp. *)
        let val send_option = {length = SW.n8 "8", position = SW.n8 "5",
			       overflow = SW.n8 "0",
			       stamps = Snow.Option.Stamp []}
	    val rcv_option = {length = SW.n8 "8", position = SW.n8 "9",
			      overflow = SW.n8 "0",
			      stamps = Quick.Option.Stamp [SW.n32 "0"]}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("time stamp", send, receive))
	end;
    (* One slot for a Time_Stamp and address. *)
        let val send_option = {length = SW.n8 "12", position = SW.n8 "5",
			       overflow = SW.n8 "0",
			       stamps = Snow.Option.Record_Stamp []}
	    val rcv = Quick.Option.Record_Stamp [{ip = snow,
						  time = SW.n32 "0"}]
	    val rcv_option = {length = SW.n8 "12", position = SW.n8 "5",
			      overflow = SW.n8 "0",
			      stamps = rcv}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("address and time stamp", send, receive))
	end;
    (* Overflow Time_Stamp and address. *)
        let val send_option = {length = SW.n8 "8", position = SW.n8 "5",
			       overflow = SW.n8 "0",
			       stamps = Snow.Option.Record_Stamp []}
	    val rcv_option = {length = SW.n8 "8", position = SW.n8 "5",
			      overflow = SW.n8 "1",
			      stamps = Quick.Option.Record_Stamp []}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("overflow address and time", send, receive))
	end;
    (* No space for a Time_Stamp that matches the address. *)
        let val send_option = {length = SW.n8 "12", position = SW.n8 "8",
			       overflow = SW.n8 "0",
			       stamps =
			         Snow.Option.Given_Stamp [{ip = quick,
							   time = NONE}]}
	    val rcv_option = {length = SW.n8 "12", position = SW.n8 "8",
			      overflow = SW.n8 "1",
			      stamps =
			        Quick.Option.Given_Stamp [{ip = quick,
							   time = NONE}]}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("overflow given address", send, receive))
	end;
    (* One slot for a Time_Stamp that matches the address. *)
        let val send_option = {length = SW.n8 "12", position = SW.n8 "8",
			       overflow = SW.n8 "0",
			       stamps =
			         Snow.Option.Given_Stamp [{ip = quick,
							   time = NONE}]}
	    val rcv_option = {length = SW.n8 "12", position = SW.n8 "16",
			      overflow = SW.n8 "0",
			      stamps =
			        Quick.Option.Given_Stamp [{ip = quick,
							   time =
							   SOME (SW.n32 "0")}]}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("given address", send, receive))
	end;
    (* One slot for a Time_Stamp that does not match the address. *)
        let val send_option = {length = SW.n8 "12", position = SW.n8 "8",
			       overflow = SW.n8 "0",
			       stamps =
			         Snow.Option.Given_Stamp [{ip = snow,
							   time = NONE}]}
	    val rcv_option = {length = SW.n8 "12", position = SW.n8 "8",
			      overflow = SW.n8 "0",
			      stamps =
			        Quick.Option.Given_Stamp [{ip = snow,
							   time = NONE}]}
	    val send = [Snow.Option.Time_Stamp send_option]
	    val receive = [Quick.Option.Time_Stamp rcv_option]
	in run_a_test (build_args ("given address, no match", send, receive))
	end)

  fun run () =
       (B.Scheduler.reset ();
	init_stacks ();
	B.Test.tests ("IpOption", 8, run_tests);
	fin_stacks ())

  val _ = if ! B.Debug.do_tests then run () else ()
 end (* struct *)

(*

	2.	structure Test_Ip_Options

*)

structure Test_Ip_Options = Test_Ip_Options (structure B = Fox_Basis
					     val debug_level = NONE)
