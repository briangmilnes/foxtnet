(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A file to test that ip is correctly generating ip specific
	status messages.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Ip_Local_Status
	2.	function run_test

		iii.	RCS Log
	
$Log: iplocalstatus.tst,v $
Revision 1.8  1995/03/12  17:55:39  esb
adapted to new trace.sig.

Revision 1.7  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.6  1995/02/21  13:17:30  esb
upgraded for SML/NJ 1.07.

Revision 1.5  1995/01/18  21:07:24  esb
added scheduler resets and made possible to run multiple times.

Revision 1.4  1995/01/06  17:04:51  esb
adapted to new Test_Addresses.

Revision 1.3  1994/11/22  13:56:55  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.2  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.1  1994/09/14  15:26:25  milnes
Initial revision


		1.	functor Test_Ip_Local_Status
*)

functor Test_Ip_Local_Status
           (structure B: FOX_BASIS
	    val debug_level: int ref option): TEST_STRUCTURE  =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "iplocalstatus.tst")
  val local_print = Trace.local_print
  val no_debug_level = NONE

  structure Sim = Build_Simulators (structure B = B
                                    val debug_level = no_debug_level)

  val ip_over_eth = SW.n16 "0x888"
  val quick = Test_Addresses.get_ip "quick"
  val snow = Test_Addresses.get_ip "snow"
  val loop = SW.n32 "0x7f000001"

  local 
   structure B1 = Build_Ip_Icmp (structure Device = Sim.Snow
				 structure B = B
				 val ip_over_eth = ip_over_eth
				 val log_icmp_echos = false
				 val eth_debug_level = no_debug_level
				 val arp_debug_level = no_debug_level
				 val ip_debug_level = no_debug_level
				 val icmp_debug_level = debug_level)
  in 
   fun init_snow () =
        (B1.Ip_Mux.add ("SE0", Test_Addresses.get_ip "snow");
	 B1.Ip.initialize ();
	 B1.Ip.set_interface_address ("SE0", Test_Addresses.get_ip "snow"))
   fun fin_snow () = 
	(B1.Ip.finalize ();
	 ())
   structure Snow_Ip = B1.Ip
   structure Snow_Eth = B1.Eth
  end

  local 
   structure B1 = Build_Ip_Icmp (structure Device = Sim.Quick
				 structure B = B
				 val ip_over_eth = ip_over_eth
				 val log_icmp_echos = false
				 val eth_debug_level = no_debug_level
				 val arp_debug_level = no_debug_level
				 val ip_debug_level = no_debug_level
				 val icmp_debug_level = debug_level)
  in 
   fun init_quick () =
        (B1.Ip_Mux.add ("SE0", Test_Addresses.get_ip "quick");
	 B1.Ip.initialize ();
	 B1.Ip.set_interface_address ("SE0", Test_Addresses.get_ip "quick"))
   fun fin_quick () = 
	(B1.Ip.finalize ();
	 ())
   structure Quick_Ip = B1.Ip
  end

  val init_stacks = init_quick o init_snow
  val fin_stacks = fin_quick o fin_snow

  val quick_ip_for_snow = 
       Quick_Ip.Address {ip = Test_Addresses.get_ip "snow", proto = SW.n8 "0"}

  val snow_eth_for_quick = 
       let val {a0, a1, a2, a3, a4, a5} = Test_Addresses.name_eth "quick"
       in Snow_Eth.Address {a0 = a0, a1 = a1, a2 = a2, a3 = a3, a4 = a4,
			    a5 = a5, proto = ip_over_eth}
       end 

(* 
		2.	function run_test

	Send over ethernet an Ip packet that specifies a fragment, and
	then send nothing else. The receiver should eventually
	generate a status message.
*)

  fun run_test () =
       let val pipe = B.Pipe.new NONE
	   fun fail_test _ = B.Pipe.enqueue (pipe, false)
	   fun check_status (Quick_Ip.Ip_Specific _) =
	        B.Pipe.enqueue (pipe, true)
	     | check_status _ = fail_test ()
	   fun test_receive connection = (fail_test, check_status)
	   val _ = Quick_Ip.initialize ()
	   val _ = Snow_Ip.initialize ()
	   val quick_connection_to_snow = 
	        Quick_Ip.connect (quick_ip_for_snow,
				  Quick_Ip.Handler test_receive)
	   fun dummy _ = ()
	   fun dummy_pair _ = (dummy, dummy)
	   val snow_connection_to_quick =
	        Snow_Eth.connect (snow_eth_for_quick,
				  Snow_Eth.Handler dummy_pair)
	   val (packet, send) =
	        Snow_Eth.allocate_send (snow_connection_to_quick, 52)
	   val ip_version = SW.n8 "4"
	   val htons = B.Order.B2.to_big
	   val htonl = B.Order.B4.to_big
	   fun fake_ip_fragment packet =
	        (B.Dyn_Array.update1 (packet, 0,
				      FoxWord8.orb
				      (FoxWord8.lshift (ip_version, 4),
				       SW.n8 "5")); (* Verhlen.*)
		 B.Dyn_Array.update1 (packet, 1, SW.n8 "0"); (* tos. *)
		                        (* use a "wrong length" of 500 *)
		 B.Dyn_Array.update2 (packet, 2, htons (SW.n16 "500"));
		 B.Dyn_Array.update2 (packet, 4, htons (SW.n16 "0")); (*Id*)
		 B.Dyn_Array.update1 (packet, 6, SW.n8 "1"); (* more frags. *)
		 B.Dyn_Array.update1 (packet, 7, SW.n8 "0"); (* First frag. *)
		 B.Dyn_Array.update1 (packet, 8, SW.n8 "1"); (* TTL 1 sec.*)
		 B.Dyn_Array.update1 (packet, 9, SW.n8 "0"); (* Protocol. *)
		 B.Dyn_Array.update4 (packet, 12,
				      htonl (Test_Addresses.get_ip "snow"));
		 B.Dyn_Array.update4 (packet, 16,
				      htonl (Test_Addresses.get_ip "quick"));
		 B.Dyn_Array.update4 (packet, 20, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 24, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 28, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 32, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 36, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 40, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 44, SW.n32 "0xFFFFFFFF");
		 B.Dyn_Array.update4 (packet, 48, SW.n32 "0xFFFFFFFF");
	                           (* initially set the checksum to zero *)
		 B.Dyn_Array.update2 (packet, 10, SW.n16 "0");
		 B.Dyn_Array.update2 (packet, 10, 
				      htons (B.Checksum.one_s_complement 
					     (B.Checksum.checksum
					      (B.Dyn_Array.read packet,
					       0, 20)))))
	   val timeout = 45000
	   fun timeout_proc () =
	        (B.Scheduler.sleep timeout;
		 fail_test ())
	   fun run_test () =
	        (B.Scheduler.fork timeout_proc;
		 send ();
		 B.Pipe.dequeue pipe)
       in fake_ip_fragment packet;
	  B.Test.test ("trigger an ip local exception", run_test);
	  Quick_Ip.finalize ();
	  Snow_Ip.finalize ();
	  ()
       end

  fun run () =
       (B.Scheduler.reset ();
	init_stacks ();
	B.Test.tests ("iplocalstatus.fun", 1, run_test);
	fin_stacks ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* functor *)

structure Test_Ip_Local_Status =
              Test_Ip_Local_Status (structure B = Fox_Basis
				    val debug_level = NONE)

