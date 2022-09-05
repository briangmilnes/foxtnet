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

                i.      Abstract

        A test file for ICMP masks.


                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      functor Test_Icmp
        2.      test_icmp_run
        3.      test_icmp_runs


                iii.    RCS Log
        
$Log: addressmask.tst,v $
Revision 1.14  1995/03/24  16:02:53  esb
now works.

Revision 1.13  1995/03/12  17:55:39  esb
adapted to new trace.sig.

Revision 1.12  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.11  1995/02/21  13:17:30  esb
upgraded for SML/NJ 1.07.

Revision 1.10  1995/01/18  21:07:24  esb
added scheduler resets and made possible to run multiple times.

Revision 1.9  1995/01/17  21:08:43  esb
adapted to new icmp.sig.

Revision 1.8  1995/01/06  17:04:51  esb
adapted to new Test_Addresses.

Revision 1.7  1994/11/22  13:56:55  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/09/30  17:05:34  esb
changed DLOCS to Dyn_Locs.

Revision 1.4  1994/09/12  18:26:39  milnes
Updated for the new icmp.

Revision 1.3  1994/08/28  21:46:06  milnes
Added default gateways.

Revision 1.2  1994/08/26  14:33:37  milnes
Rechecked in with log string corrected so we get the change log.


        1.      functor Test_Icmp_Mask
*)

functor Test_Icmp_Mask (structure B: FOX_BASIS
                        val debug_level: int ref option): TEST_STRUCTURE  =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "addressmask.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print
  val debug_constant_string = Trace.debug_constant_string
  val no_debug_level = NONE

  structure Sim = Build_Simulators (structure B = B
                                    val debug_level = no_debug_level)

  val ip_over_eth = SW.n16 "0x888"
  val quick = Test_Addresses.get_ip "quick"
  val snow = Test_Addresses.get_ip "snow"
  val loop = SW.n32 "0x7f000001"

  local 
   structure B1 = Build_Icmp (structure Device = Sim.Snow
                              structure B = B
			      val ip_over_eth = ip_over_eth
	                      val serve_echos_and_address_masks = false
                              val log_icmp_echos = true
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
   structure Snow_Icmp = B1.Icmp
   structure Snow_Ip = B1.Ip
  end

  local 
   structure B2 = Build_Icmp (structure Device = Sim.Quick
			      structure B = B
			      val ip_over_eth = ip_over_eth
			      val serve_echos_and_address_masks = false
			      val log_icmp_echos = true
			      val eth_debug_level = no_debug_level
			      val arp_debug_level = no_debug_level
			      val ip_debug_level = no_debug_level
			      val icmp_debug_level = debug_level)

  in 
   fun init_quick () =
        (B2.Ip_Mux.add ("SE0", Test_Addresses.get_ip "quick");
	 B2.Ip.initialize ();
	 B2.Ip.set_interface_address ("SE0", Test_Addresses.get_ip "quick"))
   fun fin_quick () = 
	(B2.Ip.finalize ();
	 ())
   structure Quick_Icmp = B2.Icmp
   structure Quick_Ip = B2.Ip
  end

  val init_stacks = init_quick o init_snow
  val fin_stacks = fin_quick o fin_snow

  val quick_address_for_snow = Quick_Icmp.Icmp_Address snow
  val snow_address_for_quick = Snow_Icmp.Icmp_Address quick

(*
        2.      test_icmp_run
*)

  fun test_icmp_mask (name, run_the_test, test_reception) =
   let fun test_receive connection =
            (fn p => test_reception p,
	     fn _ => ())
       val _ = (debug_constant_string "About to initialize quick.";
		Quick_Icmp.initialize ();
		debug_constant_string "About to initialize snow.";
		Snow_Icmp.initialize ();
		debug_constant_string "Done initializing snow.")
       val _ = debug_constant_string "Snow trying to connect to quick."
       val snow_connection_to_quick = 
	    Snow_Icmp.connect (snow_address_for_quick,
			       Snow_Icmp.Handler test_receive)
       val sleep_time = 1000
   in debug_print (fn _ => "in test_icmp_mask for test " ^ name);
      Snow_Icmp.set_service (snow_connection_to_quick, Snow_Icmp.Off);
      run_the_test snow_connection_to_quick;
      Snow_Icmp.close snow_connection_to_quick;
      Quick_Icmp.finalize ();
      Snow_Icmp.finalize ();
      ()
   end (* let *)

(*
        3.      run 
*)

  fun illegal_allocation returned = 
       (debug_print (fn _ => "an address_mask allocate_send call returned " ^
		     Snow_Icmp.makestring_outgoing (returned, NONE)))
 
  fun send_an_address_mask_request conn =
       let val allocation = Snow_Icmp.Out.Mask_Request {id = SW.n16 "0",
							sequence = SW.n16 "1"}
	   val (_, sender) = Snow_Icmp.allocate_send (conn, allocation)
       in sender ()
       end

  fun send_an_address_mask_reply conn =
       let val allocation = Snow_Icmp.Out.Mask_Reply
	                      {id = SW.n16 "0", sequence = SW.n16 "1",
			       address_mask = SW.n32 "2"}
	   val (_, sender) = Snow_Icmp.allocate_send (conn, allocation)
       in sender ()
       end

  fun run_tests () =
       let fun on_reply got_a_mask p = got_a_mask := true
	   fun get_no_mask (got_a_mask, name) snow_connection_to_quick =
                (send_an_address_mask_request snow_connection_to_quick;
		 B.Scheduler.sleep 10;
		 B.Test.test (name,fn () => not (! got_a_mask)))
	   fun get_a_mask (got_a_mask, name) snow_connection_to_quick =
	        (send_an_address_mask_request snow_connection_to_quick;
		 B.Scheduler.sleep 10;
		 B.Test.test (name, fn () => (! got_a_mask)))
	   fun serve_a_mask f connection  =
                (Quick_Icmp.serve_mask {interface = "SE0",
					mask = SW.n32 "0xFFFFFFFF"};
		 f connection)
	   fun send_an_address_mask f connection  =
                (send_an_address_mask_reply connection;
		 f connection)
	   val name1 = "request a mask while not serving"
	   val got1 = ref false
	   val name2 = "request a mask while serving"
	   val got2 = ref false
	   val name3 = "request a mask while serving with a mask"
	   val got3 = ref false
       in test_icmp_mask (name1, get_no_mask (got1, name1), on_reply got1);
	  test_icmp_mask (name2, serve_a_mask (get_a_mask (got2, name2)),
			  on_reply got2);
	  test_icmp_mask (name3,
			  send_an_address_mask (get_no_mask (got3, name3)),
			  on_reply got3)
       end

  fun run () =
       (B.Scheduler.reset ();
	init_stacks ();
	B.Test.tests ("Address Mask", 3, run_tests);
	fin_stacks ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

structure Test_Mask = Test_Icmp_Mask (structure B = Fox_Basis
				      val debug_level = NONE)
