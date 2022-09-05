(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	arpclient.tst: test of the client (request) side of arp

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Arp_Real
	2.	internal structures Eth_Dev, Eth
	3.	internal structure ARP
	4.	ethernet and IP addresses
	5.	internal function init
	6.	internal function final_count
	7.	internal function check_remote
	8.	internal function run_tests
	9.	function run
	10.	structure Arp_Real

---------------------------------------------------------------------
	iii.	RCS Log

$Log: arpreal.tst,v $
Revision 1.21  1995/03/12  17:54:12  esb
adapted to new trace.sig.

Revision 1.20  1995/03/07  23:51:23  esb
updated tracing.

Revision 1.19  1995/02/21  15:46:03  esb
adapted to new eth.sig with 64-bit packet counters.

Revision 1.18  1995/02/09  19:50:29  esb
made work with sml/nj 1.07

Revision 1.17  1995/01/18  21:06:03  esb
added a scheduler reset before the test.

Revision 1.16  1995/01/17  22:49:38  esb
adapted to new dev.sig.

Revision 1.15  1995/01/06  17:04:22  esb
cleaned up so it works on every machine.

Revision 1.14  1994/07/01  02:35:50  danwang
Moved control structures into Fox_Basis.

Revision 1.13  1994/06/16  21:53:14  danwang
Updated to use functorized Fox_Basis.

Revision 1.12  1993/11/11  05:04:44  esb
functor parameter changes.

Revision 1.11  1993/10/26  21:29:12  esb
restructured code to avoid having to be rootl when loading the file.

Revision 1.10  1993/10/25  19:36:53  cline
removed .U from Byte[421].U

Revision 1.9  1993/10/25  15:09:48  esb
functor parameters.

Revision 1.8  93/10/14  22:37:15  esb
adapted to new Ethernet_Device functor parameters.

Revision 1.7  1993/10/14  18:26:46  milnes
Used implicit sequencing in let bodies.

Revision 1.6  1993/10/09  23:21:33  esb
removed automatic (on loading) execution of the test.

Revision 1.5  1993/10/09  17:49:36  esb
consolidated the protocol state; we now use the new store interface.

Revision 1.4  1993/09/13  22:07:48  cline
deleted '#'s from RCS log

Revision 1.3  1993/09/02  19:55:40  esb
adapted to new PROTOCOL signature.

Revision 1.2  93/09/02  16:00:42  esb
replaced test functor by test structure.

Revision 1.1  93/08/24  21:20:29  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	functor Arp_Real
*)

functor Arp_Real (structure B: FOX_BASIS
		  val debug_level: int ref option): TEST_STRUCTURE =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "arpreal.tst")
  val local_print = Trace.local_print

(*
---------------------------------------------------------------------
	2.	internal structures Eth_Dev, Eth
*)

  val arp_protocol = SW.n16 "0x806"
  val ip_protocol = SW.n16 "0x800"

  structure Eth_Dev_Build = Build_Eth_Dev (structure B = B
					   val high_priority_filter = true
					   val debug_level = debug_level)

  structure Eth_Dev = Eth_Dev_Build.Eth_Dev

  structure Eth = Ethernet (structure Dev = Eth_Dev
			    structure B = B
			    val debug_level = debug_level)

(*
---------------------------------------------------------------------
	3.	internal structure ARP
*)

  structure Arp = Arp_Eth (structure Eth = Eth
			   val arp_protocol_number = arp_protocol
			   structure B = B
			   val debug_level = debug_level)

(*
---------------------------------------------------------------------
	4.	ethernet and IP addresses
*)

  fun make_ip ip =
       let val result = ByteArray.array (4, 0)
       in FoxWord32.update (result, 0, B.Order.B4.to_big ip);
          result
       end

  val quick_ip = make_ip (Test_Addresses.get_ip "quick")
  val snow_ip = make_ip (Test_Addresses.get_ip "snow")
  val sly_ip = make_ip (Test_Addresses.get_ip "sly")
  val cobol_ip = make_ip (Test_Addresses.get_ip "cobol")

  val unknown_ip = make_ip (SW.n32 "0")

(*
---------------------------------------------------------------------
	5.	internal function init
*)

  local
   fun address_count () =
        (Eth.initialize ();
         let val local_address = Eth.local_address ()
	     fun packets_received () =
	          (Eth.initialize ();
		   Eth.packets_received ()
		   before Eth.finalize ())
         in (local_address, packets_received)
	    before Eth.finalize ()
         end)

   fun eth_makestring {a0, a1, a2, a3, a4, a5} =
	FoxMakestring.word8 a0 ^ ":" ^ FoxMakestring.word8 a1 ^ ":" ^
	FoxMakestring.word8 a2 ^ ":" ^ FoxMakestring.word8 a3 ^ ":" ^
	FoxMakestring.word8 a4 ^ ":" ^ FoxMakestring.word8 a5 ^ ":"

   fun ip_makestring ip =
	makestring (FoxWord32.wordToInt (FoxWord32.rshift (ip, 24))) ^ "." ^
	makestring (FoxWord32.wordToInt (FoxWord32.andb
					 (FoxWord32.rshift (ip, 16),
					  SW.n32 "0xff"))) ^
	"." ^
	makestring (FoxWord32.wordToInt (FoxWord32.andb
					 (FoxWord32.rshift (ip, 8),
					  SW.n32 "0xff"))) ^
	"." ^
	makestring (FoxWord32.wordToInt (FoxWord32.andb (ip, SW.n32 "0xff")))

   fun name_ip_higher local_address =
        case Test_Addresses.eth_ip local_address of
	   NONE =>
	    (local_print ("test_addresses does not have local_address " ^
			  eth_makestring local_address);
	     ("unknown", unknown_ip))
	 | SOME ip =>
	    (case Test_Addresses.ip_name ip of
	        NONE =>
		 (local_print ("test_addresses does not have name for IP " ^
			       ip_makestring ip);
		  ("unknown", make_ip ip))
	      | SOME name =>
		 (name, make_ip ip))

  in

   fun init () =
        let val (eth, init_count) = address_count ()
	    val (a0, a1, a2, a3, a4, a5) = eth
	    val local_eth = {a0 = a0, a1 = a1, a2 = a2,
			     a3 = a3, a4 = a4, a5 = a5}
            val (name, ip) = name_ip_higher local_eth
        in (name, init_count, ip, eth)
        end

  end (* local *)

(*
---------------------------------------------------------------------
	6.	internal function final_count
*)

  fun final_count (count_fun, init_count) =
       let val current = count_fun ()
	   val delta = FoxWord64.- (current, init_count)
       in local_print ("initial packets " ^ FoxMakestring.word64 init_count ^
		       ", final packets " ^ FoxMakestring.word64 current ^
		       ", received " ^ FoxMakestring.word64 delta)
       end

(*
---------------------------------------------------------------------
	7.	internal function check_remote
*)

  fun check_remote (name, local_ip, remote_ip) () =
       (let fun receive connection p = local_print "received data"
	    fun status _ = local_print "received status"
	    fun handler c = (receive c, status)
	    val address = Arp.Connect {self = local_ip, peer = remote_ip,
				       protocol = ip_protocol}
	    val conn = Arp.connect (address, Arp.Handler handler)
	in Arp.close conn;
	   true
	end)
	 handle _ => false

(*
---------------------------------------------------------------------
	8.	internal function run_tests
*)

  fun run_tests () =
       let val _ = B.Scheduler.reset ()
	   val (local_name, count, local_ip, local_address) = init ()
	   val init_count = count ()
       in Arp.initialize ();
          if local_name <> "quick" then
           B.Test.test ("quick", check_remote ("quick", local_ip, quick_ip))
          else ();
          if local_name <> "snow" then
           B.Test.test ("snow", check_remote ("snow", local_ip, snow_ip))
          else ();
          if local_name <> "sly" then
           B.Test.test ("sly", check_remote ("sly", local_ip, sly_ip))
          else ();
          if local_name <> "cobol" andalso
	     (local_name = "sly" orelse local_name = "snow" orelse
	      local_name = "quick") then
           B.Test.test ("cobol", check_remote ("cobol", local_ip, cobol_ip))
          else ();
          final_count (count, init_count);
          Arp.finalize ();
          B.Test.test ("final", fn _ => true)
       end

(*
---------------------------------------------------------------------
	9.	function run
*)

  fun run () = B.Test.tests ("ArpReal", 4, run_tests)

 end (* struct *)

(*
---------------------------------------------------------------------
	10.	structure Arp_Real

	Note: do_prints = true is likely to make the test fail, since
	the slowdown caused by printing will cause the receiver to
	miss the reply.

*)

structure Arp_Real = Arp_Real (structure B = Fox_Basis
			       val debug_level = NONE)

