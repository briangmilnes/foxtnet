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



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Dns

		iii.	RCS Log
	
$Log: dns.tst,v $
Revision 1.1  1995/06/20  17:24:23  esb
Initial revision

Revision 1.20  1995/03/12  17:56:18  esb
adapted to new trace.sig.

Revision 1.19  1995/03/10  03:50:54  esb
adapted to new vendor.sig.

Revision 1.18  1995/02/21  13:18:09  esb
upgraded for SML/NJ 1.07.

Revision 1.17  1995/02/04  20:40:19  robby
updated to 107

Revision 1.16  1995/01/19  22:53:45  esb
updated the address of gopher.contrib.andrew.cmu.edu.

Revision 1.15  1995/01/17  23:00:30  esb
removed the automatic run, since this is a "live network" test.

Revision 1.14  1995/01/15  18:54:14  robby
updated it to use Tcp_Ip_Eth

Revision 1.13  1994/10/28  17:42:18  milnes
Added 105/93 compatibility.

Revision 1.12  1994/10/18  10:22:11  robby
fixed a typo.

Revision 1.11  1994/10/07  18:57:09  milnes
 Added tests for hostname.

Revision 1.10  1994/09/23  16:54:05  milnes
Upgraded tests to try and talk to andrew and get back one of a zillion
hosts for balancing its macmail.

Revision 1.9  1994/09/12  18:24:18  milnes
Changed with regards to initial authorities.

Revision 1.8  1994/08/28  21:45:14  milnes
 Did nothing but rcs insists I check it in again.

Revision 1.7  1994/08/23  17:16:59  milnes
 Added a test for a local host using a non-canonical name.

Revision 1.6  1994/07/19  20:24:34  milnes
Papya_ip to papaya_ip

Revision 1.5  1994/07/13  16:54:40  milnes
Updated to allow ping to use dns.

Revision 1.4  1994/07/06  17:49:47  esb
removed the automatic execution.

Revision 1.3  1994/07/04  22:19:40  esb
changed so functor instantiation no longer tries to run the test.

Revision 1.2  1994/07/04  21:36:38  esb
adapted to Copy/Create split.

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision


		1.	functor Test_Dns

*)

functor Test_Dns (structure Tcp_Ip_Eth: TCP_IP_ETH
                  structure B: FOX_BASIS): TEST_STRUCTURE  =
 struct
  structure Trace = Trace (structure V = B.V
                           val debug_level = NONE
			   val module_name = "dns.tst")
  val local_print = Trace.local_print

  structure Dns = Tcp_Ip_Eth.Dns

  val n4u18 = FoxWord32.intToWord 18  
  val n4u128 = FoxWord32.intToWord 128  
  val n4u140 = FoxWord32.intToWord 140

  infix %
  fun (a%b) = FoxWord32.orb (FoxWord32.lshift (a, 8), FoxWord32.intToWord b)

  fun test_address_query (hostname, desired) () =
       let val (dns, result) = 
	        Dns.address_query (Dns.new (Dns.default_authorities ()),
				   hostname)
       in if result = desired then true
	  else
	   (local_print ("bug found " ^
		       (case result of
			   NONE => "NONE "
			 | SOME s =>
			    ("SOME ["^
			     B.V.String.concat
			     (map (fn ip => (makestring ip)^" ") s) ^ "] ")));
	    false)
       end

 (* Andrew does some load balancing using DNS, so this test checks that
    the answer is in the list. *)
    
  fun test_address_query_list (hostname, desired) () =
  let
   val (dns, result) = 
     Dns.address_query (Dns.new (Dns.default_authorities()),hostname)

   fun report () =
    (local_print ("bug found " ^
                 (case result of
		   NONE => "NONE "
		 | SOME s =>
		    ("SOME [" ^
		     B.V.String.concat (map (fn ip => (makestring ip) ^ " ") s)^
		     "] ")));
    false)

   fun member (x,[]) = false
     | member (x,y :: rest) = if x = y then true else member (x,rest)
  in 
   case result of 
     NONE => report()
   | SOME [s] =>
      if member(s,desired) then true 
      else report()
   | _ => report() 
  end

  fun test_authority_query (domain, desired) () =
  let
   val dns_query = (Dns.new(Dns.default_authorities()), domain)
   val (dns, result) = Dns.authority_query dns_query
  in
   if result = desired then true
   else
    (local_print ("bug found " ^
      (case result of
        NONE => "NONE "
      | SOME s => ("SOME [" ^ (B.V.String.concat s) ^ "] ")));
    false)
  end

 fun test_name_query (ip, desired) () =
  let
   val (dns, result) = 
     Dns.name_query (Dns.new (Dns.default_authorities()),ip)
  in 
   if result = desired then true
   else
     (local_print ("bug found " ^
		   (case result of
		       NONE => "NONE"
		     | SOME s => ("SOME [" ^ (B.V.String.concat s) ^ "] ")));
      false)
  end


 fun run_tests () =
      (Tcp_Ip_Eth.initialize();
       B.Test.test ("test_address_query sly.fox.cs.cmu.edu",
          (test_address_query ("SLY.FOX.CS.CMU.EDU",SOME([n4u128%2%222%15]))));
       B.Test.test ("test name query 128.2.222.15 (sly)",
          (test_name_query (n4u128%2%222%15, SOME["SLY.FOX.CS.CMU.EDU"])));
       B.Test.test ("test name query 18.26.0.36 (lcs.mit.edu)",
          (test_name_query (n4u18%26%0%36, SOME["MINTAKA.LCS.MIT.EDU"])));
       (* A canonical name query. *)
       B.Test.test ("test_address_query nebula.cs.yale.edu",
          (test_address_query ("NEBULA.CS.YALE.EDU",
			       SOME ([n4u128%36%13%1]))));
       B.Test.test ("test_address_query gopher.contrib.andrew.cmu.edu",
          (test_address_query ("GOPHER.CONTRIB.ANDREW.CMU.EDU",
			       SOME [n4u128%2%35%31])));
       B.Test.test 
       ("test_address_query macmail.andrew.cmu.edu, may fail",
          (test_address_query_list ("MACMAIL.ANDREW.CMU.EDU",
           [n4u128%2%11%71,n4u128%2%15%253,n4u128%2%11%77,n4u128%2%232%62,
	    n4u128%2%11%74,n4u128%2%11%98,n4u128%2%35%86,n4u128%2%35%87,
	    n4u128%2%35%88,n4u128%2%35%89,n4u128%2%35%90,n4u128%2%232%61,
	    n4u128%2%222%199,n4u128%2%232%72,n4u128%2%222%199, n4u128%2%232%71,
	    n4u128%2%222%199,n4u128%2%232%59,n4u128%2%222%199, n4u128%2%232%70,
	    n4u128%2%222%199,n4u128%2%232%69,n4u128%2%222%199, n4u128%2%232%68,
	    n4u128%2%222%199,n4u128%2%232%67,n4u128%2%222%199, n4u128%2%232%66,
	    n4u128%2%222%199,n4u128%2%232%65,n4u128%2%222%199, n4u128%2%232%64,
	    n4u128%2%222%199,n4u128%2%232%63,n4u128%2%232%73, n4u128%2%232%155,
	    n4u128%2%232%79])));
       B.Test.test ("test_address_query lcs.mit.edu",
          (test_address_query ("LCS.MIT.EDU", SOME ([n4u18%26%0%36]))));
       B.Test.test ("test_address_query FCUSQNT.FCU.EDU.TW",
          (test_address_query ("FCUSQNT.FCU.EDU.TW", SOME [n4u140%134%12%1])));
       B.Test.test ("test_authority_query FCUSQNT.FCU.EDU.TW",
          (test_authority_query ("FCU.EDU.TW", SOME ["FCUMS1.FCU.EDU.TW"])));
       Tcp_Ip_Eth.finalize ();
       ()) handle x => (Dns.finalize (); raise x)

  fun run () =
       B.Test.tests ("dns.tst",9,run_tests)

 end

structure Test_Dns_Udp =
  Test_Dns (structure Tcp_Ip_Eth = Tcp_Ip_Eth
            structure B = Fox_Basis)

