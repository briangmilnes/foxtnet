(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	Test code for SUN RPC server

	ii.	Table of Contents

	1.      functor Test_sunrpc_server

	iii.	RCS Log

$Log: sunrpc_server.tst,v $
Revision 1.2  1994/10/24  23:11:00  kcchiang
remove extraneous arguments to Build_SunRPC_UDP

Revision 1.1  94/10/14  12:00:00  kcchiang
Initial revision

*)

(*
        1.      functor Test_sunrpc_server
*)
functor Test_sunrpc_server (structure B : FOX_BASIS): TEST_STRUCTURE =
struct
  val ip_over_eth = 2ux800
  val arp_over_eth = 2ux806
  val udp_over_ip = 1u17
  val udp_header_size = 8
  val udp_server_port = 2ux5742
      
  structure Sim = Build_Simulators (structure B = B
				    val do_prints = false)
  structure Xvoid = XVoid ()
  structure Xint = XInt ()


  (* Server setup 
     ============ *)
  local
    structure SunRPC_UDP = 
	Build_SunRPC_UDP(structure B                  = B
			 structure Device             = Sim.Quick
			 val local_port               = udp_server_port
			 val ip_over_eth              = ip_over_eth
			 val arp_over_eth             = arp_over_eth
			 val do_prints                = false
			 val use_arp                  = false)

    structure wrap = Wrap (structure Arg = Xint
			   structure Res = Xint)
  in
    val wrap = wrap.externalize
    structure S = SunRPC_server(structure B = B
				structure SunRPC_prot = SunRPC_UDP
				val do_print = false
				val local_interface = "SE0"
				val local_ip = Test_Addresses.quick_ip)
  end


  (* RPC remote functions
     ==================== *)
  structure Prog =
  struct
   val prog_id = 811

   structure Fact =
   struct
       fun fact 0 = 1
	 | fact n = if n < 0 then 0
		    else n * (fact (n-1))
       val ver = 1
       val id = 1
   end

   structure Add1 =
   struct
       fun add1 a = a + 1
       val ver = 1
       val id = 2
   end

   structure Mul2 =
   struct
       fun mul2 a = a * 2
       val ver = 1
       val id = 3
   end
  end


   (* Install remote functions on server
      ================================== *)
  fun server_install () =
      (S.install (Prog.prog_id,Prog.Fact.ver,Prog.Fact.id) (wrap Prog.Fact.fact);
       S.install (Prog.prog_id,Prog.Add1.ver,Prog.Add1.id) (wrap Prog.Add1.add1);
       S.install (Prog.prog_id,Prog.Mul2.ver,Prog.Mul2.id) (wrap Prog.Mul2.mul2);
       ())



  (* Client setup
     ============ *)
  structure XSun = SunRPC (structure Remote_Proc_Params = Xint
			   structure Remote_Proc_Result = Xint)

  local
    val udp_client_port = 2ux5742
    val use_arp = false
    val do_prints = false

    structure Udp_Stack = BuildUdp(structure Device         = Sim.Snow
				   structure B              = B
				   val ip_over_eth          = ip_over_eth
				   val udp_over_ip          = udp_over_ip
				   val use_arp              = use_arp
				   val do_prints            = do_prints)

    structure Prot = Udp_Stack.Udp
    structure Ip = Udp_Stack.Ip

  in
      val AuthFlavor = XSun.auth_flavor.AUTH_NULL
      val Null_Auth = (AuthFlavor, ByteArray.array (0,0))
      val RPCresult = ref (0,XSun.Body.T1(0,0,0,0,Null_Auth,Null_Auth,0));
      val RPCver = 2

      fun ResExtract rpc_msg = 
      let exception ERR_IN_RPC_RESULT
      in
	  case rpc_msg of
	      (c,XSun.Body.T2
	       (XSun.ReplyBody.T1 (_,XSun.ReplyData.T1 res)))
	      => (c,res)
	    | _ => raise ERR_IN_RPC_RESULT
      end
  
      structure C = 
      struct
	fun initialize () = 
	    (Prot.initialize ();
	     let val local_ip = Test_Addresses.snow_ip
	         val local_interface = "SE0"
	     in
	       Ip.control 
	       (Ip.Set_Interface_Address {interface=local_interface,
						     ip_number=local_ip});
	       ()
	     end)

	fun finalize () = Prot.finalize ()

	fun handler queue connection incoming =
        let val rpc_res = B.Receive_Packet.read incoming
	    val (rpc_res,_) = XSun.unmarshall (rpc_res,0)
	in
	    B.V.print "Client received result...\n";
	    Prot.close connection;
	    B.Pipe.enqueue (queue, ResExtract rpc_res);
	    ()
	end

	fun send (queue, rpc_msg) =
        let val addr = 
	      Prot.Address 
	      {remote_peer = Ip.Address
	       {proto=udp_over_ip, ip=Test_Addresses.quick_ip},
	       local_port = udp_client_port,
	       remote_port= udp_server_port }
	    val connection = Prot.active_open (addr, handler queue)
	    val (outgoing_data,_) = 
		XSun.marshall (rpc_msg,(ByteArray.array (XSun.size rpc_msg,0),0))
	    val outgoing_packet =
		B.Send_Packet.create (outgoing_data,udp_header_size)
	in
	  B.V.print "Client sending RPC msg...";
	  Prot.send connection outgoing_packet;
	  B.V.print " done\n"
	end
      end

  end (* local *)



  (* tests utilities
     =============== *)
  fun FactArgGen c x = (c,XSun.Body.T1 
			(RPCver,Prog.prog_id,Prog.Fact.ver,Prog.Fact.id,Null_Auth,
			 Null_Auth,x))
  fun Add1ArgGen c x = (c,XSun.Body.T1 
			(RPCver,Prog.prog_id,Prog.Add1.ver,Prog.Add1.id,Null_Auth,
			 Null_Auth,x))
  fun Mul2ArgGen c x = (c,XSun.Body.T1 
			(RPCver,Prog.prog_id,Prog.Mul2.ver,Prog.Mul2.id,Null_Auth,
			 Null_Auth,x))


  (* tests
     ===== *)
  fun fact_test queue () = 
      (C.send (queue, FactArgGen 1 3);
       (B.Pipe.dequeue queue) = (1, 6)) andalso
      (C.send (queue, FactArgGen 9 4);
       (B.Pipe.dequeue queue) = (9, 24)) andalso
      (C.send (queue, FactArgGen 100 6);
       (B.Pipe.dequeue queue) = (100, 720))

  fun add1_test queue () =
      (C.send (queue, Add1ArgGen 2 3);
       (B.Pipe.dequeue queue) = (2, 4)) andalso
      (C.send (queue, Add1ArgGen 97 88);
       (B.Pipe.dequeue queue) = (97, 89)) andalso
      (C.send (queue, Add1ArgGen 100 6);
       (B.Pipe.dequeue queue) = (100, 7))

  fun mul2_test queue () =
      (C.send (queue, Mul2ArgGen 2 10);
       (B.Pipe.dequeue queue) = (2, 20)) andalso
      (C.send (queue, Mul2ArgGen 97 88);
       (B.Pipe.dequeue queue) = (97, 176)) andalso
      (C.send (queue, Mul2ArgGen 100 6);
       (B.Pipe.dequeue queue) = (100, 12))


  fun tests () =
  let val q = B.Pipe.new NONE
  in
       server_install ();
       C.initialize ();
       S.initialize ();
       B.Test.test "fact_test" (fact_test q);
       B.Test.test "add1_test" (add1_test q);
       B.Test.test "mul2_test" (mul2_test q);
       C.finalize ();
       S.finalize ();
       ()
  end

  fun run () =
      (B.Test.tests "SunRPC server" 3 tests;
       ())
end


structure Test_sunrpc_server = Test_sunrpc_server (structure B = Fox_Basis)
