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

	Low level DNS Protocol implementation


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Hostname
	2.	structure Dns_Test


		iii.	RCS Log
	
$Log: dnsproto.tst,v $
Revision 1.5  1997/02/13  00:44:30  esb
updated to avoid compiler warnings about non-generalizable variables.

Revision 1.4  1996/03/04  21:31:04  esb
adapted to new dnsproto.sig.

Revision 1.3  1996/02/14  20:23:10  esb
eliminated top-level local declaration.

Revision 1.2  1996/02/06  23:41:09  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.1  1996/01/16  22:00:13  cline
Initial revision

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision



	1.	functor Hostname
*)

functor Hostname(structure Dev: DEVICE_PROTOCOL
		 sharing type Dev.Setup.T = string) =
 struct

  exception Hostname_Lookup_Failed

  fun run_session (Dev.S {extension = Dev.Dev_Session_Extension
			              {local_address, ...}, ...}) =
       let infix ++
           fun op++ (x,y) = Word48.+ (Word48.<< (x,0w8), y)
           fun l i = (Word48.fromInt
	              (Word8.toInt
		         (Word_Array.W8.Native.F.nth
		            (Word_Array.to8 local_address, i))))
	             handle _ => raise Hostname_Lookup_Failed
           val local_address_48 =
                 l 0w0 ++ l 0w1 ++ l 0w2 ++ l 0w3 ++ l 0w4 ++ l 0w5
       in case (case Test_Addresses.eth_ip local_address_48 of
	         SOME a => Test_Addresses.ip_name a
	       | NONE => raise Hostname_Lookup_Failed) of
	   SOME name => name
         | NONE => raise Hostname_Lookup_Failed
       end

  fun hostname interface_name =
       Dev.session (interface_name, run_session)
 end

(*
	2.	structure Dns_Test
*)

structure Dns_Test =
 struct
  structure B = Fox_Basis

  structure Base = Build_Eth_Dev (structure B = B
				  val debug_level = NONE)

  local

   structure Stack = Build_Udp (structure Device = Base.Dev
				structure B = Fox_Basis
				val udp_over_ip = Word8.fromInt 17
				val eth_debug_level = NONE
				val arp_debug_level = NONE
				val ip_debug_level = NONE
				val icmp_debug_level = NONE
				val udp_debug_level = NONE)
  in
   structure Ip = Stack.Ip
   structure Udp = Stack.Udp
  end

  structure Hostname = Hostname (structure Dev = Base.Dev)

  structure Dns = Dns_Protocol(structure B = B
			       structure Lower = Udp
			       val dns_port = Word16.fromInt 53
			       val debug_level = SOME (ref 2))

  structure Dns_M = Dns.Message

  fun query_name (name, server) =
       let val interface_name = (*"tu0"*) "ln0"
	   exception Not_An_Ip_Address
	   val server_ip = case Ip.Host_Id.parse server of
	                      SOME ip => ip
			    | NONE => raise Not_An_Ip_Address
           val server_address = server_ip
	   val local_hostname = Hostname.hostname interface_name ^ ".foxnet"
	   val local_id = Test_Addresses.get_ip local_hostname
	   val gateway_id = Test_Addresses.get_ip "gw"
	   val setup = Ip.Network_Setup.Setup
	        [{local_id = local_id, interface = interface_name,
	          gateways = [gateway_id], mask = NONE, mtu = NONE}]
	   val query =
	        let val header = Dns_M.Header {query = true,
					       opcode = Dns_M.Query,
					       aa = false,
					       tc = false,
					       rd = false,
					       ra = false,
					       rcode = Dns_M.No_Error}
		  val question = Dns_M.Question {name = name,
						 rr_qtype = Dns_M.A_Q,
						 rr_class = Dns_M.IN}
		in Dns_M.Message {header = header,
				  question = [question],
				  answer = [],
				  authority = [],
				  additional = []}
		end

	fun handler data_pipe key =
	     let val done_pipe = B.Pipe.new (): Dns_M.message option B.Pipe.T
	         fun timeout () =
	              (B.Scheduler.sleep 5000;
		       B.Pipe.enqueue (done_pipe, NONE))
		 fun connection_handler (Dns.C {send, ...}) =
	              (send query;
		       B.Scheduler.fork timeout;
		       B.Pipe.enqueue (data_pipe, B.Pipe.dequeue done_pipe))
		 fun data_handler (_, packet) =
		      (print ("got a packet:\n" ^ Dns_M.makestring packet ^
			      "\n");
		       B.Pipe.enqueue (data_pipe, SOME packet))
		 fun status_handler _ = B.Pipe.enqueue (done_pipe, NONE)
	     in {connection_handler = connection_handler,
		 data_handler = data_handler,
		 status_handler = status_handler}
	     end

	fun run_session (Dns.S {connect, ...}) =
	      let val data_pipe = B.Pipe.new (): Dns_M.message option B.Pipe.T
	      in connect (server_address, Dns.H (handler data_pipe));
		 B.Pipe.dequeue data_pipe
	      end

       in Dns.session (setup, run_session)
       end
 end

