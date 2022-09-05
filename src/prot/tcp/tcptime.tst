(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature HOSTNAME
	2.	functor HOSTNAME
	3.	signature GET_TIME
	4.	functor Get_Time
	5.	structure Get_Time

		iii.	RCS Log
	
$Log: tcptime.tst,v $
Revision 1.1  1995/09/26  14:02:59  cline
Initial revision


*)

(*
		1.	signature HOSTNAME
*)
signature HOSTNAME = sig val hostname: unit -> string end

(*
		2.	functor HOSTNAME
*)
functor Hostname(structure Dev: DEVICE_PROTOCOL
		 sharing type Dev.Setup.T = unit): HOSTNAME =
struct

  exception Hostname_Lookup_Failed

  fun run_session (Dev.S {extension={local_address, ...}, ...}) =
    let
      infix ++
      fun op++ (x,y) = FoxWord48.+ (FoxWord48.shift (x,8), y)
      fun l i = (FoxWord48.intToWord
	           (FoxWord8.wordToInt
		      (Word_Array.W8.nth (local_address, i))))
	        handle _ => raise Hostname_Lookup_Failed
      val local_address_48 = l 0 ++ l 1 ++ l 2 ++ l 3 ++ l 4 ++ l 5
    in
      case (case Test_Addresses.eth_ip local_address_48 of
	      SOME a => Test_Addresses.ip_name a
	    | NONE => raise Hostname_Lookup_Failed) of
	SOME name => name
      | NONE => raise Hostname_Lookup_Failed
    end

  fun hostname () = Dev.session ((), run_session)
end

(*
		3.	signature GET_TIME
*)
signature GET_TIME =
sig
  val get_time: string -> string
end

(*
		4.	functor Get_Time
*)
functor Tcp_Time(val debug_level: int ref option
		 structure B:FOX_BASIS): GET_TIME =
struct

  val time_port = SW.n16 "13"

  local
    val tcp_protocol = SW.n8 "6"
    val port1 = SW.n16 "0x1111"
    val port2 = SW.n16 "0x2222"
    val window_size = 4096
    val user_timeout = 5000

    structure Base = Build_Eth_Dev (structure B = B
				    val high_priority_filter = true
				    val debug_level = debug_level)

    structure Stack = Build_Tcp (structure Device = Base.Dev
				 structure B = Fox_Basis
				 val initial_window_size = window_size
				 val user_timeout = user_timeout
				 val compute_checksums = true
				 val tcp_over_ip = tcp_protocol
				 val eth_debug_level = debug_level
				 val arp_debug_level = debug_level
				 val ip_debug_level = debug_level
				 val icmp_debug_level = debug_level
				 val tcp_debug_level = debug_level)
  in
    structure Ip = Stack.Ip
    structure Tcp = Stack.Tcp

    structure Hostname = Hostname (structure Dev = Base.Dev)
  end (* local *)

  fun packet_to_string p =
    let
      fun list_char (w,l) = (Char.chr (FoxWord8.wordToInt w))::l
      fun array_to_string a =
	String.implode (Word_Array.W8.Rev.fold list_char [] a)
    in
      array_to_string
        (Tcp.Incoming.sub (p, {start=0, length=Tcp.Incoming.size p}))
    end

  fun handler data_pipe key =
    let
      val done_pipe = B.Pipe.new ()
      fun connection_handler _ = B.Pipe.dequeue done_pipe
      fun data_handler (_, packet) = B.Pipe.enqueue (data_pipe,
						     packet_to_string packet)
      fun status_handler _ = B.Pipe.enqueue (done_pipe, ())
    in
      {connection_handler = connection_handler,
       data_handler = data_handler,
       status_handler = status_handler}
    end

  fun run_session peer (Tcp.S {connect, ...}) =
    let
      val data_pipe = B.Pipe.new ()
    in
      connect (peer, Tcp.H (handler data_pipe));
      B.Pipe.dequeue data_pipe
    end

  fun get_time remote_host =
    let
      val local_hostname = Hostname.hostname () ^ ".foxnet"
      val local_id = Test_Addresses.get_ip local_hostname
      val peer_id = Test_Addresses.get_ip remote_host
      val peer = Tcp.Transport_Address.Remote_Specified
	           {peer=peer_id, remote_port=time_port}
      val setup = Ip.Network_Setup.Setup
			[{local_id = local_id, interface = "SE0",
			  gateways = [], mask = NONE, mtu = NONE}]
    in
      print ("Starting session on host " ^ local_hostname ^ "\n");
      Tcp.session (setup, run_session peer)
    end

end (* struct *)

(*
		5.	structure Get_Time
*)
structure Tcp_Time = Tcp_Time (structure B = Fox_Basis
			       val debug_level = NONE);
