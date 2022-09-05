(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robby Findler (Robert.Findler@cs.cmu.edu)
	Daniel Wang (Daniel.Wang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor zwrite

		iii.	RCS Log
	
$Log: zwrite.fun,v $
Revision 1.4  1994/09/03  21:06:23  danwang
Fixed authentication.

Revision 1.4  1994/09/03  21:03:51  danwang
Fixed authentication.

Revision 1.3  1994/07/17  00:42:06  robby
started to add authentication

Revision 1.1  94/07/14  16:27:24  milnes
Initial revision

		1.	functor zwrite
*)

functor Zwrite (structure B : FOX_BASIS
                structure Ip : IP_PROTOCOL
                structure Udp : UDP_PROTOCOL
		structure K : KERBEROS
		structure Z : ZEPHYR_EXTERN
                sharing type Udp.outgoing_message = B.Send_Packet.T
		    and type Udp.incoming_message = B.Receive_Packet.T
		    and type Ip.ip_address = Udp.lower_layer_address
                val udp_over_ip : ubyte1
                val zephyr_hm_port : ubyte2
		val my_ip : ubyte4
		val my_port : ubyte2
		val my_realm : string
                val do_prints: bool) : ZWRITE =
struct

    val debug_print=if do_prints then fn x =>
	(B.V.print("zwrite.fun: " ^ x ^"\n"); B.V.flush ())
		    else fn _=>() 
			
    fun build_unique_id () =
	let
	    val B.V.Misc.TIME {sec, usec} = B.V.Misc.gettimeofday ()
	in
	    {ip_address=my_ip,
	     time_sec=Byte4.from_int sec,
	     time_usec=Byte4.from_int usec}
	end
    
    fun build_packet (to,from,message) =
	let
	    val id = build_unique_packet_id();
	    val credential = SOME (K.get_credential 
				   (K.Service {name_s="zephyr",
					       instance_s="zephyr",
					       realm_s=my_realm}))
		handle (K.Kerberos_Error _) => NONE
	    val authenticator =
		case credential of
		    NONE => ByteArray.array(0,0)
		  | (SOME credential) =>
			(K.authenticator_to_bytearray 
			 (K.build_authenticator (credential,4u0)))
	    val authent_len = Byte4.from_int(ByteArray.length
					     authenticator)
	    val _ = debug_print("built authenticator")	    
	    val zrec = (Z.Zrec {version= "ZEPH0.2",
				num_fields=4u17,
				kind=4u2,
				packet_id=id,
				port=my_port,
				auth=4u1,
				authent_len=authent_len,
				authenticator=authenticator,
				class="MESSAGE",
				instance="PERSONAL",
				opcode="",
				sender=from ^ "@" ^ my_realm,
				recipent=to  ^ "@" ^ my_realm,
				default_format=
				"Class $class, Instance $instance:\n" ^
				"To: @bold($recipient)\n" ^
				"@bold($1) <$sender>\n\n$2",
				checksum=4u0,
				frag_count = "0/20",
				notice_id = id,
				data	= [from,message]})
	    val _ = debug_print("zwrite.fun: calculating packet size\n")
	    val size = Z.size zrec
	    val _ = debug_print("zwrite.fun: packet size is"
				^(makestring size) ^ "\n")
	    val array = Z.marshall(zrec,ByteArray.array(size,0))
	in
	    array
	end

  val header_bytes = 50
 (* An overestimate of the number of header bytes required in the
  zephyr udp packets. *)
  exception Quit

  fun zwrite ({to, from, message}) = 
    (Udp.initialize ();
     let 
	 val zhm_addr =
	     Udp.Address {remote_peer =
			  Ip.Address {ip=my_ip, proto=udp_over_ip},
			  local_port  = my_port,
			  remote_port = zephyr_hm_port}
	 val conn = Udp.active_open(zhm_addr,(fn _ => (fn _ =>())))
     in 
	 Udp.send conn
	 (B.Send_Packet.create ((build_packet (to,from,message)),
				header_bytes));

	 B.Scheduler.sleep (1000);

	 (* in case the message comes back  to us. *)
	 Udp.close conn
     end;
     Udp.finalize ();
     ()) handle x => (Udp.finalize (); raise x)
    
end
