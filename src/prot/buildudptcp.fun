(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	...

		iii.	RCS Log
	
$Log: buildudptcp.fun,v $
Revision 1.6  1995/02/04  20:39:19  robby
updated to 107

Revision 1.5  1994/11/22  13:58:38  milnes
Removed addressing functor arguments.

Revision 1.4  1994/11/10  16:11:41  milnes
Updated for tcpipeth/addressing and debug_trace structure.

Revision 1.3  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.2  1994/08/29  21:59:01  robby
had a name wrong the first time 'round.

Revision 1.1  1994/08/29  16:58:25  robby
Initial revision


		1.	...
*)
functor Build_Udp_Tcp(structure Device:DEVICE_PROTOCOL
		      structure B:FOX_BASIS
		      sharing type Device.incoming=B.Dyn_Array.T
			  and type Device.outgoing=B.Dyn_Array.T
		      val ip_over_eth:FoxWord16.word
		      val tcp_over_ip:FoxWord8.word
		      val udp_over_ip:FoxWord8.word
		      val tcp_initial_window_size:int
		      val user_timeout:int
		      ):BUILD_UDP_TCP=
struct
  type ubyte1 = FoxWord8.word
  type ubyte2 = FoxWord16.word
  type ubyte4 = FoxWord32.word

  structure Byte1 = FoxWord8
  structure Byte2 = FoxWord16
  structure Byte4 = FoxWord32

  local
   structure lower = Build_Ip (structure Device = Device
			       structure B = B
			       val ip_protocol = ip_over_eth)
  in open lower
  end

  local
   fun ip_equal (a, b: Ip.ip_number) = a = b
   fun ip_checksum ip =
        let val buffer = B.Create.create 4
	in Byte4.update (buffer, 0, B.Order.B4.to_big ip);
	   B.Checksum.checksum (buffer, 0, 4)
	end

   val protocol_checksum = Byte2.intToWord (Byte1.wordToInt tcp_over_ip)

  in
   structure Udp = Udp (structure Lower = Ip
			structure B = B
			val ip_equal = ip_equal
			val ip_checksum = ip_checksum
			val udp_protocol = udp_over_ip
			val protocol_checksum = protocol_checksum
			val compute_checksums = false)

   structure Tcp = Tcp (structure Lower = Ip
			structure B = B
			val ip_equal = ip_equal
			val ip_checksum = ip_checksum
			val tcp_protocol = tcp_over_ip
			val protocol_checksum = protocol_checksum
			val initial_window = tcp_initial_window_size
			val compute_checksums = true
			val abort_unknown_connections = true
			val user_timeout = user_timeout
                        val do_prints = true)
  end
end
