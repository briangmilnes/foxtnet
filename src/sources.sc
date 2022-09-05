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

	The build file for the FoxNet using SC

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	the basis
	2.	prot
	3.	prot/addressing
	4.	prot/arp
	5.	prot/dev
	6.	prot/eth
	7.	prot/icmp
	8.	prot/ip
	9.	prot/rpc
	10.	prot/tcp
	11.	prot/udp

		iii.	RCS Log
	
$Log: sources.sc,v $
Revision 1.12  1995/03/10  03:54:33  esb
added benchmarking, but commented out since it's not working yet.

Revision 1.11  1995/03/07  23:59:49  esb
updated for new addressing structure.

Revision 1.10  1995/02/21  13:31:48  esb
now works under 1.07.

Revision 1.9  1995/02/13  22:55:06  esb
fixed so the basis is loaded as needed.

Revision 1.8  1995/02/04  20:38:56  robby
updated to 107

		1.	the basis
*)

basis.sc

(*
		2.	prot
*)

prot/buildtcpipeth.fun
prot/conn.fun
prot/conn.sig
prot/demo.str
prot/proto.sig
prot/simple.fun
prot/tcpipeth.sig
prot/tcpipeth.str
prot/testaddress.str

(*
		3.	prot/addressing
*)

prot/addressing/addressing.fun
prot/addressing/addressing.sig
prot/addressing/osaddressing.sig
prot/addressing/osfaddressing.fun
prot/addressing/machaddressing.fun
prot/addressing/@sys/osaddressing.fun

(*
		4.	prot/arp
*)

prot/arp/arp.fun
prot/arp/arp.sig
prot/arp/arpeth.fun

(*
		5.	prot/dev
*)

prot/dev/@sys/sources.sc
prot/dev/@sys/ethdev.fun
prot/dev/@sys/buildethdev.fun
prot/dev/buildethdev.sig
prot/dev/buildsim.fun
prot/dev/buildsim.sig
prot/dev/dev.sig
prot/dev/sim.fun
prot/dev/wire.fun
prot/dev/wire.sig

(*
		6.	prot/eth
*)

prot/eth/buildeth.fun
prot/eth/buildeth.sig
prot/eth/eth.fun
prot/eth/eth.sig

(*
		7.	prot/icmp
*)

prot/icmp/buildicmp.fun
prot/icmp/buildipicmp.fun
prot/icmp/buildipicmp.sig
prot/icmp/icmp.fun
prot/icmp/ipicmp.fun

(*
		8.	prot/ip
*)

prot/ip/buildip.fun
prot/ip/buildip.sig
prot/ip/emptyicmp.fun
prot/ip/icmp.sig
prot/ip/ip.fun
prot/ip/ip.sig
prot/ip/ipfrag.fun
prot/ip/ipfrag.sig
prot/ip/iploop.fun
prot/ip/ipmux.sig
prot/ip/ipoptions.fun
prot/ip/ipoptions.sig
prot/ip/iproute.fun
prot/ip/iproute.sig
prot/ip/pseudoip.fun

(*
		9.	prot/rpc
*)

prot/rpc/dns/dns.fun
prot/rpc/dns/dns.sig

(*
		10.	prot/tcp
*)

prot/tcp/buildtcp.fun
prot/tcp/buildtcp.sig
prot/tcp/tcp.sig
prot/tcp/tcpaction.fun
prot/tcp/tcpaction.sig
prot/tcp/tcplog.fun
prot/tcp/tcplog.sig
prot/tcp/tcpmain.fun
prot/tcp/tcpreceive.fun
prot/tcp/tcpreceive.sig
prot/tcp/tcpresend.fun
prot/tcp/tcpresend.sig
prot/tcp/tcpsend.fun
prot/tcp/tcpsend.sig
prot/tcp/tcpstate.fun
prot/tcp/tcpstate.sig
prot/tcp/tcptcb.fun
prot/tcp/tcptcb.sig

(*
		11.	prot/udp
*)

prot/udp/buildudp.fun
prot/udp/buildudp.sig
prot/udp/udp.fun
prot/udp/udp.sig
prot/user.fun
prot/user.sig

(*
		11.	benchmarking
*)

(*
prot/timeprotocol.sig
prot/timeprotocol.fun
prot/timingtest.sig
prot/timingtest.fun
prot/benchmark.sig
prot/benchmark.fun
prot/eth/eth.tim
prot/ip/ip.tim
prot/udp/udp.tim
prot/tcp/tcp.tim
prot/proto.tim
prot/tests.tim
*)
