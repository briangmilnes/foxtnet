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

	The test files for the FoxNet.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	include whatever sources are needed
	2.	basic testing
	3.	testing util

		iii.	RCS Log
	
$Log: test.sc,v $
Revision 1.7  1995/03/08  18:12:30  esb
commented out addressing.tst.

Revision 1.6  1995/03/08  16:01:42  esb
added addressing.tst.

Revision 1.5  1995/03/08  00:00:23  esb
added word.tst, removed uip.tst, uudp.tst, and utcp.tst.

Revision 1.4  1995/02/21  16:34:34  robby
changed "-sources.sc" to "sources.sc".

Revision 1.3  1995/02/09  19:47:48  esb
added the files that were commented out, improved the comments.


		1.	include whatever sources are needed
*)

sources.sc

(*
		2.	basic testing
*)

vendor/word.tst
test/teststructure.sig
test/test.tst

(*
		3.	test the basis
*)

util/store.tst
util/fifo.tst
util/lifo.tst
util/priority.tst
util/checksum.tst
util/checksum.tim
util/tree.tst
util/bytearrays/copy.tst
util/bytearrays/copy.tim
util/bytearrays/access.tst
util/bytearrays/dynarray.tst
util/order/order.tst
util/order/order.tim
filter/filter.tst
control/coro.tst
control/coro.tim
control/event.tst
control/pipe.tst
extern/xdr/xdr.tst

(*
		3.	test the protocols
*)

prot/conn.tst
(*
prot/addressing/addressing.tst
*)
prot/dev/sim.tst
prot/dev/ethdev.tst
prot/eth/eth.tst
prot/arp/arp.tst
prot/ip/ip.tst
prot/ip/ipoptions.tst
prot/icmp/icmp.tst
prot/icmp/ipicmp.tst
prot/icmp/addressmask.tst
prot/icmp/iplocalstatus.tst
prot/udp/udp.tst
prot/tcp/tcpstate.tst
prot/tcp/tcpsend.tst
prot/tcp/tcpreceive.tst
prot/tcp/tcpaction.tst
prot/tcp/tcp.tst
prot/tcp/tcpping.tst
