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
	ii.	Table of Contents
	iii.	RCS Log

$Log: use.sml,v $
Revision 1.2  1995/02/07  23:27:24  esb
revised according to standard usage.

Revision 1.1  1994/10/14  11:50:43  kcchiang
Initial revision


*)

val xdr = ["./extern/xdr/xdr.sig","./extern/xdr/xdr.fun"]

val test_xdr = ["./extern/xdr/xdr.tst"]

val sunrpc_server =
    (map (fn x=>"./extern/xdr/"^x) 
     ["wrap.sig","wrap.fun","sunrpc.sig","sunrpc.fun","build_sunrpc.sig",
      "build_sunrpc_udp.fun","sunrpc_server.sig","sunrpc_server.fun",
      "sunrpc_udp.str"])

val test_sunrpc_server = ["./sunrpc_server.tst"]
