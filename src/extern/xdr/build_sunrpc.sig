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

	Signature for BUILD_SUNRPC_PROT: allows SUN RPC to be built on
	                                 top of UDP or TCP.

	ii.	Table of Contents

	1.	signature BUILD_SUNRPC_PROT

	iii.	RCS Log

$Log: build_sunrpc.sig,v $
Revision 1.1  1994/10/14  11:59:57  kcchiang
Initial revision


*)

(*
        1.      signature BUILD_SUNRPC_PROT
*)
signature BUILD_SUNRPC_PROT =
sig
    structure Prot : PROTOCOL
    structure Ip : IP_PROTOCOL

    val address_pattern        : Prot.address_pattern
    val prot_header_size       : int
end
