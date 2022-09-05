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

      timingtest.sig provides a signature for a structure that is useful for
 byte encoding and decoding a datatype that is used for timing control connections.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: timingtest.sig,v $
Revision 1.1  1995/06/20  17:27:21  esb
Initial revision

Revision 1.5  1994/09/12  18:21:13  milnes
 Added timing for ip_no_icmp/ip.

Revision 1.4  1994/08/17  16:32:15  esb
the benchmarks now work.

Revision 1.3  1994/07/07  16:32:32  esb
renamed TIMINGTEST to TIMING_TEST.

Revision 1.2  1994/06/16  21:52:46  danwang
Updated to use functorized Fox_Basis.

Revision 1.1  1994/04/20  14:37:59  milnes
Initial revision

		1.     signature TIMINGTEST
*)

signature TIMING_TEST = 
 sig
  type incoming_data
  type outgoing_data
  datatype protocol = Eth | Ip_No_Icmp | Ip | Udp | Tcp
  exception Emit_Error of string
  exception Parse_Error of string * incoming_data

  datatype check_data = None | Some | All 
  datatype test = Test of {protocol: protocol,   
			   repetitions: ubyte4,
			   size: ubyte4,
			   confirm: ubyte4, 
			   check_data: check_data,
			   print_packets: bool,
			   print_history: bool}

  val makestring_test: test -> string 
  val parse_test: incoming_data -> test
  val emit_test: ('a * int -> (outgoing_data * (unit -> unit))) * 'a * test
               -> (unit -> unit)
 end 





