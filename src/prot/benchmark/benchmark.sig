(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: benchmark.sig,v $
Revision 1.1  1995/06/20  17:27:21  esb
Initial revision

Revision 1.2  1995/03/10  03:49:26  esb
partial port to 1.07.

Revision 1.1  1994/06/20  15:45:56  esb
Initial revision


		1.	signature BENCHMARK_PROTOCOL
*)

signature BENCHMARK_PROTOCOL =
 sig
  type client_address
  type server_address
  type check_data
  type ubyte4

  val run_client: {repetitions: ubyte4, size: ubyte4, confirm: ubyte4,
		   print_history: bool} * client_address
                -> string

  val run_server: {repetitions: ubyte4, size: ubyte4, confirm: ubyte4,
		   check_data: check_data, print_packets: bool,
		   print_history: bool}
                 * server_address
                -> string
 end



