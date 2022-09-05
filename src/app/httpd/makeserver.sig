(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (Christopher.Stone@cs.cmu.edu
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

		i.	Abstract
		ii.	Table of Contents
		iii.	RCS Log
		1.	signature MAKETCPSERVER

		iii.	RCS Log
	
$Log: makeserver.sig,v $
# Revision 1.2  1995/03/08  20:28:19  cstone
# Uses TCP_STACK rather than TCP_PROTOCOL
#
# Revision 1.1  1995/01/25  22:40:51  cstone
# Initial revision
#

		1.	signature MAKETCPSERVER
*)

signature MAKETCPSERVER =
sig

   structure TcpStack : TCP_STACK

   type server_id
   type producer_id

   exception StatusNotify of TcpStack.Tcp.status

   val install    : TcpStack.Tcp.address -> 
                     ((TcpStack.Tcp.connection * producer_id) -> unit) -> 
                     int option -> 
                     server_id

   val uninstall  : server_id -> int
       
   val get_line   : producer_id -> unit -> string
   val get_packet : producer_id -> unit -> TcpStack.Tcp.incoming
   val get_string : producer_id -> int  -> string
end
