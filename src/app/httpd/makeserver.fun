(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (Christopher.Stone@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

	i.	Abstract

        maketcpserver : Utility functions for creating a server program
                        running on TCP: installation, removal,
                        and downcalls.

	ii.	Table of Contents

        i.	Abstract
	ii.  	Table of Contents
	iii. 	RCS Log
 	1.      functor MakeTcpServer
	2. 	Types of tokens exported
	3.	function install
	4.      function uninstall
        5.      Packet format conversions
        6.      downcalls

	iii.	RCS Log
	
$Log: makeserver.fun,v $
# Revision 1.2  1995/03/08  20:27:31  cstone
# Takes TCP_STACK rather than TCP_PROTOCOL.
#
# Revision 1.1  1995/01/25  22:42:43  cstone
# Initial revision
#


------------------------------------------------------------------------
	1.	functor MakeTcpServer
*)

functor MakeTcpServer(structure B: FOX_BASIS
                      structure TcpStack: TCP_STACK
                      sharing type TcpStack.Tcp.incoming = B.Dyn_Array.T
                     ) : MAKETCPSERVER =
struct

   structure TcpStack = TcpStack
   structure Tcp  = TcpStack.Tcp
   structure Pipe = B.Pipe

   val servers_installed = ref 0

(*
------------------------------------------------------------------------
	2.	Types of tokens exported

                The server_id identifies a specific server (i.e,
                the httpd server).  Each producer_id corresponds to a
                connection, and is passed to the downcall functions.

*)	
   datatype server_id =
        SID of (unit -> unit) * (unit -> Tcp.connection list)

   datatype producer_id  = 
        PROD of Tcp.incoming option Pipe.T * Tcp.status Pipe.T

(*
------------------------------------------------------------------------
	3.	function install

                Creates the handler, does the passive open, and
                returns a server_id for the installed server.  The
                function server_fn will be called with a connection
                and producer_id for each connection.  instance_limit
                is as in start_passive.
*)

   fun install (addr: Tcp.address) server_fn instance_limit =
      let fun handler (connect : Tcp.connection) =
	  let val pkt_buffer  = B.Pipe.new NONE: Tcp.incoming option Pipe.T
	      val stat_buffer = B.Pipe.new NONE: Tcp.status Pipe.T
              val producer_id = PROD (pkt_buffer, stat_buffer)
	      val _ = B.Scheduler.fork 
                           (fn () => server_fn (connect, producer_id))
	  in
	      (fn pkt  =>  Pipe.enqueue (pkt_buffer, SOME pkt),
	       fn stat => (Pipe.enqueue (stat_buffer, stat);
			   Pipe.enqueue (pkt_buffer, NONE)))
          end

          val _    = TcpStack.initialize ()
          val _    = (servers_installed := !servers_installed + 1)
          val s_id = SID (Tcp.start_passive 
	                   (addr, Tcp.Handler handler, instance_limit))
      in
          s_id
      end


(*
------------------------------------------------------------------------
	4.	function uninstall

                Closes all currently open connections for a given
                server, and accepts no new connections.
*)

   fun uninstall (SID(stop, opens)) =
       (app Tcp.close (opens ());
	stop();
	TcpStack.finalize ();
        servers_installed := !servers_installed - 1;
        !servers_installed)

(*
------------------------------------------------------------------------
	5.	packet format conversion functions
*)

   val darray_to_intlist = (map FoxWord8.wordToInt) o B.Dyn_Array.to_list1
   val intlist_to_darray = B.Dyn_Array.init_list1 o (map FoxWord8.intToWord)


(*
------------------------------------------------------------------------
	6.	downcalls

                get_packet returns a single TCP packet; get_line
                returns a single line (terminated by LF or CRLF, which
                are stripped).  Either may raise StatusNotify if
                a status message has been received.
*)

   exception StatusNotify of Tcp.status

   fun get_packet (PROD(datapipe, statuspipe)) =
      let fun get_packet' () = 
             case (Pipe.dequeue datapipe) of
	        NONE => 
	           if (Pipe.size statuspipe = 0) then
	             get_packet' ()
	           else
	             raise StatusNotify(Pipe.dequeue statuspipe)
              | SOME pkt => pkt
      in
         get_packet'
      end

   fun get_string (prod as PROD(datapipe, statuspipe)) =
      let val get_pack = get_packet prod
          fun splitlist _ []  = ([], [])
            | splitlist 0 lst = ([], lst)
            | splitlist n (x::xs) = 
                let val (a,b) = splitlist (n-1) xs
                in
                    (x::a, b)
                end
          fun get_string' n = 
                 if (n <= 0) then 
                    []
                 else
                    let val lst = (darray_to_intlist o get_pack) ()
                        val len = B.V.List.length lst
                    in
                        if (len <= n) then
                           lst @ (get_string' (n - len))
                        else
                           let val (a,b) = splitlist n lst in
                              Pipe.requeue (datapipe, SOME (intlist_to_darray b));
                              a
                           end
                    end
      in
          implode o (map chr) o get_string'
      end

   (* CR = ASCII 13; LF = ASCII 10 *)
   fun get_line (prod as PROD(datapipe, statuspipe)) =
      let val get_pack = get_packet prod
          fun upto_LF lst [] = 
                (upto_LF lst ((darray_to_intlist o get_pack) ())
                 handle e => (Pipe.requeue (datapipe, 
                                            SOME (intlist_to_darray lst));
                              raise e))
            | upto_LF lst [10] = lst
            | upto_LF lst (10::cs) = 
                (Pipe.requeue (datapipe, SOME (intlist_to_darray cs)); lst)
            | upto_LF lst [13,10] = lst
            | upto_LF lst (13::10::cs) = 
                (Pipe.requeue (datapipe, SOME (intlist_to_darray cs)); lst)
            | upto_LF lst (c ::cs) = upto_LF (c::lst) cs

       in

          implode o (map chr) o rev o (fn () => upto_LF [] [])

       end

end


