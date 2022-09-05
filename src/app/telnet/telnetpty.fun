(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Sidd Puri (puri@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

        Collection of functions for dealing with Unix pseudo-terminals.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure Telnet_Pty
	2.	declarations from runtime
	3.	conversion functions
	4.	function select
	5.	function openpty
	6.	function get_winsize
	7.	function set_winsize
	8.	function new_session

		iii.	RCS Log
	
$Log: telnetpty.fun,v $
Revision 1.1  1996/07/05  17:41:46  esb
Initial revision


		1.	structure Telnet_Pty
*)

functor Telnet_Pty (structure B: FOX_BASIS
		    val debug_level: int ref option): TELNET_PTY =
 struct

  type winsize = int * int
  type fd = Posix.FileSys.file_desc
  type c_fd = int
  type termios = Posix.TTY.termios
  type time_ms = int

  local

   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "telnetpty.fun"
			    val makestring = fn _ => NONE)

(*
		2.	declarations from runtime
*)

   val fox_fun = System.Unsafe.CInterface.c_function "FoxNet"
 
   val c_openpty: winsize -> c_fd * c_fd = fox_fun "openpty"

   val c_get_winsize: c_fd -> winsize = fox_fun "get_winsize"

   val c_set_winsize: c_fd (* tty *) * int * int (* winsize *) -> unit
         = fox_fun "set_winsize"

   val c_setsid: c_fd (* tty *) -> unit = fox_fun "setsid"

   (* the last argument to select tells us if there is a timeout,
      and, if so, what the timeout is in seconds and microseconds. *)
   val c_select: int * c_fd list * c_fd list * c_fd list * (int * int) option
                 -> int = fox_fun "select"

(*
	3.	conversion functions
*)

   fun fd_to_int fd =
        Word32.toInt (Posix.FileSys.fdToWord fd)

   fun int_to_fd fd =
        Posix.FileSys.wordToFD (Word32.fromInt fd)

(*
	4.	function select

	Unix select packet filter for reading with timeout ms milliseconds.
	Returns false if it times out, true if the file descriptor is ready.
	This code lifted from Alpha-OSF1 packetfilter.
*)

  in
   fun select (fd, ms) =
        let val sec = ms quot 1000
	    val usec = 1000 * (ms rem 1000)
	    val int_fd = Word32.toInt (Posix.FileSys.fdToWord fd)
	    val nfds = int_fd + 1
	    val result = ((c_select (nfds, [int_fd], [], [], SOME (sec, usec)))
			  handle x =>
			   Trace.print_raise_again (x, SOME "select"))
        in result > 0
        end (* let *)

(*
	5.	function openpty
*)

    fun openpty (termios, winsize) =
         let val (master, slave) = c_openpty winsize
	 in Posix.TTY.tcsetattr (int_to_fd master,
				 Posix.TTY.TC.sanow, termios);
	    (int_to_fd master, int_to_fd slave)
	 end

(*
	6.	function get_winsize
*)

    val get_winsize = c_get_winsize o fd_to_int

(*
	7.	function set_winsize
*)

    fun set_winsize (tty, (rows, cols)) =
         c_set_winsize (fd_to_int tty, rows, cols)

(*
	8.	function new_session
*)

    val new_session = c_setsid o fd_to_int

  end (* local *)
 end
