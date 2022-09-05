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
	1.	signature TELNET_PTY

		iii.	RCS Log
	
$Log: telnetpty.sig,v $
Revision 1.1  1996/07/05  17:40:30  esb
Initial revision


		1.	signature TELNET_PTY
*)

signature TELNET_PTY =
 sig
  type fd = Posix.FileSys.file_desc
  type termios = Posix.TTY.termios
  type winsize = int * int
  type time_ms = int

  val get_winsize: fd -> winsize
  val new_session: fd -> unit
  val openpty: termios * winsize -> fd * fd
  val select: fd * time_ms -> bool
  val set_winsize: fd * winsize -> unit
 end
