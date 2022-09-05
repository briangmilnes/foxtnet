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

	Conversion functions to and from lists and arrays, and
	manipulation of terminal attributes.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TERM_FLAGS
	2.	signature TERMINAL

		iii.	RCS Log
	
$Log: telnetutil.sig,v $
Revision 1.1  1996/07/05  17:41:58  esb
Initial revision


		1.	signature TERM_FLAGS

	Terminal attribute manipulation: the Posix flags are really
	messy to use.  Ugh.  Hopefully this will provide a cleaner
	interface.
*)

signature TERM_FLAGS =
 sig
  include POSIX_FLAGS
  val set: flags * flags -> flags
  val clear: flags * flags -> flags
  val setw: Word32.word * flags -> flags
  val clearw: Word32.word * flags -> flags
 end

(*
		2.	signature TERMINAL
*)

signature TERMINAL =
 sig
  type fd = Posix.FileSys.file_desc
  type index = Posix.TTY.V.index

  datatype flags =
      I of Posix.TTY.I.flags | Iw of SysWord.word
    | O of Posix.TTY.O.flags | Ow of SysWord.word
    | C of Posix.TTY.C.flags | Cw of SysWord.word
    | L of Posix.TTY.L.flags | Lw of SysWord.word

  val set: fd * flags -> unit
  val clear: fd * flags -> unit
  val setcc: fd * (index * char) list -> unit

  val get_input_speed: fd -> int
  val get_output_speed: fd -> int
 end
