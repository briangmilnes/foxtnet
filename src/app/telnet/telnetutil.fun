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

	Functions to manipulation of terminal attributes.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Term_Flags
	2.	functor Term

		iii.	RCS Log
	
$Log: telnetutil.fun,v $
Revision 1.1  1996/07/05  17:42:29  esb
Initial revision


		1.	functor Term_Flags
*)

functor Term_Flags (Flags: POSIX_FLAGS): TERM_FLAGS =
 struct
  open Flags				(* wordTo, toWord *)

  fun set    (f, fs) = wordTo (Word32.orb (toWord f, toWord fs))
  fun clear  (f, fs) = wordTo (Word32.andb (Word32.notb (toWord f), toWord fs))
  fun setw   (w, fs) = wordTo (Word32.orb (w, toWord fs))
  fun clearw (w, fs) = wordTo (Word32.andb (Word32.notb w, toWord fs))
 end

(*
		2.	functor Term
*)

structure Term: TERMINAL =
 struct
  type fd = Posix.FileSys.file_desc
  type index = Posix.TTY.V.index

  structure I = Term_Flags (Posix.TTY.I)
  structure O = Term_Flags (Posix.TTY.O)
  structure C = Term_Flags (Posix.TTY.C)
  structure L = Term_Flags (Posix.TTY.L)

  datatype flags =
      I of I.flags | Iw of Word32.word
    | O of O.flags | Ow of Word32.word
    | C of C.flags | Cw of Word32.word
    | L of L.flags | Lw of Word32.word

  local
   fun set_iflag (I f, iflag) = I.set (f, iflag)
     | set_iflag (Iw w, iflag) = I.setw (w, iflag)
     | set_iflag (_, iflag) = iflag

   fun set_oflag (O f, oflag) = O.set (f, oflag)
     | set_oflag (Ow w, oflag) = O.setw (w, oflag)
     | set_oflag (_, oflag) = oflag

   fun set_cflag (C f, cflag) = C.set (f, cflag)
     | set_cflag (Cw w, cflag) = C.setw (w, cflag)
     | set_cflag (_, cflag) = cflag

   fun set_lflag (L f, lflag) = L.set (f, lflag)
     | set_lflag (Lw w, lflag) = L.setw (w, lflag)
     | set_lflag (_, lflag) = lflag

  in
   fun set (fd, flag) =
        let val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} =
	          Posix.TTY.fieldsOf (Posix.TTY.tcgetattr fd)
	    val attr = Posix.TTY.termios {iflag = set_iflag (flag, iflag),
					  oflag = set_oflag (flag, oflag),
					  cflag = set_cflag (flag, cflag),
					  lflag = set_lflag (flag, lflag),
					  cc = cc, ispeed = ispeed,
					  ospeed = ospeed}
	in Posix.TTY.tcsetattr (fd, Posix.TTY.TC.sanow, attr)
	end
  end

  local
   fun clear_iflag (I f, iflag) = I.clear (f, iflag)
     | clear_iflag (Iw w, iflag) = I.clearw (w, iflag)
     | clear_iflag (_, iflag) = iflag

   fun clear_oflag (O f, oflag) = O.clear (f, oflag)
     | clear_oflag (Ow w, oflag) = O.clearw (w, oflag)
     | clear_oflag (_, oflag) = oflag

   fun clear_cflag (C f, cflag) = C.clear (f, cflag)
     | clear_cflag (Cw w, cflag) = C.clearw (w, cflag)
     | clear_cflag (_, cflag) = cflag

   fun clear_lflag (L f, lflag) = L.clear (f, lflag)
     | clear_lflag (Lw w, lflag) = L.clearw (w, lflag)
     | clear_lflag (_, lflag) = lflag

  in
   fun clear (fd, flag) =
        let val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} =
                   Posix.TTY.fieldsOf (Posix.TTY.tcgetattr fd)
	    val attr = Posix.TTY.termios {iflag = clear_iflag (flag, iflag),
					  oflag = clear_oflag (flag, oflag),
					  cflag = clear_cflag (flag, cflag),
					  lflag = clear_lflag (flag, lflag),
					  cc = cc, ispeed = ispeed,
					  ospeed = ospeed}
	in Posix.TTY.tcsetattr (fd, Posix.TTY.TC.sanow, attr)
	end
  end

  fun setcc (fd, l) =
       let val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} =
	         Posix.TTY.fieldsOf (Posix.TTY.tcgetattr fd)
	   val cc = Posix.TTY.V.cc l
	   val attr = Posix.TTY.termios {iflag = iflag, oflag = oflag,
					 cflag = cflag, lflag = lflag,
					 cc = cc, ispeed = ispeed,
					 ospeed = ospeed}
       in Posix.TTY.tcsetattr (fd, Posix.TTY.TC.sanow, attr)
       end

  val get_input_speed = (Word32.toInt o Posix.TTY.speedToWord o
			 Posix.TTY.cfgetispeed o Posix.TTY.tcgetattr)

  val get_output_speed = (Word32.toInt o Posix.TTY.speedToWord o
			  Posix.TTY.cfgetospeed o Posix.TTY.tcgetattr)

 end

