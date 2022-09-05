(* Conversion functions -----------------------------------------------------*)

val list_to_array =
  Word_Array.from8
  o Word_Array.W8.Native.F.new
    (fn [] => NONE | (x :: xs) => SOME (Word8.fromInt x, xs))

val array_to_list =
  (Word_Array.W8.Native.R.fold
    (fn (x, xs) => Word8.toInt x :: xs)
    [])
  o Word_Array.to8

(* Terminal attribute manipulation ------------------------------------------*)

(* The Posix flags are really messy to use.  Ugh. *)
(* Hopefully this will provide a cleaner interface. *)

signature TERM_FLAGS = sig
  include POSIX_FLAGS
  val set   : flags * flags -> flags
  val clear : flags * flags -> flags
end

functor TermFlags (Flags : POSIX_FLAGS) = struct
  open Flags
  open SysWord
  fun set    (f, fs) = wordTo (orb (toWord f, toWord fs))
  fun clear  (f, fs) = wordTo (andb (notb (toWord f), toWord fs))
  fun setw   (w, fs) = wordTo (orb (w, toWord fs))
  fun clearw (w, fs) = wordTo (andb (notb w, toWord fs))
end

signature TERM = sig
  datatype flags =
      I of Posix.TTY.I.flags | Iw of SysWord.word
    | O of Posix.TTY.O.flags | Ow of SysWord.word
    | C of Posix.TTY.C.flags | Cw of SysWord.word
    | L of Posix.TTY.L.flags | Lw of SysWord.word

  val set   : Posix.FileSys.file_desc * flags -> unit
  val clear : Posix.FileSys.file_desc * flags -> unit
  val setcc : Posix.FileSys.file_desc * (Posix.TTY.V.index * char) list -> unit
end

structure Term : TERM = struct
  structure I = TermFlags (Posix.TTY.I)
  structure O = TermFlags (Posix.TTY.O)
  structure C = TermFlags (Posix.TTY.C)
  structure L = TermFlags (Posix.TTY.L)

  datatype flags =
      I of I.flags | Iw of SysWord.word
    | O of O.flags | Ow of SysWord.word
    | C of C.flags | Cw of SysWord.word
    | L of L.flags | Lw of SysWord.word

  fun set (fd, flag) = let
    val { iflag, oflag, cflag, lflag, cc, ispeed, ospeed } =
      Posix.TTY.fieldsOf (Posix.TTY.tcgetattr fd)
    val attr = Posix.TTY.termios {
      iflag = case flag of I  f => I.set    (f, iflag)
			 | Iw w => I.setw   (w, iflag) | _ => iflag,
      oflag = case flag of O  f => O.set    (f, oflag)
			 | Ow w => O.setw   (w, oflag) | _ => oflag,
      cflag = case flag of C  f => C.set    (f, cflag)
			 | Cw w => C.setw   (w, cflag) | _ => cflag,
      lflag = case flag of L  f => L.set    (f, lflag)
			 | Lw w => L.setw   (w, lflag) | _ => lflag,
      cc = cc, ispeed = ispeed, ospeed = ospeed
    }
  in Posix.TTY.tcsetattr (fd, Posix.TTY.TC.sanow, attr) end

  fun clear (fd, flag) = let
    val { iflag, oflag, cflag, lflag, cc, ispeed, ospeed } =
      Posix.TTY.fieldsOf (Posix.TTY.tcgetattr fd)
    val attr = Posix.TTY.termios {
      iflag = case flag of I  f => I.clear  (f, iflag)
			 | Iw w => I.clearw (w, iflag) | _ => iflag,
      oflag = case flag of O  f => O.clear  (f, oflag)
			 | Ow w => O.clearw (w, oflag) | _ => oflag,
      cflag = case flag of C  f => C.clear  (f, cflag)
			 | Cw w => C.clearw (w, cflag) | _ => cflag,
      lflag = case flag of L  f => L.clear  (f, lflag)
			 | Lw w => L.clearw (w, lflag) | _ => lflag,
      cc = cc, ispeed = ispeed, ospeed = ospeed
    }
  in Posix.TTY.tcsetattr (fd, Posix.TTY.TC.sanow, attr) end

  fun setcc (fd, l) = let
    val { iflag, oflag, cflag, lflag, cc, ispeed, ospeed } =
      Posix.TTY.fieldsOf (Posix.TTY.tcgetattr fd)
    val cc = Posix.TTY.V.cc l
    val attr = Posix.TTY.termios {
      iflag = iflag, oflag = oflag, cflag = cflag, lflag = lflag,
      cc = cc, ispeed = ispeed, ospeed = ospeed
    }
  in Posix.TTY.tcsetattr (fd, Posix.TTY.TC.sanow, attr) end
end

(*---------------------------------------------------------------------------*)
