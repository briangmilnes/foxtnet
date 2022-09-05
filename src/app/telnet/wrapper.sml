(*---------------------------------------------------------------------------*>
 *
 *  wrapper.fun
 *  3 / 6 / 96
 *  Sidd Puri
 *
 *  Import C functions defined in the foxml runtime and wrap them with an ML
 *  function.
 *
<*---------------------------------------------------------------------------*)

local

val fox_fun = System.Unsafe.CInterface.c_function "FoxNet"
val fd2int  = SysWord.toInt o Posix.FileSys.fdToWord
val int2fd  = Posix.FileSys.wordToFD o SysWord.fromInt

in

(*---------------------------------------------------------------------------*>
 *
 *  (from ml-ptystuff.c)
 *
 *  fd = Posix.FileSys.file_desc
 *
 *  poll : fd * int -> bool
 *
<*---------------------------------------------------------------------------*)

local
  val c_poll :
        int				(* fd to poll for read *)
      * int				(* timeout in msec *)
     -> bool				(* true if input pending *)
    = fox_fun "poll"
in
  fun poll (fd, timeout) = c_poll (fd2int fd, timeout)
end

(*---------------------------------------------------------------------------*>
 *
 *  (from ml-packetfilter.c)
 *
 *  fd = Posix.FileSys.file_desc
 *
 *  select :
 *      int * (fd list * fd list * fd list) * float option
 *   -> int * (fd list * fd list * fd list)
 *
<*---------------------------------------------------------------------------*)

local
  val c_select :
        int				(* number of file descriptors *)
      * int list			(* read fd's *)
      * int list			(* write fd's *)
      * int list			(* exception fd's *)
      * (int * int) option		(* timeout (sec, usec) *)
     -> int				(* number of ready fd's *)
    = fox_fun "select"
in
  fun select (nfd, (read, write, exn), timeout) = let
    val (r, w, e) = (map fd2int read, map fd2int write, map fd2int exn)
    val t = case timeout of
        SOME x => SOME (floor x, floor ((x - real (floor x)) * 1000.0))
      | NONE => NONE
    val nfd = c_select (nfd, r, w, e, t)
    val (read, write, exn) = (map int2fd r, map int2fd w, map int2fd e)
  in
    (nfd, (read, write, exn))
  end
end

(*---------------------------------------------------------------------------*>
 *
 *  (from ml-ptystuff.c)
 *
 *  fd = Posix.FileSys.file_desc
 *  winsize = int * int
 *
 *  openpty     : winsize -> fd * fd
 *  get_winsize : fd -> winsize
 *  set_winsize : fd * winsize -> unit
 *  new_session : fd -> unit
 *
<*---------------------------------------------------------------------------*)

type winsize = int * int

local
  val c_openpty :
        int * int			(* window size *)
     -> int * int			(* master and slave file descriptors *)
    = fox_fun "openpty"

  val c_get_winsize :
        int				(* tty file descriptor *)
     -> int * int			(* window size *)
    = fox_fun "get_winsize"

  val c_set_winsize :
        int				(* tty file descriptor *)
      * int * int			(* window size *)
     -> unit
    = fox_fun "set_winsize"

  val c_new_session :
        int				(* tty file descriptor *)
     -> unit
    = fox_fun "new_session"
in
  fun openpty (termios, winsize) = let
    val (master, slave) = c_openpty winsize
  in
    Posix.TTY.tcsetattr (int2fd master, Posix.TTY.TC.sanow, termios);
    (int2fd master, int2fd slave)
  end

  val get_winsize = c_get_winsize o fd2int

  fun set_winsize (tty, (rows, cols)) =
    c_set_winsize (fd2int tty, rows, cols)

  val new_session = c_new_session o fd2int
end

(*---------------------------------------------------------------------------*)

end
