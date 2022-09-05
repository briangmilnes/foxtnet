(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A user interface for FTP.

	Supported commands:
	    ls
	    ls dir
	    cd dir
	    get remotefile
	    get remotefile localfile
	    get remotefile -
	    quit/q/exit/ctrl-D
	    help/?

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ftp_ui

		iii.	RCS Log

$Log: ftp_ui.fun,v $
Revision 1.2  1994/10/27  10:25:35  cokasaki
better dns support, added Remote_Operation_Failed exception

Revision 1.1  94/08/25  11:52:04  cokasaki
Initial revision
	
	
*)

(*
	1.	functor Ftp_ui
*)

functor Ftp_ui (structure B : FOX_BASIS
		val foxrun : string (* The path to the runtime. *)
		structure Ftp : FTP) : FTP_UI =
struct

  fun lex s =  (* break a string into a list of words, dropping whitespace *)
	let fun white " "  = true
	      | white "\t" = true
	      | white "\n" = true
	      | white _ = false

	    fun drop [] = []
	      | drop (c :: cs) =
		  if white c then drop cs
                             else getword ([c],cs)

	    and getword (w,[]) = [implode (rev w)]
	      | getword (w,c::cs) =
		  if white c then implode (rev w) :: drop cs
		             else getword (c :: w,cs)

	in drop (explode s) end

  fun ftp servername =
    let val _ = Ftp.initialize ()
        val {get,ls,cd,quit} = Ftp.connect servername

	val helpmsg = "Available commands:\n"
	              ^ "\tls [ <remote-directory> ]\n"
	              ^ "\tcd <remote-directory>\n"
	              ^ "\tget <remote-file> [ <local-file> | - ]\n"
	              ^ "\tquit\n"

        fun check f =
	      (f (); ("",false)) 
	      handle Ftp.Remote_Operation_Failed err =>
		       (err ^ "\n", err = "Server closed connection.")

        fun myget remotename localname =
	  if localname = "-" then
	      get remotename std_out
	  else
	      let val dest = open_out localname
	      in
		  get remotename dest
		    handle exn =>
			(close_out dest;
			 System.Unsafe.SysIO.unlink localname;
			 raise exn);
		  close_out dest
	      end
              handle Io _ => raise Ftp.Remote_Operation_Failed 
		                     "Could not open local file."

        fun interp ws =
          case ws of
	    ["get",remotename,localname] => 
		check (fn () => myget remotename localname)
	  | ["get",filename] => check (fn () => myget filename filename)
	  | "get" :: _ => ("Usage: get <remote-file> [ <local-file> ]\n",false)
	  | ["cd",dirname] => check (fn () => cd dirname)
	  | "cd" :: _ => ("Usage: cd <remote-directory>\n", false)
	  | ["ls",dirname] => check (fn () => ls (SOME dirname) std_out)
	  | ["ls"] => check (fn () => ls NONE std_out)
	  | "ls" :: _ => ("Usage: ls [ <remote-directory> ]\n", false)
	  | "quit" :: _ => ("", true)
	  | "q"    :: _ => ("", true)
	  | "exit" :: _ => ("", true)
	  | "help" :: _ => (helpmsg, false)
	  | "?"    :: _ => (helpmsg, false)
	  | [] => ("", false)
	  | _ => ("Unknown command\n", false)

        fun loop () =
	    (B.V.print "fftp> ";
	     B.V.flush ();
	     if end_of_stream std_in then quit ()
	     else let val (result,done) = interp (lex (input_line std_in))
		  in
		      B.V.print result;
		      if done then quit ()
		      else loop ()
		  end)

    in
	B.V.print "WARNING: Do NOT try to write to a local file in afs.\n";
	B.V.print "         Be especially careful with the 'get foo' command if your local\n";
	B.V.print "         directory is in afs.\n";

	loop ();
        Ftp.finalize ();
	B.V.print "Bye...\n";
	()
    end
    handle exn =>
	(Ftp.finalize (); 
	 raise exn)


  fun make_ftp_executable () =
    let val scriptname = "fftp"
	val heapname = "." ^ scriptname ^ ".heap"

	fun startup_ftp ([_,_,_,hostname],_) = ftp hostname
	  | startup_ftp (_,_) = B.V.print ("Usage: " ^ scriptname ^ " hostname\n")

	val script = open_out scriptname
    in
	output (script, "\n" ^ foxrun ^ " -s " ^ heapname ^ " $1 $2\n");
	close_out script;
	execute ("chmod", ["a+x", scriptname]);
	  (* chmod doesn't work from within foxml.  do it by hand. *)
	
	exportFn(heapname, startup_ftp)
    end 

end

