(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline    (Ken.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A use file for ftp.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	FTP files

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.1  1994/08/25  11:47:05  cokasaki
Initial revision


*)

(*
	1.	FTP files
*)

val ftp = map (fn s => "./app/ftp/" ^ s)
              ["ftp.sig","ftp.fun","ftp.str",
	       "ftp_ui.sig","ftp_ui.fun","ftp_ui.str"]
