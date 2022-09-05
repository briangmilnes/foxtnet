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

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FTP_UI

		iii.	RCS Log
	
$Log: ftp_ui.sig,v $
Revision 1.1  1994/08/25  11:49:35  cokasaki
Initial revision
	

*)

(*
	1.	signature FTP_UI
*)

signature FTP_UI = 
sig
    val ftp : string (* hostname *) -> unit

    val make_ftp_executable : unit -> unit
end
