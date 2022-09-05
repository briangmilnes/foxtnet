(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
        Ken Cline    (Ken.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Support for reading realm information from configuration file.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature KRB_REALM


		iii.	RCS Log
	
$Log: krbrealm.sig,v $
Revision 1.1  1994/08/25  12:13:36  robby
Initial revision

Revision 1.1  1994/07/13  18:49:02  robby
Initial revision

Revision 1.1  1994/05/20  02:25:06  robby
Initial revision



		1.	signature KRB_REALM
 *)

signature KRB_REALM =
sig

  (* Return the local realm name *)
  val local_realm       : unit -> string

  (* Return the list of hostnames that config_file lists as
     Kerberos servers for the given realm *)
  val kerberos_servers  : string -> string list
end
