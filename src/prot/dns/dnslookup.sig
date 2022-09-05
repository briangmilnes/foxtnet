(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	DNS Lookup signature


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DNS_LOOKUP

		iii.	RCS Log
	
$Log: dnslookup.sig,v $
Revision 1.3  1996/02/29  17:35:33  esb
added type transport_setup and made it part of the overall setup.

Revision 1.2  1996/02/23  19:54:31  cline
added setup argument to lookup and inverse_lookup

Revision 1.1  1996/02/15  15:06:33  cline
Initial revision


	1.	signature DNS_LOOKUP
*)

signature DNS_LOOKUP =
 sig

  (*
     Setup type for DNS lookup functions.  Lookup searches for hosts
     by appending each domain listed in domain_list to the end of the
     given name (use a single "." for the case where no domain suffix
     is desired).  For example:

	["alias.cs.cmu.edu", "cs.cmu.edu", "cmu.edu", "."]

     Servers is a list of DNS server IP addresses in expressed as
     dotted numeric strings (e.g. ["128.2.222.180"]).
   *)
  type host_id
  type transport_setup
  type setup = {domain_list: string list,
		servers: host_id list,
		transport: transport_setup}

  val lookup: setup -> string -> host_id option
  val inverse_lookup: setup -> host_id -> string option
 end
