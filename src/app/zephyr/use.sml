
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

	A link file for the UDP protocol.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	Zephyr


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.5  1994/09/03  21:22:38  danwang
Update for new zephyr functors.

Revision 1.4  1994/07/14  16:55:13  milnes
Added zwrite.sig, zwrite.fun, zwrite.str.

Revision 1.3  1993/12/15  15:16:47  cline
Changed table of contents.

Revision 1.2  1993/11/11  05:57:33  esb
changed prot/udp to app/zephyr.

Revision 1.1  1993/10/20  14:21:34  cline
Initial revision


		1.	Zephyr
*)
local
  fun prefix l = map (fn x => "./app/zephyr/" ^ x) l
in

    val zephyr = prefix
	["zephyr_extern.sig",
	 "zwrite.sig",
	 "zephyr_lower.sig",
	 "zephyr.sig",
	 "zephyr_client.sig",
	 "zephyr_extern.fun",
	 "zwrite.fun",
	 "zephyr_lower.fun",
	 "zephyr.fun",     
	 "zephyr_client.fun",
	 "zclient.str"]

end


