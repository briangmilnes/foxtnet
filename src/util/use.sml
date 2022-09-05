(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A link file for the trivial util module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.25  1995/02/13  22:58:51  esb
added string-to-word.sml.

Revision 1.24  1994/10/19  23:04:54  milnes
added environ.sig and environ.fun.

Revision 1.23  1994/09/30  16:34:26  esb
removed test_timingboard.

Revision 1.22  1994/09/23  18:24:46  milnes
Removed trie.tst in favor of tree.tst.

Revision 1.21  1994/09/21  15:14:52  milnes
Replaced tries with trees.

Revision 1.20  1994/07/01  04:51:28  danwang
Added trie files.

Revision 1.19  1994/06/16  16:51:18  danwang
Updated for functorized Fox_Basis

Revision 1.18  1994/02/21  00:16:22  esb
added orderutil.

Revision 1.17  94/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.15  1994/01/17  21:37:24  esb
added timingboard.

Revision 1.14  1994/01/13  16:18:52  milnes
Updated on the path to updating coroutines with a delete.

Revision 1.13  93/11/05  20:46:24  cline
added a missing ','

Revision 1.12  1993/11/05  20:40:39  cline
added unaligned.{sig,sml}

Revision 1.11  1993/11/02  22:35:39  esb
added checksum.tim and test_arrayutil.

Revision 1.10  1993/10/06  01:22:26  esb
added store.tst.

Revision 1.9  1993/09/20  23:10:56  esb
restructured into code, test, and real.

Revision 1.8  1993/09/02  15:47:01  esb
added a use for bytearrays/use.sml.

Revision 1.7  1993/08/30  15:56:22  milnes
Added access.sig,fun and str.

Revision 1.6  1993/07/23  16:07:04  esb
added checksum.tst

Revision 1.5  1993/07/20  14:29:45  esb
added priority queue data structure files.

Revision 1.4  1993/07/10  04:05:09  esb
removed the timing functions, which are now in ../time

Revision 1.3  1993/06/18  14:51:03  esb
eliminated some unused files

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	util
*)

(* Although timingboard is really a utility, to allow it to be called
 in basic modules like vendor, we load it in src/use.sml. *)

use "./util/bytearrays/use.sml";
use "./util/parsing/use.sml";
use "./util/order/use.sml";

val util = (map (fn x => "./util/" ^ x)
		["store.sig", "store.fun",
		 "fifo.sig", "fifo.fun",
(* lifo is currently not used. Include this line again if you need to use it.
		 "lifo.sig", "lifo.fun",
 *)
		 "priority.sig", "priority.fun",
		 "checksum.sig", "checksum.fun",		 
		 "tabulate.sml",
                 "unaligned.sig", "unaligned.sml",
                 "environ.sig", "environ.fun",
                 "deq.sig", "deq.fun", "tree.sig", "tree.fun",
		 "string-to-word.sml"]) 
                  @ arrayutil @ orderutil @ parsing

val test_util = (map (fn x => "./util/" ^ x)
		     ["store.tst", "fifo.tst",
		      "lifo.sig", "lifo.fun", "lifo.tst",
		      "priority.tst", "checksum.tst", "checksum.tim", "tree.tst"]) @
                test_arrayutil @ test_orderutil

