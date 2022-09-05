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

	This file provides various options for linking together the
FoxNet protocols software using SourcGroup.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	FoxNet


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.29  1995/02/13  22:55:37  esb
added test_extern.

Revision 1.28  1994/09/30  16:56:52  esb
removed vendor and timingboard.

Revision 1.27  1994/09/19  22:09:00  robby
removed packet stuff. Note: I had re-added it to allow both basies to
be built, but that is no longer necessary.

Revision 1.26  1994/08/26  21:49:50  robby
added send and receive packet back in

Revision 1.25  1994/08/26  14:20:33  robby
added eXene

Revision 1.24  1994/08/26  14:19:07  milnes
Added locs.

Revision 1.23  1994/08/02  20:21:31  esb
removed packet.

Revision 1.22  1994/07/14  20:31:13  robby
added extern directory and kebreros

Revision 1.21  94/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.19  1994/01/17  21:36:53  esb
removed timingboard (now moved to util).

Revision 1.18  1994/01/14  20:18:05  milnes
Added timingboard/use.sml use.

Revision 1.17  1994/01/14  20:16:17  milnes
Added timingboard.

Revision 1.16  1993/12/23  22:57:32  esb
added test_packet.

Revision 1.15  1993/11/10  16:17:19  cline
added app/use.sml

Revision 1.14  1993/10/25  17:49:31  milnes
Beats me what I did.

Revision 1.13  1993/10/12  22:32:39  esb
changed time and post_load to time and real.

Revision 1.12  1993/10/09  18:25:00  esb
changed "time_" back to "real_", since some of the files included
under this name measure functionality but not timing or performance.

Revision 1.11  1993/10/08  15:43:28  milnes
Corrected and updated buildimage.

Revision 1.10  1993/10/07  14:38:56  milnes
Added a buildimage. The chmod does not yet work.

Revision 1.9  1993/09/20  23:10:41  esb
restructured into code, test, and real.

Revision 1.8  1993/09/17  16:48:01  milnes
Changed something or other in here, but I can't remember what as RCS
lets me check it out without an intended change listing.

Revision 1.7  1993/09/13  22:08:07  cline
deleted '#'s from RCS log

Revision 1.6  1993/09/02  11:59:01  esb
added basis to the directories.

Revision 1.5  1993/07/20  15:22:14  esb
Made indexing optional and improved the comment.

Revision 1.4  1993/07/01  15:54:43  milnes
*** empty log message ***

Revision 1.3  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.2  1993/06/10  19:28:37  milnes
Wasted the user.

Revision 1.1  93/06/10  21:30:03  milnes
Initial revision

Revision 1.1  1993/06/10  21:25:20  milnes
Initial revision


		1.	FoxNet
*)

(* Use this statement to turn on index file generation for gnutags.
              System.Control.indexing := true;

   Use this statement to turn off index file generation for gnutags.
   This makes it easier to run as rootl, since rootl doesn't have
   write permission on AFS.
              System.Control.indexing := false;
 *)

use "./useutils2.sml";
use "./extern/use.sml";
use "./locative/use.sml";
use "./basis/use.sml"; 
use "./control/use.sml";
use "./debug/use.sml";
use "./filter/use.sml";
(*use "packet/use.sml";*)
use "./prot/use.sml";
use "./test/use.sml"; 
use "./time/use.sml";
use "./useutils.sml";
use "./util/use.sml";
use "./app/use.sml";
use "eXene/use.sml";

val base = test @ debug @ util @ filter @ 
  control @ time @ extern @ locs @ basis 

val all = base @ protocols

val test_base = test_util @ test_filter @ test_control @ test_extern

val test_all = test_base @ test_protocols

val real_all = real_protocols

val time_all = time_protocols

fun foxuse files = map use files

