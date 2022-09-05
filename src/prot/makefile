# 
#$Log: makefile,v $
# Revision 1.31  1996/04/30  19:26:45  esb
# removed addressing
#
# Revision 1.30  1996/02/26  15:28:01  cline
# added directory init
#
# Revision 1.29  1995/06/20  17:13:37  esb
# major reorganization to get all source files out of this directory.
#
# Revision 1.28  1994/10/26  13:59:43  esb
# added simple.fun and demo.str.
#
# Revision 1.27  1994/10/21  13:52:46  milnes
# Added addrssing
# .
#
# Revision 1.26  1994/10/14  01:14:14  milnes
# Added "-" before cos.
#
# Revision 1.25  1994/10/13  17:04:08  milnes
# Added the tcp/ip stuff.
#
# Revision 1.24  1994/10/11  17:52:15  cline
# changed /usr/cs/bin/csh /bin/csh
#
# Revision 1.23  1994/08/29  20:05:16  robby
# refixed the incorrect previous fixing of the spelling error
#
# Revision 1.22  1994/08/29  20:03:55  robby
# fixed my spelling error
#
# Revision 1.21  1994/08/29  16:59:24  robby
# added buildudptcp's .fun and .sig files.
#
# Revision 1.20  1994/08/02  20:35:22  esb
# added conn.{sig,fun,tst}.
#
# Revision 1.19  1994/06/25  20:22:22  robby
# added rpc dir
#
# Revision 1.18  94/06/20  20:15:07  esb
# replaced timeprotocol.fun with benchmark.sig and benchmark.fun.
# 
# Revision 1.17  1994/05/23  14:04:42  milnes
# Added icmp.
#
# Revision 1.16  1994/04/21  15:25:53  milnes
# Added timing test .sig and .fun[
#
# Revision 1.15  1994/03/29  17:44:40  milnes
# Added tests.tim.
#
# Revision 1.14  1994/02/17  17:10:27  milnes
# Checked out, reinstalled old, rechecked in to repair the afs file
# move problems that confused rcs.
#
# Revision 1.13  1994/01/18  17:51:46  milnes
# Removed foxmachines as it is outdated.
#
# Revision 1.12  1994/01/17  23:46:26  esb
# added testaddress.str.
#
# Revision 1.11  1993/11/10  21:20:54  esb
# added foxmachines.sig.
#
# Revision 1.10  1993/09/10  18:11:36  milnes
# Added timeprotocol and prot.tim
#
# Revision 1.9  1993/09/10  11:26:32  cline
# added arp
#
# Revision 1.8  93/08/25  15:26:23  esb
# removed pdisp.fun.
# 
# Revision 1.7  1993/07/11  02:30:48  esb
# added pdisp.fun
#
# Revision 1.6  1993/06/22  17:53:56  esb
# added user.fun
#
# Revision 1.5  1993/06/11  13:46:04  milnes
# Makefile->makefile.
#
# Revision 1.4  93/06/11  17:37:14  milnes
# Reverted again.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := 
DIRS := arp benchmark block dev dns eth icmp ip proto rpc stack tcp udp init

all: make populate

make: 
	-co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	-co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

