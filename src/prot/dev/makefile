#
#$Log: makefile,v $
# Revision 1.26  1996/04/30  19:58:57  esb
# many changes, including renaming the subdirectories
#
# Revision 1.25  1996/04/30  15:04:43  esb
# removed ethdev.tst, which only tests the makestring routines
#
# Revision 1.24  1996/01/15  19:00:53  cline
# *** empty log message ***
#
# Revision 1.23  1995/09/25  16:53:28  cline
# added devmeter.fun
#
# Revision 1.22  1995/06/20  16:53:24  esb
# added makedev.fun
#
# Revision 1.21  1995/03/12  04:56:21  cstone
# Added alpha_osf20 directory
#
# Revision 1.20  1995/02/11  06:50:18  esb
# removed deviceUser.
#
# Revision 1.19  1994/10/20  17:58:30  cline
# *** empty log message ***
#
# Revision 1.18  1994/10/14  01:14:51  milnes
# Added "-" before cos.
#
# Revision 1.17  1994/10/04  21:12:11  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.16  1994/08/12  19:14:44  esb
# replaced ethdev.tim with devreal.tst.
#
# Revision 1.15  1994/06/16  16:45:05  danwang
# Updated to use functorized Fox_Basis
#
# Revision 1.14  1994/06/07  16:32:05  robby
# Added a signature
#
# Revision 1.13  94/05/23  14:13:05  milnes
# Added ethdev.tst.
# 
# Revision 1.12  1994/01/17  23:15:10  esb
# added buildsim.fun.
#
# Revision 1.11  1993/11/04  14:02:16  esb
# removed receive_thread.
#
# Revision 1.10  1993/10/25  17:37:45  milnes
# Added buildethdev.
#
# Revision 1.9  1993/09/20  22:59:21  esb
# removed ethdev.str
#
# Revision 1.8  1993/09/20  22:29:35  esb
# added receive_thread.
#
# Revision 1.7  1993/07/08  18:16:03  milnes
# got rid of test_real.sml
#
# Revision 1.6  1993/06/23  15:34:32  milnes
# Deleted Nick's hacked test file.
#
# Revision 1.5  1993/06/23  15:32:30  milnes
# Removed ethdev.sml in favor of ethdev.fun. Don't ask us why we had two copies, we don't know.
#
# Revision 1.4  1993/06/11  13:46:01  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:28:59  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := dev.sig makedev.fun devreal.tst sim.fun wire.fun wire.sig buildethdev.sig buildsim.sig buildsim.fun buildsim.sig devmeter.fun
DIRS := osf1 ultrix

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

