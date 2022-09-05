#
# $Log: makefile,v $
# Revision 1.39  1997/03/10  19:10:16  esb
# added httpd.cm and ping.cm.
#
# Revision 1.38  95/11/13  16:49:50  cstone
# Added foxbasis.use
# 
# Revision 1.37  1995/09/28  17:55:45  cstone
# added foxnet.use
#
# Revision 1.36  1995/06/20  16:19:04  esb
# new setup, does not completely work yet, checked in so Ken can work on it.
#
# Revision 1.35  1995/03/08  18:15:56  esb
# added basis.sc.
#
# Revision 1.34  1995/03/08  15:57:27  esb
# eliminated the version-dependent files.
#
# Revision 1.33  1994/11/09  19:11:49  cline
# added test106.sc
#
# Revision 1.32  1994/11/09  19:09:43  cline
# added sources106.sc
#
# Revision 1.31  1994/10/26  19:41:09  milnes
# Added the numbers-*.str files.
#
# Revision 1.30  1994/10/14  01:06:29  milnes
# Added "-" before the cos to ignore check out errors on the alphas.
#
# Revision 1.29  1994/10/11  18:31:53  cline
# fixed added .sml to make-numbers.
#
# Revision 1.28  1994/10/11  18:28:24  cline
# added test.sc, real.sc, and make-numbers
#
# Revision 1.27  1994/10/04  17:34:41  esb
# changed /usr/cs/bin/csh to /bin/csh, which exists on most architectures
#
# Revision 1.26  1994/09/30  17:22:58  esb
# added sources.sc
#
# Revision 1.25  1994/08/25  23:03:26  milnes
# Added locative.
#
# Revision 1.24  1994/08/15  20:38:02  robby
# added eXene
#
# Revision 1.23  94/08/02  20:21:31  esb
# removed packet.
# 
# Revision 1.22  1994/07/14  20:31:01  robby
# added extern directory
#
# Revision 1.21  94/02/21  10:37:57  esb
# added useutils2.sml.
# 
# Revision 1.20  94/01/18  16:40:05  milnes
# Removed the timingboard stuff, its now in util.
# 
# Revision 1.19  1994/01/17  19:51:04  milnes
# Changes for ip fragmentation.
#
# Revision 1.18  93/12/14  22:04:16  cline
# added directory app
# 
# Revision 1.17  1993/09/02  11:58:49  esb
# added basis to the directories.
#
# Revision 1.16  1993/07/08  20:22:22  milnes
# Removed TAGS from the archive as it is binary and RCS breaks on it.
#
# Revision 1.15  1993/06/11  13:54:31  milnes
# Killed the user. It's not often that software gets to do that.
#
# Revision 1.14  93/06/11  13:53:51  milnes
# Just for Edo.
# 
# Revision 1.13  93/06/11  13:49:26  milnes
# Bagged testutil.
# 
# Revision 1.12  93/06/11  13:46:16  milnes
# Makefile->makefile.
# 
# Revision 1.11  93/06/11  13:29:13  milnes
# Reconstructed after cataclism.
# 


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := README foxbasis.cm foxnet.cm foxtest.cm httpd.cm ping.cm makefile foxnet.use foxbasis.use
DIRS := administrivia app basis control debug eXene extern filter prot test time util vendor

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

