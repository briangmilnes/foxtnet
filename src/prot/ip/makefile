#
#$Log: makefile,v $
# Revision 1.36  1996/04/30  20:26:12  esb
# removed ipreal.tst iptrace.tst
#
# Revision 1.35  1996/02/27  18:29:26  esb
# renamed host_id.str to hostid.str.
#
# Revision 1.34  1996/02/26  15:26:06  cline
# added host_id.str
#
# Revision 1.33  1996/01/15  20:09:40  cline
# added icmpheader.{sig,fun}
#
# Revision 1.32  1995/10/04  21:36:19  esb
# added three test programs
#
# Revision 1.31  1995/06/29  19:14:19  esb
# added ipfrag.tst
#
# Revision 1.30  1995/06/23  20:15:00  esb
# added ipheader.tst
#
# Revision 1.29  1995/06/21  16:16:48  cline
# added pseudoip.tst
#
# Revision 1.28  1995/06/20  20:41:34  cline
# *** empty log message ***
#
# Revision 1.27  1995/06/20  17:04:28  esb
# added ipheader.fun
#
# Revision 1.26  1995/03/08  16:00:45  esb
# removed uip.tst.
#
# Revision 1.25  1994/10/25  16:36:14  esb
# added pseudoip.fun.
#
# Revision 1.24  1994/10/14  01:16:01  milnes
# Added "-" before cos.
#
# Revision 1.23  1994/10/04  21:04:45  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.22  1994/08/19  18:01:14  milnes
# Added emptyicmp.fun
#
# Revision 1.21  1994/08/15  19:54:41  milnes
# Added icmp.sig.
#
# Revision 1.20  1994/08/03  19:20:44  esb
# added ipmux.sig and iploop.fun.
#
# Revision 1.19  1994/08/02  20:31:33  esb
# removed ipaux.{sig,fun}, ipresolve.sig, and ipeth.fun.
#
# Revision 1.18  1994/06/15  20:47:40  milnes
# Installed subnet routing.
#
# Revision 1.17  1994/06/07  16:33:20  robby
#  Added a signature
#
# Revision 1.16  94/05/04  13:18:30  milnes
# Added ipoptions stuff.
# 
# Revision 1.15  1994/03/09  03:07:59  esb
# added ipfrag.tim.
#
# Revision 1.14  94/01/14  15:10:15  milnes
# Added ipfrag.
# 
# Revision 1.13  93/11/10  18:08:58  esb
# added ipresolve.sig, ipeth.fun, and ipreal.tst
# 
# Revision 1.12  1993/10/25  18:19:26  milnes
# Added ip.tim
#
# Revision 1.11  1993/10/25  17:39:06  milnes
# Added buildip.
#
# Revision 1.10  1993/10/19  17:34:19  milnes
# Added ipaux.sig and ipaux.fun
#
# Revision 1.9  1993/07/16  19:09:07  esb
# adapted to new module structure of this directory.
#
# Revision 1.8  1993/06/29  20:19:22  esb
# added ipserver.tst and ipclient.tst
#
# Revision 1.7  1993/06/29  19:20:00  esb
# added uip.tst
#
# Revision 1.6  1993/06/18  18:01:19  esb
# restructured so the first target is all: make populate
#
# Revision 1.5  1993/06/14  17:21:05  esb
# renamed files from ".sml" to ".fun", ".tst"
#
# Revision 1.4  1993/06/11  13:51:08  milnes
# makefile->make.
#
# Revision 1.3  93/06/11  13:45:52  milnes
# Makefile->makefile.
# 
# Revision 1.2  93/06/11  13:28:51  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := buildip.fun buildip.sig ip.sig icmp.sig ip.fun ipheader.fun \
ip.tim ip.tst ipheader.tst \
ipfrag.fun ipfrag.sig ipfrag.tst iploop.fun ipmux.sig ipmux.fun\
ipoption.sig ipoption.fun ipoption.tst \
iproute.fun iproute.sig pseudoip.fun pseudoip.tst \
ipicmp.tst ipping.tst icmpheader.sig icmpheader.fun \
hostid.str

DIRS := 

all: make populate

make: 
	-co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	-co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print 