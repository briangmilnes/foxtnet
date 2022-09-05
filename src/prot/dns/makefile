# $Log: makefile,v $
# Revision 1.4  1996/03/04  21:27:06  esb
# added builddns.{sig,fun}
#
# Revision 1.3  1996/02/16  16:35:43  cline
# added dnslookup.sig and dnscache.fun
#
# Revision 1.2  1996/01/16  21:59:37  cline
# *** empty log message ***
#
# Revision 1.1  1995/06/20  17:24:23  esb
# Initial revision
#
#Revision 1.7  1995/03/07  23:50:13  esb
#got rid of various build functors and structures.
#
#Revision 1.6  1994/10/14  01:14:02  milnes
#Added "-" before cos.
#
#Revision 1.5  1994/10/04  21:29:17  esb
#changed /usr/cs/bin/csh to /bin/csh
#
#Revision 1.4  1994/07/04  20:29:08  esb
#added a blank in front of the log.
#
# Revision 1.3  1994/07/04  20:27:33  esb
# replaced .lnk with .str, changed test file to be in real_dns.
#
# Revision 1.2  1994/06/29  19:31:48  milnes
# Added dnstcp.str.
#
# Revision 1.1  1994/06/29  19:29:56  milnes
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := dns.sig dnsmessage.fun dnsproto.sig dnsproto.fun dnsproto.tst \
	 dnslookup.fun dnslookup.sig dnscache.fun builddns.sig builddns.fun
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
	find . -type f -name "#*#" -print -exec rm {} \;

