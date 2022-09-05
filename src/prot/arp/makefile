#
#$Log: makefile,v $
# Revision 1.7  1996/04/30  19:15:18  esb
# eliminated arpreal.tst
#
# Revision 1.6  1995/06/20  16:58:49  esb
# added arpheader.fun
#
# Revision 1.5  1994/10/14  01:14:26  milnes
# Added "-" before cos.
#
# Revision 1.4  1994/10/04  21:12:38  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.3  1994/08/02  20:29:01  esb
# removed arpresolve.{sig,fun}
#
# Revision 1.2  1993/10/27  01:19:26  esb
# added arpresolve.
#
# Revision 1.1  1993/08/24  21:20:29  esb
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := arp.sig arp.fun arpheader.fun arpeth.fun arp.tst
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
