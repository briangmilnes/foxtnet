# 
#$Log: makefile,v $
# Revision 1.5  1997/03/10  19:06:18  esb
# removed user.sig user.fun and rpc.sig, which are obsolete
#
# Revision 1.4  96/04/30  20:28:59  esb
# removed conn.tst.
# 
# Revision 1.3  1995/08/08  20:26:29  esb
# removed proto.alt
#
# Revision 1.2  1995/08/08  20:24:47  esb
# added proto.alt
#
# Revision 1.1  1995/06/20  17:11:14  esb
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := conn.fun proto.sig protoexn.fun protoexn.sig protoextern.fun protoextern.tst prototype.sig
	 
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

