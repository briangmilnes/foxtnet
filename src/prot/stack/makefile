# 
#$Log: makefile,v $
# Revision 1.5  1996/06/11  03:35:02  esb
# removed demo.str and simple.fun
#
# Revision 1.4  1995/09/18  19:31:10  cline
# added segment.fun and stream.fun
#
# Revision 1.3  1995/07/05  17:51:58  cline
# added transport.fun
#
# Revision 1.2  1995/06/21  15:00:05  cline
# changed testaddresses.sig to testaddresses.str
#
# Revision 1.1  1995/06/20  17:19:46  esb
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := buildtcpipeth.fun tcpipeth.sig tcpipeth.str \
	 transport.sig transport.fun network.sig testaddress.str \
	 segment.fun stream.fun
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
	find . -type f -name ".d.*" -print -exec rm {} \;

