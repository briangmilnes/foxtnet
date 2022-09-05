#
#$Log: makefile,v $
# Revision 1.2  1995/03/08  18:13:31  esb
# added sources.sc
#
# Revision 1.1  1994/10/20  17:57:49  cline
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml buildethdev.fun ethdev.fun packetfilter.fun packetfilter.sig sources.sc
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

