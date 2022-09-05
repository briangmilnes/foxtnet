# $Log: makefile,v $
# Revision 1.4  1995/09/18  19:29:16  cline
# removed segment.fun
#
# Revision 1.3  1995/06/21  18:13:21  esb
# added ethall.tst
#
# Revision 1.2  1995/06/21  15:02:00  cline
# *** empty log message ***
#
# Revision 1.1  1995/06/20  20:35:38  cline
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := constant.fun packetvalue.fun sendreceive.fun void.fun multiplex.fun \
	address.fun ethall.fun ethall.tst arpall.fun length.fun \
	minlength.fun

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