# 
#$Log: makefile,v $
# Revision 1.1  1995/06/20  17:27:21  esb
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := timeprotocol.sig timeprotocol.fun benchmark.sig benchmark.fun \
	 proto.tim tests.tim timingtest.sig timingtest.fun 
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

