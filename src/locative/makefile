#
# $Log: makefile,v $
# Revision 1.2  1994/10/04  17:37:40  esb
# replaced /usr/cs/bin/csh by /bin/csh
#
# Revision 1.1  1994/08/25  23:08:07  milnes
# Initial revision
#
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml locative.fun locative.sig use.sml
DIRS := dynarray 

all: make populate

make: 
	co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

