#
# $Log: makefile,v $
#Revision 1.2  1994/10/04  21:17:11  esb
#changed /usr/cs/bin/csh to /bin/csh
#
#Revision 1.1  1994/08/25  23:12:30  milnes
#Initial revision
#
#


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml dxs.sig dxs.fun dxbyten.fun
DIRS := 

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

