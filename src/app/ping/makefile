# 
#$Log: makefile,v $
# Revision 1.2  1994/10/04  21:18:38  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/05/10  13:07:47  milnes
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml ping.sig ping.fun ping.str 
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

