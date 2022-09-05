# 
#$Log: makefile,v $
# Revision 1.2  1994/10/04  21:19:11  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/08/28  21:59:25  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml 
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

