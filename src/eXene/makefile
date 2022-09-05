# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  19:30:22  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/26  14:55:43  robby
# added util-lib
#
# Revision 1.1  1994/08/15  19:22:50  robby
# Initial revision
#
# Revision 1.1  1994/08/15  19:21:13  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml
DIRS := lib widgets util-lib

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

