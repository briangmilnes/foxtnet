# 
#$Log: makefile,v $
# Revision 1.2  1996/09/17  16:50:33  esb
# added inetd.build
#
# Revision 1.1  1996/05/14  21:14:19  esb
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml inetd.fun inetd.str inetd.build
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

