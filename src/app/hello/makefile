# 
#$Log: makefile,v $
# Revision 1.1  1996/09/17  16:46:30  esb
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := hello.fun hello.str 
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

