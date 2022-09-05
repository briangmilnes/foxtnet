# 
#$Log: makefile,v $
# Revision 1.2  1994/10/04  21:16:11  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/08/17  21:39:25  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml load-util

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

