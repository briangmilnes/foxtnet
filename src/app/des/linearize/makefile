#
# $Log: makefile,v $
# Revision 1.2  1994/10/04  21:37:58  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/08/25  12:15:19  robby
# Initial revision
#
# Revision 1.1  1994/07/14  20:30:47  robby
# Initial revision
#


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := des_linearize.fun des_linearize.sig use.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

