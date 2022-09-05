# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:38:54  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/26  14:51:59  robby
# removed use.sml
#
# Revision 1.1  1994/08/15  18:17:47  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := hash-xid.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

