# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:39:18  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/17  21:34:58  robby
# fixedup some files
#
# Revision 1.1  94/08/15  17:35:50  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := xatoms.sml xprops.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

