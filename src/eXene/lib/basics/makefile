# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:34:28  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/17  21:34:55  robby
# fixedup some files
#
# Revision 1.1  94/08/15  18:05:14  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := geom.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

