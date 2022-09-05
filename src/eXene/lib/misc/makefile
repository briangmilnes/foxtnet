# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:39:12  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/17  21:35:00  robby
# fixedup some files
#
# Revision 1.1  94/08/15  18:04:17  robby
# Initial revision
# 
# Revision 1.1  1994/08/15  12:38:56  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := exene-version.sml xdebug.sml xvalid.sml xprint.sml mlx-err.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

