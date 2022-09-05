# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:39:24  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/17  21:34:57  robby
# fixedup some files
#
# Revision 1.1  94/08/15  18:07:23  robby
# Initial revision
# 
# Revision 1.1  1994/08/15  17:29:38  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := rounded-rect-sig.sml spline.sml ellipse-sig.sml rounded-rect.sml ellipse.sml spline-sig.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

