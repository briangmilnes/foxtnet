# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:39:00  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/17  21:35:03  robby
# fixedup some files
#
# Revision 1.1  94/08/15  18:15:23  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := drawing.sml font.sml interact.sml abstract.sml exene-base.sml geometry.sml std-cursor.sml abstract.sml.0 exene-win.sml iccc.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

