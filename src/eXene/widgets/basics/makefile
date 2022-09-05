# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:38:40  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/19  14:39:36  robby
# Added display.sml
#
# Revision 1.1  1994/08/15  19:27:43  robby
# Initial revision
#
# Revision 1.1  1994/08/15  19:02:42  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := dialog.sml router.sml shell.sml widget.sml display.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

