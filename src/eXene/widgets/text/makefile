# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:38:21  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/22  14:28:37  robby
# added display-text.sml
#
# Revision 1.1  1994/08/15  20:54:23  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := text-keysym-trans.sml vtty-sig.sml extstr.sml text-widget-sig.sml vtty.sml field-edit.sml text-widget.sml str-edit.sml txtwin.sml display-text.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

