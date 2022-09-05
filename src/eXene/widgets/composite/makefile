# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:38:33  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/22  21:34:58  robby
# added mac-popup-menu.sml
#
# Revision 1.1  1994/08/15  19:27:59  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := index.sml shape.sml background.sml menu-button.sml simple-menu.sml box-layout.sml pile.sml viewport-sig.sml scrollbar-layout.sml viewport.sml box.sml scrollport-sig.sml widget-set.sml frame.sml scrollport.sml mac-popup-menu.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

