# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:38:48  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/26  14:50:13  robby
# removed use.sml
#
# Revision 1.1  1994/08/15  19:26:04  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := font-server.sml pixmap.sml color-server-sig.sml gc-server.sml tile.sml color-server.sml hash-window.sml toplevel-win.sml cursor.sml image.sml window-env.sml display.sml keymap.sml window-sig.sml draw-master.sml keysym-trans.sml window.sml draw-types.sml keysym.sml winreg.sml draw.sml pen-rep.sml font-base.sml pen.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

