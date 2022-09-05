# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:38:27  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/28  20:40:52  robby
# added button-list
#
# Revision 1.1  1994/08/15  19:28:10  robby
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := framed-button.sml scrollbar.sml button-ctrl.sml framed-label.sml slider-view.sml button-view.sml framed-toggle.sml slider.sml button.sml label-view.sml text-list.sml canvas-sig.sml label.sml toggle-ctrl.sml canvas.sml toggle.sml color-rect.sml scroll-view.sml divider.sml scrollbar-sig.sml button-list.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

