# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:39:06  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/17  21:35:01  robby
# fixedup some files
#
# Revision 1.1  94/08/15  12:46:45  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := xevttypes.sml xsendevt.sml keybut.sml xio.sml xserver-addr.sml xcvtfns.sml xprottypes.sml xshutdown.sml xdisplay.sml xreply.sml xwin.sml xerrors.sml xrequest.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

