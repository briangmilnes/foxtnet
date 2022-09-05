# 
#$Log: makefile,v $
# Revision 1.2  1994/10/04  21:19:39  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/08/25  11:47:43  cokasaki
# Initial revision
#
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml ftp.sig ftp.fun ftp.str ftp_ui.sig ftp_ui.fun ftp_ui.str
DIRS := 

all: make populate

make: 
	co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

