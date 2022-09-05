#
#$Log: makefile,v $
# Revision 1.4  1994/10/14  01:11:52  milnes
# Added - before co.
#
# Revision 1.3  1994/10/04  21:14:30  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/06/16  16:53:09  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.1  1994/02/18  09:32:04  esb
# Initial revision
#
# Revision 1.9  1993/10/29  04:43:29  esb
# added the copy functions.
#
# Revision 1.8  1993/09/02  15:35:57  esb
# added format.sig/fun/str and access.str
#
# Revision 1.7  1993/08/30  15:56:56  milnes
# Added access.sig, fun and str.
#
# Revision 1.6  1993/06/11  14:34:20  milnes
# Changed acces to access.
#
# Revision 1.5  93/06/11  13:46:06  milnes
# Makefile->makefile.
# 
# Revision 1.4  93/06/11  13:29:02  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml order.sig order.fun order.tst order.tim
DIRS := 

all: make populate

make: 
	-co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	-co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

