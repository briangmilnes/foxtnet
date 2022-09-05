#
# $Log: makefile,v $
# Revision 1.4  1995/06/20  17:41:19  esb
# eliminated basis.fun.
#
# Revision 1.3  1994/10/04  19:30:26  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/06/16  16:07:25  danwang
# Updated to use functorized Fox_Basis
#
# Revision 1.1  1993/08/27  20:32:46  esb
# Initial revision
#
# Revision 1.4  1993/06/11  13:46:20  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:29:18  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := basis.sig basis.str
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

