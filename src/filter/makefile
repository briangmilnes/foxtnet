#
#$Log: makefile,v $
# Revision 1.9  1995/01/14  02:24:20  esb
# fixed the filter, not tested yet.
#
# Revision 1.8  1994/12/21  20:36:49  milnes
# Updated for duplicate addressing.
#
# Revision 1.7  1994/10/14  01:17:27  milnes
# Added "-" before cos.
#
# Revision 1.6  1994/10/04  19:29:49  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.5  1994/06/16  16:33:06  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.4  1993/06/11  13:45:14  milnes
# Makefile->makefile
#
# Revision 1.3  93/06/11  13:28:09  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := filter.fun filter.sig filter.fun filter.tst use.sml
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

