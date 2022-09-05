#
#$Log: makefile,v $
# Revision 1.8  1994/10/14  01:17:15  milnes
# Added "-" before cos.
#
# Revision 1.7  1994/10/04  19:30:02  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.6  1994/06/16  16:48:59  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.5  1993/09/02  15:27:01  esb
# replaced test_functor.sig by teststructure.sig.
#
# Revision 1.4  1993/06/11  13:45:30  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:28:27  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := test.fun test.sig test.tst teststructure.sig use.sml
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

