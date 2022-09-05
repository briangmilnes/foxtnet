#
# $Log: makefile,v $
# Revision 1.4  1994/10/14  01:12:56  milnes
# Added "-" before cos.
#
# Revision 1.3  1994/10/04  21:14:08  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/06/16  16:52:21  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.1  1994/01/14  12:24:03  milnes
# Initial revision
#
# 


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := timingboard.sig timingboard.fun  timingboard.tim use.sml
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

