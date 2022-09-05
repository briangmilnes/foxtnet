# 
#$Log: makefile,v $
# Revision 1.3  1995/03/08  15:53:59  esb
# fixed a bug.
#
# Revision 1.2  1995/03/08  15:42:13  esb
# added system-dependent directories.
#
# Revision 1.1  1994/10/19  23:18:06  milnes
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml addressing.sig addressing.fun addressing.tst osaddressing.sig machaddressing.fun osfaddressing.fun
DIRS := alpha_osf1 alpha_osf20 pmax_mach

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

