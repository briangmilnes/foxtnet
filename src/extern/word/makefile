#
# $Log: makefile,v $
# Revision 1.2  1995/11/12  16:54:37  esb
# removed externN.fun, replaced it with externkey.fun.
#
# Revision 1.1  1995/06/20  17:44:17  esb
# Initial revision
#
# Revision 1.5  1995/06/20  17:42:00  esb
# added word sub-directory
#
# Revision 1.4  1995/02/09  19:49:12  esb
# added xdr.
#
# Revision 1.3  1994/10/04  19:30:30  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/25  23:05:48  robby
# added dynarray
#
# Revision 1.1  1994/07/14  20:30:17  robby
# Initial revision
#


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := externkey.sig externkey.fun
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

