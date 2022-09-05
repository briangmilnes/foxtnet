# 
#$Log: makefile,v $
# Revision 1.3  1994/10/04  21:16:31  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.2  1994/08/15  20:54:31  robby
# fixed tex into text
#
# Revision 1.1  94/08/15  19:29:24  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml load-w widgets.sc
DIRS := basics composite simple text

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

