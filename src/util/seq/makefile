#
# $Log: makefile,v $
# Revision 1.3  1995/11/12  16:49:15  esb
# renamed seq -> sequence and monoseq -> seq.
#
# Revision 1.2  1995/06/29  18:21:52  esb
# added word48array.str.
#
# Revision 1.1  1995/06/20  17:35:35  esb
# Initial revision
#
# 


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := sequence.sig seq.sig sequence.fun seq.fun compare.fun wordarray.str wordarray.tst oddwordarray.sig oddwordarray.str
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

