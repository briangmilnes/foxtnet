#
# $Log: makefile,v $
# Revision 1.8  1995/03/07  20:27:23  esb
# eliminated debugtrace and printer.
#
# Revision 1.7  1994/11/10  16:30:44  milnes
# Added the debug trace files.
#
# Revision 1.6  1994/10/04  19:30:25  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.5  1994/06/16  16:31:31  danwang
# Updated to use functorized Fox_Basis
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
FILES := debug.fun debug.sig use.sml trace.sig trace.fun
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

