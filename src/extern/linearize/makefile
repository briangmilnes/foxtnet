#
# $Log: makefile,v $
#Revision 1.6  1995/03/13  04:02:23  cstone
#Make sure RCS info was all commented out.
#
# Revision 1.5  1994/10/04  21:16:52  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.4  1994/08/25  23:47:26  robby
# added buildlinearize.fun and .sig
#
# Revision 1.3  94/08/25  23:04:02  robby
# deleted des and kerberos subdirs
# 
# Revision 1.2  1994/07/14  22:57:55  robby
# swapped around somefiles
#
# Revision 1.1  94/07/14  20:30:10  robby
# Initial revision
# 


SHELL := /bin/csh
PWD := $(shell pwd)
FILES := bytearray.fun bytearray.sig cstring.fun int.fun int.sig linearize.sig pair.sig pair.fun string.sig ubyte.fun ubyte.sig use.sml swap.sig swap.fun buildlinearize.fun buildlinearize.sig
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

