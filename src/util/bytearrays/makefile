#
#$Log: makefile,v $
# Revision 1.16  1996/04/30  19:35:31  esb
# removed the dynarrays.
#
# Revision 1.15  1994/10/14  01:13:09  milnes
# Added "-" before cos.
#
# Revision 1.14  1994/10/04  21:13:47  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.13  1994/08/02  20:18:04  esb
# added dynarray.* and compare*
#
# Revision 1.12  1994/07/04  18:16:26  esb
# added create.sig create.fun
#
# Revision 1.11  94/06/27  17:11:16  robby
# added access.tst
# 
# Revision 1.10  94/06/16  16:50:23  danwang
# Updated for functorized Fox_Basis
# 
# Revision 1.9  1993/10/29  04:43:29  esb
# added the copy functions.
#
# Revision 1.8  1993/09/02  15:35:57  esb
# added format.sig/fun/str and access.str
#
# Revision 1.7  1993/08/30  15:56:56  milnes
# Added access.sig, fun and str.
#
# Revision 1.6  1993/06/11  14:34:20  milnes
# Changed acces to access.
#
# Revision 1.5  93/06/11  13:46:06  milnes
# Makefile->makefile.
# 
# Revision 1.4  93/06/11  13:29:02  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml comparearray.sig comparebyte.fun comparedyn.fun copy.sig copy.fun copy.tst copy.tim create.sig create.fun format.sig format.fun access.sig access.fun access.tst
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

