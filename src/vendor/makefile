#
#$Log: makefile,v $
# Revision 1.15  1996/01/15  18:26:14  cline
# added bits.str
#
# Revision 1.14  1995/03/08  18:18:05  esb
# added word.tst
#
# Revision 1.13  1995/03/08  15:52:38  esb
# simplified to only have one version.
#
# Revision 1.12  1994/11/09  19:10:29  cline
# added word.106.fun
#
# Revision 1.11  1994/11/09  19:02:20  cline
# added vendor.106.fun
#
# Revision 1.10  1994/10/14  01:16:59  milnes
# Added "-" before cos.
#
# Revision 1.9  1994/10/04  19:30:17  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.8  1994/09/30  16:14:16  esb
# added 105-specific versions and word
#
# Revision 1.7  1994/06/16  16:53:40  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.6  1993/10/25  17:39:03  esb
# added byte.str.
#
# Revision 1.5  1993/06/18  17:23:59  esb
# got rid of the vendor functor
#
# Revision 1.4  1993/06/11  13:45:38  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:28:35  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml vendor.sig vendor.fun word.sig word.fun word.tst bits.str
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

