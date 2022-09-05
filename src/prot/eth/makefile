# 
# $Log: makefile,v $
# Revision 1.10  1995/06/20  16:56:16  esb
# added ethheader.fun
#
# Revision 1.9  1994/10/14  01:16:20  milnes
# Added "-" before cos.
#
# Revision 1.8  1994/10/04  21:04:05  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.7  1994/06/06  20:49:08  esb
# added buildeth.sig
#
# Revision 1.6  93/10/25  17:38:30  milnes
# Added buildeth.
# 
# Revision 1.5  1993/09/10  18:25:22  milnes
# Added eth.tim.
#
# Revision 1.4  1993/06/11  13:50:36  milnes
# makefile->make.
#
# Revision 1.3  93/06/11  13:45:50  milnes
# Makefile->makefile.
# 
# Revision 1.2  93/06/11  13:28:48  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := eth.sig eth.fun ethheader.fun eth.tst eth.tim buildeth.sig buildeth.fun
DIRS := 

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

make: 
	-co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	-co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)
