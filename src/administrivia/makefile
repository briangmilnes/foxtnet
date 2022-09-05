#
#$Log: makefile,v $
# Revision 1.10  1996/06/19  22:47:43  esb
# added ethernet-ip
#
# Revision 1.9  1994/10/04  19:30:21  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.8  1994/03/02  21:43:51  esb
# removed codequality.text and lacunae.text (obsolete).
#
# Revision 1.7  1993/06/11  13:46:10  milnes
# Makefile->makefile.
#
# Revision 1.6  93/06/11  13:29:07  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := README header.c header.sml ethernet-ip
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

