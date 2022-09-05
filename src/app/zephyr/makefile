#
#$Log: makefile,v $
#Revision 1.5  1994/10/04  21:17:31  esb
#changed /usr/cs/bin/csh to /bin/csh
#
#Revision 1.4  1994/09/03  21:26:43  danwang
#Updated for new zephyr functors.
#
#Revision 1.4  1994/09/03  21:22:03  danwang
#Update for new zephyr functors.
#
# Revision 1.3  1994/07/14  16:25:14  milnes
#  Added .sig, .fun, .str and deleted .sml.
#
# Revision 1.2  1994/01/06  13:57:23  esb
# added zwrite.sml
#
# Revision 1.1  1993/10/20  14:19:24  cline
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml zwrite.sig zwrite.fun zwrite.str zephyr_lower.sig zephyr.sig zephyr_client.sig zephyr_lower.fun zephyr.fun zephyr_client.fun zclient.str
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

