#
#$Log: makefile,v $
#Revision 1.12  1994/10/04  21:17:55  esb
#changed /usr/cs/bin/csh to /bin/csh
#
#Revision 1.11  1994/08/25  11:03:24  robby
#added linearize and rpc subdirs
#
#Revision 1.10  1994/08/08  14:53:18  danwang
#Added buildkerberos.fun.
#
#Revision 1.9  1994/07/13  19:37:17  robby
#updated to new kerberos
#
#Revision 1.7  93/12/27  20:51:17  cline
#added swap.{sig,fun}
#
# Revision 1.6  1993/12/27  20:22:40  cline
# Deleted krbfiles, and added krbrealm and tktfile [.sig & .fun]
#
# Revision 1.5  1993/12/21  15:17:34  cline
# Added forge.sig.
#
# Revision 1.4  1993/12/15  13:40:24  cline
# added lots of files.  Kinit now runs.
#
# Revision 1.3  1993/10/25  18:02:00  cline
# *** empty log message ***
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES:= use.sml kerberos.sig kerberos.fun buildkerberos.fun
DIRS := linearize rpc

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
