# 
#$Log: makefile,v $
# Revision 1.8  1994/12/01  18:46:07  esb
# removed extern.sig, which is defined in ..../extern.
#
# Revision 1.7  1994/10/14  01:13:40  milnes
# Added "-" before cos.
#
# Revision 1.6  1994/10/04  21:13:23  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.5  1994/08/26  14:09:40  robby
# removed kerberos
#
# Revision 1.4  1994/07/13  19:37:44  robby
# added kerberos subdir
#
# Revision 1.3  94/07/07  17:00:36  robby
# added extern.sig
# 
# Revision 1.2  94/06/29  19:16:36  robby
# added dns directory
# 
# Revision 1.1  94/06/25  20:41:47  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml rpc.sig
DIRS := dns

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

