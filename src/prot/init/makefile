# 
#$Log: makefile,v $
# Revision 1.5  1996/04/22  16:54:15  esb
# added ultrixinit.fun
#
# Revision 1.4  1996/04/11  16:49:15  derby
# Added OSF1_init.fun
#
# Revision 1.3  1996/04/11  16:25:22  derby
# added constants.str
#
# Revision 1.2  1996/02/27  18:33:42  esb
# renamed hostname_ip.str  to hostnameip.str
#
# Revision 1.1  1996/02/26  15:27:33  cline
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := init.fun hostnameip.str constants.str osf1init.fun ultrixinit.fun
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

