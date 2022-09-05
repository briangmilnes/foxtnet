#
#$Log: makefile,v $
# Revision 1.5  1995/03/08  18:24:36  esb
# added sources.sc
#
# Revision 1.4  1995/03/07  23:53:58  esb
# updated to 1.07.
#
# Revision 1.3  1995/02/11  06:50:35  esb
# added deviceUser.
#
# Revision 1.2  1994/10/21  20:33:59  milnes
# Removed ethdev.str.
#
# Revision 1.1  1994/10/20  17:56:54  cline
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml buildethdev.fun deviceUser.sig deviceUser.sml mach_portUser.sml ethdev.fun mach_portUser.sig sys.sig sys.sml sources.sc
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

