# 
#$Log: makefile,v $
# Revision 1.2  1996/07/05  17:40:13  esb
# adapted to latest revision.
#
# Revision 1.1  1996/05/19  17:07:50  esb
# Initial revision
#
#

SHELL := /bin/csh
COMMA := ,
PWD := $(shell pwd)
FILES := telnetpty.sig telnetpty.fun \
         telnetutil.sig telnetutil.fun \
	 telnetopt.sig telnetopt.str \
	 telnetdata.sig telnetdata.fun \
	 telnetio.sig telnetio.fun \
	 telnet.sig telnet.fun \
	 telnetclient.fun telnetclient.str \
	 telnetserver.fun telnetserver.str
DIRS := 

all: make populate

make: 
	co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

${FILES}: %: RCS/%${COMMA}v
	co $@

populate: ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

