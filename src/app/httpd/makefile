# 
#$Log: makefile,v $
# Revision 1.4  1995/03/15  19:04:00  cstone
# Only checks out files when RCS is newer.
#
# Revision 1.3  1995/03/08  20:33:32  cstone
# Fixed filename typo
#
# Revision 1.2  1995/03/08  20:25:13  cstone
# added new filenames.
#
# Revision 1.1  1995/01/25  22:49:11  cstone
# Initial revision
#

SHELL := /bin/csh
COMMA := ,
PWD := $(shell pwd)
FILES := boast.sml foxnetcpipeth.str httpd.fun httpd.sig httpd.str inferior-sml.sml makeserver.sig makeserver.fun safe.sml safe_foxnet.sml tcpstack.sig talk.sml use.sml
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

