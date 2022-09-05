#
# $Log: makefile,v $
# Revision 1.6  1995/02/13  23:01:34  esb
# added utils.sig.
#
# Revision 1.5  1994/10/25  16:12:33  pscheng
# correct more typos
#
# Revision 1.4  94/10/25  15:54:17  pscheng
# fixed typos
# 
# Revision 1.3  94/10/25  15:51:38  pscheng
# added use.sml to list of files
# 
# Revision 1.2  94/10/21  14:08:16  milnes
# Added log.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := base.sig base.sml Stream.sig stream.sml Pos.sig pos.sml ParserBase.sig cont.sml Parser.sig parser.sml Input.sig input.sml link.sml utils.str use.sml utils.sig
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

