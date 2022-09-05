# 
#$Log: makefile,v $
# Revision 1.9  1994/10/04  21:18:15  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.8  1994/08/25  11:00:30  robby
# added linearize subdir
#
# Revision 1.7  1994/07/13  19:18:53  robby
# added des_prime.fun, des_prime.sig
#
# Revision 1.6  94/06/17  13:45:43  robby
# added old_des.fun old_des.sig
# 
# Revision 1.5  94/06/14  14:06:27  robby
# changed block.sml to block.fun
# 
# Revision 1.4  94/05/25  13:41:10  robby
# added des.sml.
# 
# Revision 1.3  94/02/15  18:45:06  esb
# added struct.txt.
# 
# Revision 1.2  94/02/15  18:05:49  esb
# added the new files.
# 
# Revision 1.1  93/12/10  18:44:49  cline
# Initial revision
# 
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml des.sig des.fun des.sml block.sig key.sig block.fun text.sig text.sml aux.sig aux.sml util.sig util.sml struct.txt old_des.fun old_des.sig des_prime.sig des_prime.fun
DIRS := linearize

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

