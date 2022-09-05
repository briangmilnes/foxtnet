# 
#$Log: makefile,v $
# Revision 1.13  1996/09/17  16:36:16  esb
# added hello
#
# Revision 1.12  1996/09/17  15:50:35  cline
# added interrupt.str
#
# Revision 1.11  1996/05/14  21:15:30  esb
# added inetd
#
# Revision 1.10  1994/10/04  19:30:27  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.9  1994/08/28  21:58:26  robby
# added gopher
#
# Revision 1.8  1994/08/28  18:52:45  robby
# added finger
#
# Revision 1.7  1994/08/25  11:38:04  cokasaki
# Added ftp.
#
# Revision 1.6  94/05/10  13:06:50  milnes
# Removed ping as it is in a subdirectory.
# 
# Revision 1.5  1994/05/04  19:16:13  milnes
# Added ping.
#
# Revision 1.4  1994/02/21  11:43:10  esb
# re-added the subdirectories.
#
# Revision 1.3  94/01/14  17:43:49  cline
# *** empty log message ***
# 
# Revision 1.2  1993/12/15  13:46:38  cline
# added des
#
# Revision 1.1  1993/10/20  14:01:45  cline
# Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml interrupt.str
DIRS := telnet inetd des kerberos zephyr ping ftp finger gopher hello

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

