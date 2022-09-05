#
#$Log: makefile,v $
# Revision 1.14  1995/06/28  19:47:20  cline
# added udpheader.fun
#
# Revision 1.13  1995/03/08  16:00:55  esb
# removed uudp.tst.
#
# Revision 1.12  1994/10/14  01:15:11  milnes
# Added "-" before cos.
#
# Revision 1.11  1994/10/04  21:10:46  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.10  1994/06/07  16:36:42  robby
# Added buildudp.sig
#
# Revision 1.9  94/01/17  13:23:10  cline
# remover userudp.fun, added uudp.tst.
# 
# Revision 1.8  1993/12/21  13:49:02  cline
# added udp.tim
#
# Revision 1.7  1993/10/25  18:13:12  milnes
# Added udp.tst.
#
# Revision 1.6  1993/10/25  17:39:47  milnes
# Add buildudp.
#
# Revision 1.5  1993/10/21  20:26:53  milnes
# Turned udp.sml into udp.fun and the same for userudp.
#
# Revision 1.4  1993/06/11  13:45:59  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:28:57  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := udp.tst udp.sig udp.fun buildudp.sig buildudp.fun udp.tim \
	udpheader.fun
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

