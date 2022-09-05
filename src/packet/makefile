#
#$Log: makefile,v $
# Revision 1.7  1994/06/16  16:34:42  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.6  1994/04/04  13:31:08  esb
# added receive_packet.tst
#
# Revision 1.5  93/12/23  22:58:27  esb
# added send_packet.tst.
# 
# Revision 1.4  1993/06/11  13:46:13  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:29:10  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /usr/cs/bin/csh
PWD := $(shell pwd)
FILES := compare_byte_arrays.fun compare_message.sig compare_receive_packet.fun receive_packet.fun receive_packet.sig  receive_packet.tst send_packet.fun send_packet.sig send_packet.tst use.sml
DIRS := 

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

