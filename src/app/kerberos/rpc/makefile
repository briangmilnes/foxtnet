# 
#$Log: makefile,v $
# Revision 1.2  1994/10/04  21:38:04  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/08/25  12:13:37  robby
# Initial revision
#
# Revision 1.2  1994/07/14  20:33:50  robby
# moved around stuff from this directory to dirs
#
# Revision 1.1  94/07/13  19:37:59  robby
# Initial revision
# 

SHELL := /bin/csh
PWD := $(shell pwd)
FILES :=kerberos_rpc.sig kerberos_rpc.fun krbrealm.sig krbrealm.fun use.sml bytearray_rpc.sig bytearray_rpc.fun
 

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

