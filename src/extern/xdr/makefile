# 
#$Log: makefile,v $
# Revision 1.2  1995/01/26  00:30:24  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/10/14  11:59:57  kcchiang
# Initial revision
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := use.sml xdr.sig xdr.fun xdr.tst wrap.sig wrap.fun sunrpc_udp.str sunrpc_server.tst sunrpc_server.sig sunrpc_server.fun sunrpc.sig sunrpc.fun build_sunrpc_udp.fun build_sunrpc.sig

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

