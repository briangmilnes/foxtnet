#
# $Log: makefile,v $
# Revision 1.2  1994/10/04  21:38:10  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.1  1994/08/25  10:24:41  robby
# Initial revision
#
# Revision 1.2  1994/07/14  22:58:21  robby
# swapped around some files
#
# Revision 1.1  94/07/14  20:59:57  robby
# Initial revision
# 



SHELL := /bin/csh
PWD := $(shell pwd)
FILES := kerberos_error.fun kerberos_error.sig kerberos_linearize.fun kerberos_linearize.sig kerberos_misc.fun kerberos_misc.sig kerberos_tag.fun kerberos_tag.sig use.sml

all: make populate

make: 
	co makefile

populate:
	co ${FILES}

clean: 
	find . -type f -name "*~"  -print -exec rm {} \;
	find . -type f -name "#*#" -print -exec rm {} \;

