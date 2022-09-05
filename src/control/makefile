#
#$Log: makefile,v $
# Revision 1.12  1995/08/16  21:27:31  esb
# added writeonce variables.
#
# Revision 1.11  1995/06/20  16:48:24  esb
# added the semaphore files
#
# Revision 1.10  1995/03/10  03:45:27  esb
# eliminated dispatcher.
#
# Revision 1.9  1994/10/14  01:16:43  milnes
# Added "-" before cos.
#
# Revision 1.8  1994/10/04  19:30:19  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.7  1994/06/16  16:29:59  danwang
# Updated to use functorized Fox_Basis
#
# Revision 1.6  1994/02/08  16:41:18  esb
# added coro.tim.
#
# Revision 1.5  93/09/02  15:17:10  esb
# added event and pipe, removed the misnamed queue.
# 
# Revision 1.4  1993/06/11  13:45:46  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:28:44  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := coro.sig coro.fun coro.tst coro.tim event.sig event.fun event.tst pipe.sig pipe.fun pipe.tst pipe.tim sem.sig sem.fun writeonce.sig writeonce.fun writeonce.tst
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

