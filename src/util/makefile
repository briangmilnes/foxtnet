#
#$Log: makefile,v $
#Revision 1.27  1996/04/30  20:22:13  esb
#eliminated duplicates, lifo, trie.
#
#Revision 1.26  1996/04/30  19:41:04  esb
#eliminated lifo
#
#Revision 1.25  1995/06/20  17:36:08  esb
#added sub-directory seq
#
#Revision 1.24  1995/02/13  22:58:51  esb
#added string-to-word.sml.
#
#Revision 1.23  1994/10/26  17:59:31  robby
#added parsing subdirectory
#
#Revision 1.22  1994/10/19  22:58:12  esb
#added environ.sig and environ.fun.
#
#Revision 1.21  1994/10/14  01:12:32  milnes
#Added "-" before cos.
#
#Revision 1.20  1994/10/11  17:43:32  esb
#added tree.tst
#
#Revision 1.19  1994/10/04  19:30:20  esb
#changed /usr/cs/bin/csh to /bin/csh
#
#Revision 1.18  1994/09/29  19:47:02  robby
#added tree.fun and tree.sig
#
#Revision 1.17  1994/07/01  03:58:38  danwang
#Added tire to Fox_Basis.
#
#Revision 1.16  1994/06/16  16:51:18  danwang
#Updated for functorized Fox_Basis
#
#Revision 1.15  1994/02/21  00:16:10  esb
#added order.
#
#Revision 1.14  94/02/15  15:25:42  jgmorris
#Added use.sml.
#
#Revision 1.13  94/01/18  16:38:21  milnes
#Added the timingboard directory.
#
#Revision 1.12  1994/01/14  11:20:15  milnes
#Updated to reflect what we are actually loading.
#
#Revision 1.11  93/11/05  20:39:07  cline
#added unaligned.{sig,sml}
#
#Revision 1.10  1993/11/02  22:35:55  esb
#added checksum.tim.
#
#Revision 1.9  1993/10/06  01:22:26  esb
#added store.tst.
#
# Revision 1.8  1993/07/23  15:45:38  esb
# removed array.sig and array.sml, which nobody was using.
#
# Revision 1.7  1993/07/20  14:29:45  esb
# added priority queue data structure files.
#
# Revision 1.6  1993/07/10  04:05:09  esb
# removed the timing functions, which are now in ../time
#
# Revision 1.5  1993/06/18  14:51:03  esb
# eliminated some unused files
#
# Revision 1.4  1993/06/11  13:46:08  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:29:04  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := store.sig store.fun fifo.sig fifo.fun priority.sig priority.fun checksum.sig checksum.fun tabulate.sml unaligned.sig unaligned.sml deq.sig deq.fun store.tst fifo.tst priority.tst checksum.tst checksum.tim use.sml tree.fun tree.sig tree.tst environ.sig environ.fun string-to-word.sml

DIRS := bytearrays timingboard order parsing seq

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

