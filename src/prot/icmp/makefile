#
#$Log: makefile,v $
#Revision 1.12  1995/09/26  16:30:40  esb
#eliminated the now-unnecessary files.
#
#Revision 1.11  1995/09/24  18:03:16  cstone
#Added icmpheader.sig
#
#Revision 1.10  1995/08/08  20:24:24  esb
#added {ip,}icmp.sig and icmpheader.*
#
#Revision 1.9  1994/10/14  01:13:24  milnes
#Added "-" before cos.
#
#Revision 1.8  1994/10/04  21:13:03  esb
#changed /usr/cs/bin/csh to /bin/csh
#
#Revision 1.7  1994/09/19  17:38:57  esb
#added use.sml
#
#Revision 1.6  1994/09/14  15:28:38  milnes
#Updated to add iplocalstatus.tst and noticed that this was quite out of
#sync with the use file so I made them consistent.
#
#Revision 1.5  1994/08/15  19:53:17  milnes
#Removed icmp.sig, ip has it now.
#
#Revision 1.4  1994/06/07  11:55:31  esb
#added icmpecho.str
#
#Revision 1.3  1994/06/06  23:24:53  esb
#added buildicmp.fun
#
#Revision 1.2  1994/06/06  21:06:06  esb
#added use.sml
#
#Revision 1.1  1994/05/23  14:09:03  milnes
#Initial revision
#
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := icmpheader.sig icmpheader.fun icmpheader.tst
DIRS := 

all: make populate

make: 
	-co makefile
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake make;)

populate:
	-co ${FILES}
	$(foreach dir,${DIRS}, cd ${PWD}/${dir}; gmake populate;)

clean: 
	find . -type f -name *~  -print -exec rm {} \;
	find . -type f -name #*# -print -exec rm {} \;
