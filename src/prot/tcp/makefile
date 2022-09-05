#
#$Log: makefile,v $
# Revision 1.23  1995/09/26  14:44:21  cline
# added tcptime.tst
#
# Revision 1.22  1995/08/08  22:32:54  esb
# added tcpheader.{sig,fun}, removed tcpaction.{sig,fun}
#
# Revision 1.21  1995/03/08  16:01:05  esb
# removed utcp.tst.
#
# Revision 1.20  1995/01/22  15:50:22  esb
# added tcpserve.tst
#
# Revision 1.19  1994/10/14  01:15:41  milnes
# Added "-" before cos.
#
# Revision 1.18  1994/10/04  21:05:29  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.17  1994/06/17  17:50:20  esb
# added tcpeth.tst
#
# Revision 1.16  1994/06/07  16:34:31  robby
#  Added a signature
#
# Revision 1.15  94/03/25  16:45:17  esb
# added tcpping.tst.
# 
# Revision 1.14  94/02/17  01:11:17  esb
# added tcplog.{sig,fun} and tcpreal.tst.
# 
# Revision 1.13  94/01/19  20:50:02  esb
# added tcpresend.
# 
# Revision 1.12  1994/01/09  03:17:09  esb
# entered the files for the new release of TCP.
#
# Revision 1.11  1993/11/11  16:10:23  cline
# added buildtcp.fun
#
# Revision 1.10  1993/10/22  13:41:29  esb
# removed tcpaux.sig and iptcp.fun
#
# Revision 1.9  1993/10/13  17:37:06  esb
# added tcpecho.tst.
#
# Revision 1.8  1993/09/10  11:27:35  cline
# added use.sml
#
# Revision 1.7  93/08/30  20:29:14  esb
# deleted an obsolete line.
# 
# Revision 1.6  1993/08/13  14:16:47  esb
# major revision
#
# Revision 1.5  1993/07/11  02:29:53  esb
# replaced tcpip.sml with tcpip.fun
#
# Revision 1.4  1993/06/11  13:45:56  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:28:54  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := buildtcp.sig buildtcp.fun tcp.sig tcp.tim tcp.tst tcpecho.tst tcpeth.tst tcplog.fun tcplog.sig tcpmain.fun tcpping.tst tcpreal.tst tcpreceive.fun tcpreceive.sig tcpreceive.tst tcpresend.fun tcpresend.sig tcpsend.fun tcpsend.sig tcpsend.tst tcpstate.fun tcpstate.sig tcpstate.tst tcptcb.fun tcptcb.sig use.sml tcpserve.tst tcpheader.sig tcpheader.fun tcptime.tst
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

