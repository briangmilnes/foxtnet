#
#$Log: makefile,v $
# Revision 1.10  1996/03/08  17:31:24  derby
# Add file to support the cyclecounter.
#
# Revision 1.9  1994/10/04  19:30:23  esb
# changed /usr/cs/bin/csh to /bin/csh
#
# Revision 1.8  1994/06/16  16:49:32  danwang
# Updated for functorized Fox_Basis
#
# Revision 1.7  1994/01/17  18:55:55  milnes
# Added timer.fun timer.sig
#
# Revision 1.6  1993/12/07  02:44:33  esb
# added timer.{sig,fun}
#
# Revision 1.5  1993/07/10  04:01:51  esb
# brought in the timing functions that used to be in util
#
# Revision 1.4  1993/06/11  13:46:18  milnes
# Makefile->makefile.
#
# Revision 1.3  93/06/11  13:29:16  milnes
# Reconstructed after cataclism.
# 
#

SHELL := /bin/csh
PWD := $(shell pwd)
FILES := stopwatch.fun stopwatch.sig time.fun time.sig timer.fun timer.sig timeutil.fun timeutil.sig timing.fun timing.sig timer.sig timer.fun use.sml timebase.sig timebase32.str cyclecount.sml
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

