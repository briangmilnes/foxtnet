
 This directory, /afs/cs/project/fox/FoxNet/src, contains the sources
for the Fox Project's implementations of protocol software in SML/NJ.

The directory structure breaks down as follows:

TAGS            - a tags file for the whole set of source files 
administrivia/  - a trivial coding standard and some header files.
bmark/          - some benchmark files.
bytes/          - sml for low level representation.
control/        - a directory of control oriented files.
driver/         - a directory tree of driver software.
filter/         - a module the does filtering on binary data types.
foxnetsg.sml    - a file of source group commands.
packet/         - some packet data structures.
prot/           - a directory tree of protocols.
test/           - some test utilities.
user/           - a definition of a user protocol.
util/           - some utilities.

 Each directory in this tree that holds sml code will contain a
link.sml file. This file defines a source group group to compile, load
and clean its files. The basic source group operations are defined
in foxnetsg.sml.

 However, source group is tickling several NJ bugs, so there is also
a hierarcy of use files. Simply use "use.sml" to get all of the
use files to load, and then "foxuse all" to load them.

$Log: README,v $
Revision 1.2  1994/03/03  17:24:41  esb
added RCS log.

