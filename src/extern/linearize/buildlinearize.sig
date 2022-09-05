(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: buildlinearize.sig,v $
Revision 1.1  1994/08/25  23:44:37  robby
Initial revision


		1.	...
*)
signature BUILD_LINEARIZE=
sig
  structure Ubyte1:UBYTE1_LINEARIZE
  structure Ubyte2:UBYTE2_LINEARIZE
  structure Ubyte4:UBYTE4_LINEARIZE
  structure Int:INT_LINEARIZE

  (* these structures will reverse the byteorder
     from the bytearray into the ML representation *)
  structure Rev_Ubyte2:UBYTE2_LINEARIZE
  structure Rev_Ubyte4:UBYTE4_LINEARIZE
  structure Rev_Int:INT_LINEARIZE

  structure CString:STRING_LINEARIZE
  structure ByteArray1:BYTEARRAY_LINEARIZE
  structure ByteArray2:BYTEARRAY_LINEARIZE
  structure ByteArray3:BYTEARRAY3_LINEARIZE
  structure ByteArray4:BYTEARRAY_LINEARIZE
  structure Rev_ByteArray2:BYTEARRAY_LINEARIZE
end
