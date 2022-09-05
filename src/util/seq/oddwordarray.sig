(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	oddwordarray.sig: signatures for word arrays other than the
	standard power-of-two-byte sized ones.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ODD_WORD_ARRAY
	2.	signature CONVERT_SEQ
	3.	signature FINITE_SEQ
	4.	signature COMPARE_SEQ
	5.	signature LIST_SEQ
	6.	signature STREAM_SEQ
	7.	signature CHANNEL_SEQ
	8.	signature ARRAY_SEQ
	9.	signature REV_ARRAY_SEQ
	10.	signature BYTE_ACCESS_ARRAY
	11.	signature WORD_ARRAY
	12.	signature EXPOSE_WORD_ARRAY

		iii.	RCS Log
	
$Log: oddwordarray.sig,v $
Revision 1.1  1995/11/12  16:47:20  esb
Initial revision


		1.	signature ODD_WORD_ARRAY
*)

signature ODD_WORD_ARRAY =
 sig
  include BYTE_ACCESS_ARRAY

  val from: T -> Word_Array.T
  val to: Word_Array.T -> T
 end
