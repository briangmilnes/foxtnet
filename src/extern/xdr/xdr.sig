(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	Implements the different XDR marshall/unmarshall functions.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	1.	signature INT_XDR
	2.	signature VOID_XDR
	3.	signature STRING_XDR
	4.	signature ENUM_XDR
	5.	signature BOOL_XDR
	6.	signature UBYTE_XDR
	7.	signature OPAQUE_XDR
	8.	signature ARRAY_XDR
	9.	signature UNION2_XDR
	10.	signature UNION2D_XDR
	11.	signature UNION3D_XDR
	12.	signature STRUCT_XDR
	13.	signature LIST_XDR
	14.	signature INTOPT_XDR

	iii.    RCS Log

$Log: xdr.sig,v $
Revision 1.3  1995/05/02  23:28:19  cstone
Changed signature for unions with default.

Revision 1.2  1995/02/07  23:26:57  esb
adapted to new extern.sig.

Revision 1.1  1994/10/14  12:00:03  kcchiang
Initial revision


The following code follows the specification of XDR (RFC 1014).  Some
of the XDR types defined below (eg. INTOPT) are extensions to RFC
1014, and are not part of the definitions in the RFC.

*)

(*
	1.	signature INT_XDR

	XDR signature for non-negative integers
*)

signature INT_XDR =
 sig
  include EXTERN
  sharing type T = int
 end (* sig *)

(*
	2.	signature VOID_XDR

	XDR signature for "void"
*)

signature VOID_XDR =
 sig
  include EXTERN
  sharing type T = unit
 end (* sig *)

(*
	3.	signature STRING_XDR

	XDR signature for strings
*)

signature STRING_XDR =
 sig
  include EXTERN
  sharing type T = string
 end (* sig *)

(*
	4.	signature ENUM_XDR

	XDR signature for enumerations.
*)

signature ENUM_XDR = INT_XDR

(*
	5.	signature BOOL_XDR

	XDR signature for booleans.
*)

signature BOOL_XDR =
 sig
  include EXTERN
  sharing type T = bool
 end (* sig *)

(*
	6.	signature UBYTE_XDR

	XDR signature for unsigned byte4s.
*)

signature UBYTE_XDR =
 sig
  include EXTERN
  sharing type T = FoxWord32.word
 end (* sig *)

(*
	7.	signature OPAQUE_XDR

	XDR signature for opaque values, i.e. uninterpreted bytes
*)

signature OPAQUE_XDR =
 sig
  include EXTERN
  sharing type T      = ByteArray.bytearray
 end (* sig *)

(*
	8.	signature ARRAY_XDR

	XDR signature for arrays
*)

signature ARRAY_XDR = EXTERN


(*
	9.	signature UNION2_XDR

	XDR signature for unions of 2 other types
*)

signature UNION2_XDR =
 sig
  structure Variant_1: EXTERN
  structure Variant_2: EXTERN

  datatype union = T1 of Variant_1.T | T2 of Variant_2.T

  include EXTERN
  sharing type T = union
 end (* sig *)

(*
	10.	signature UNION2D_XDR

	same as above, but supports defaults
*)

signature UNION2D_XDR =
 sig			
  structure Variant_1: EXTERN	
  structure Variant_2: EXTERN
  structure Enum: ENUM_XDR

  datatype union = T1 of Variant_1.T | T2 of (Enum.T * Variant_2.T)

  include EXTERN
  sharing type T = union
 end (* sig *)

(*
	11.	signature UNION3D_XDR

	same as above, but with 3 types
*)

signature UNION3D_XDR =
 sig
  structure Variant_1: EXTERN
  structure Variant_2: EXTERN
  structure Variant_3: EXTERN
  structure Enum: ENUM_XDR

  datatype union = T1 of Variant_1.T 
                 | T2 of Variant_2.T 
                 | T3 of (Enum.T * Variant_3.T)

  include EXTERN
  sharing type T = union
 end (* sig *)

(*
	12.	signature STRUCT_XDR

	Signature for "structures", i.e. tuples.
*)

signature STRUCT_XDR = EXTERN

(* 
	13.	signature LIST_XDR

	Signature for lists.
*)

signature LIST_XDR = EXTERN

(*
	14.	signature INTOPT_XDR

	Signature for integer options.
*)

signature INTOPT_XDR = EXTERN

