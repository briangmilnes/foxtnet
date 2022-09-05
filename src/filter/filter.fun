(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This file provides a signature for an ethernet packet filter.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Filter
	2.	exported types
	3.	internal function fv_string
	4.	function makestring
	5.	function filtered_value
	6.	function filter
	7.	assembler types
	8.	internal function assemble
	9.	internal function gen_filtered_value
	10.	internal function gen_filter
	11.	function make

		iii.	RCS Log
	
$Log: filter.fun,v $
Revision 1.22  1996/03/12  21:24:41  esb
adapted to new FOXWORD signature.

Revision 1.21  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.20  1995/11/10  23:28:52  esb
adapted to new word_array.

Revision 1.19  1995/09/18  19:26:01  esb
now compiles to Word_Array.T instead of ByteArray.bytearray.

Revision 1.18  1995/06/20  16:42:59  esb
minor changes.

Revision 1.17  1995/03/09  22:43:39  esb
adapted to new tracing scheme.

Revision 1.16  1995/01/20  17:45:58  esb
tested it and made it work for the MIPS.

Revision 1.15  1995/01/14  02:24:20  esb
fixed the filter, not tested yet.

Revision 1.14  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.13  1994/10/20  19:15:04  esb
fixed a bug.

Revision 1.12  1994/10/20  19:12:19  esb
got rid of a compiler warning.

Revision 1.11  1994/10/20  14:51:10  cline
*** empty log message ***

Revision 1.10  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.9  1994/09/30  16:13:07  esb
use FoxWord16 instead of Byte2

Revision 1.8  1994/08/12  04:10:47  esb
made printing of filters more explicit and unequivocal.

Revision 1.7  1994/06/05  18:39:42  milnes
Added some missing filter operations that make writing filters easier.

Revision 1.6  1993/10/25  19:22:30  cline
removed .U from Byte[421].U

Revision 1.5  1993/10/14  18:19:09  milnes
Used implicit sequencing in let bodies.

Revision 1.4  1993/08/23  22:09:03  esb
added the makestring function.

Revision 1.3  1993/07/22  19:46:18  nickh
typo.

Revision 1.2  1993/07/22  18:18:38  nickh
Adjusted offsets for sub and update to use byte offsets.

Revision 1.1  1993/06/10  22:29:09  milnes
Initial revision


*)

(*
		1.	functor Filter
*)

functor Filter (structure V: VENDOR
                structure Debug: DEBUG
		val filter_os: string (* "mach" or "osf1" *)
                val debug_level: int ref option) : FILTER =
 struct
  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "filter.fun"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  datatype os_type = Mach | Osf

  (* raised at link time for invalid filter_os functor argument *)
  exception Filter_OS

  val os_type = case filter_os of
                   "osf1" => Osf
		 | "mach" => Mach
		 | _ => raise Filter_OS


(*
		2.	exported types
*)

  datatype filtered_value =
      Literal  of Word16.word (* A literal piece of data. *)
    | Data     of int          (* The specified word of the packet data. *)
    | Indirect of filtered_value
    | Andb     of filtered_value * filtered_value (* Bitwise AND. *)
    | Orb      of filtered_value * filtered_value (* Bitwise OR. *)
    | Xorb     of filtered_value * filtered_value (* Bitwise XOR. *)
    | Lshift   of filtered_value * int            (* Left Shift. *)
    | Rshift   of filtered_value * int            (* Right Shift. *)
    | Plus     of filtered_value * filtered_value (* Addition. *)
    | Minus    of filtered_value * filtered_value (* Subtraction. *)

  datatype filter = 
      True  (* The filter that passes all packets. *)
    | False (* The filter that passes no packets. *)
    | Equal   of filtered_value * filtered_value (* Equality. *)
    | Less    of filtered_value * filtered_value (* Less Than. *)
    | Leq     of filtered_value * filtered_value (* Less than or equal. *)
    | Greater of filtered_value * filtered_value (* Greater Than. *)
    | Geq     of filtered_value * filtered_value (* Greater than or equal. *)
    | Neq     of filtered_value * filtered_value (* Inequality. *)
    | When_Equal of filtered_value * filtered_value * filter
       (* If equal, stop the computation with true, otherwise continue. *)
    | Unless_Equal of filtered_value * filtered_value * filter 
       (* If unequal, stop the computation with false, otherwise continue. *)
    | When_Not_Equal of filtered_value * filtered_value * filter
    | Unless_Not_Equal of filtered_value * filtered_value * filter 

  type packet = Word8Array.array
  type device_filter = Word8Array.array

(*
		3.	internal function fv_string

	This function returns a string given a filtered_value
*)

  fun fv_string (Literal b) = "0x" ^ Word16.toString b
    | fv_string (Data i) = "d [" ^ Integer.toString i ^ "]"
    | fv_string (Indirect v) = "ref [" ^ fv_string v ^ "]"
    | fv_string (Andb (f1, f2)) =
       "(" ^ fv_string f1 ^ ") && (" ^ fv_string f2 ^ ")"
    | fv_string (Orb (f1, f2)) =
       "(" ^ fv_string f1 ^ ") || (" ^ fv_string f2 ^ ")"
    | fv_string (Xorb  (f1, f2)) =
       "(" ^ fv_string f1 ^ ") Xor  (" ^ fv_string f2 ^ ")"
    | fv_string (Lshift (f, i)) =
       "(" ^ fv_string f ^ ") << (" ^ Integer.toString i ^ ")"
    | fv_string (Rshift (f, i)) =
       "(" ^ fv_string f ^ ") >> (" ^ Integer.toString i ^ ")"
    | fv_string (Plus (f1, f2)) =
       "(" ^ fv_string f1 ^ ") + (" ^ fv_string f2 ^ ")"
    | fv_string (Minus (f1, f2)) =
       "(" ^ fv_string f1 ^ ") - (" ^ fv_string f2 ^ ")"

(*
		4.	function makestring
*)

  fun makestring True = "True"
    | makestring False = "False"
    | makestring (Equal (f1, f2)) = fv_string f1 ^ " = " ^ fv_string f2
    | makestring (Less (f1, f2)) = fv_string f1 ^ " < " ^ fv_string f2
    | makestring (Leq (f1, f2)) = fv_string f1 ^ " <= " ^ fv_string f2
    | makestring (Greater (f1, f2)) = fv_string f1 ^ " > " ^ fv_string f2
    | makestring (Geq (f1, f2)) = fv_string f1 ^ " >= " ^ fv_string f2
    | makestring (Neq (f1, f2)) = fv_string f1 ^ " <> " ^ fv_string f2
    | makestring (When_Equal (f1, f2, f)) =
       "when " ^ fv_string f1 ^ " = " ^ fv_string f2 ^ " then True; else " ^
       makestring f
    | makestring (Unless_Equal (f1, f2, f)) =
       "unless " ^ fv_string f1 ^ " = " ^ fv_string f2 ^ " then False; else " ^
       makestring f
    | makestring (When_Not_Equal (f1, f2, f)) =
       "when " ^ fv_string f1 ^ " <> " ^ fv_string f2 ^ " then True; else " ^
       makestring f
    | makestring (Unless_Not_Equal (f1, f2, f)) =
       "unless " ^ fv_string f1 ^ " <> " ^ fv_string f2 ^
       " then False; else " ^ makestring f

(*
		5.	function filtered_value

	Interpret a value of type filtered_value on a given packet.
*)

  fun filtered_value (Literal b, packet) = b
    | filtered_value (Data i, packet) = Word16.sub (packet, i * 2)
    | filtered_value (Indirect v, packet) =
       let val pointer = filtered_value (v, packet)
       in Word16.sub (packet, Word16.toInt pointer * 2) 
       end
    | filtered_value (Andb (left, right), packet) =
       Word16.andb (filtered_value (left, packet),
		       filtered_value (right, packet))
    | filtered_value (Orb (left, right), packet) =
       Word16.orb (filtered_value (left, packet),
		      filtered_value (right, packet))
    | filtered_value (Xorb (left, right), packet) =
       Word16.xorb (filtered_value (left, packet),
		       filtered_value (right, packet))
    | filtered_value (Lshift (value, shift), packet) =
       Word16.<< (filtered_value (value, packet), Word31.fromInt shift)
    | filtered_value (Rshift (value, shift), packet) =
       Word16.>> (filtered_value (value, packet), Word31.fromInt shift)
    | filtered_value (Plus (left, right), packet) =
       Word16.+ (filtered_value (left, packet),
		    filtered_value (right, packet))
    | filtered_value (Minus (left, right), packet) =
       Word16.- (filtered_value (left, packet),
		    filtered_value (right, packet))

(*
		6.	function filter

	Interpret a value of type filter on a given packet.
*)

  fun filter True packet = true
    | filter False packet = false 
    | filter (Equal (left, right)) packet =
       filtered_value (left, packet) = filtered_value (right, packet)
    | filter (Less (left, right)) packet =
       Word16.< (filtered_value (left, packet),
		    filtered_value (right, packet))
    | filter (Leq (left, right)) packet =
       Word16.<= (filtered_value (left, packet),
		     filtered_value (right, packet))
    | filter (Greater (left, right)) packet =
       Word16.> (filtered_value (left, packet),
		    filtered_value (right, packet))
    | filter (Geq (left, right)) packet =
       Word16.>= (filtered_value (left, packet),
		     filtered_value (right, packet))
    | filter (Neq (left, right)) packet =
       filtered_value (left, packet) <> filtered_value (right, packet) 
    | filter (When_Equal (left, right, continuation)) packet =
       if filtered_value (left, packet) = filtered_value (right, packet) then
	true
       else filter continuation packet
    | filter (Unless_Equal (left, right, continuation)) packet =
       if filtered_value (left, packet) = filtered_value (right, packet) then
	filter continuation packet
       else false
    | filter (When_Not_Equal (left, right, continuation)) packet =
       if filtered_value (left, packet) <> filtered_value (right, packet) then
	true
       else filter continuation packet
    | filter (Unless_Not_Equal (left, right, continuation)) packet =
       if filtered_value (left, packet) <> filtered_value (right, packet) then
	filter continuation packet
       else false

(*
		7.	assembler types
*)

  datatype asm_value = Push_Lit of Word16.word
                     | Push_Zero
                     | Push_Word of int (* includes Push_Hdr as needed. *)
                     | Push_Ind
                     | Push_Stack of int
                     | Push_None

  datatype asm_op = Asm_Nop | Asm_Eq | Asm_Lt | Asm_Le | Asm_Gt | Asm_Ge
                  | Asm_And | Asm_Or | Asm_Xor | Asm_Neq | Asm_Lsh | Asm_Rsh
                  | Asm_Add | Asm_Sub
                  | Asm_Cor | Asm_Cand | Asm_Cnor | Asm_Cnand

  fun makestring_asm_value (Push_Lit n) = "push 0x" ^ Word16.toString n
    | makestring_asm_value (Push_Word n) =
       "push [" ^ Integer.toString n ^ "]"
    | makestring_asm_value Push_Zero = "push 0"
    | makestring_asm_value Push_Ind = "push indirect"
    | makestring_asm_value (Push_Stack n) =
       "push stack [" ^ Integer.toString n ^ "]"
    | makestring_asm_value Push_None = "no push"

  fun makestring_asm_op Asm_Nop = "nop"
    | makestring_asm_op Asm_Eq = "eq"
    | makestring_asm_op Asm_Lt = "lt"
    | makestring_asm_op Asm_Le = "le"
    | makestring_asm_op Asm_Gt = "gt"
    | makestring_asm_op Asm_Ge = "ge"
    | makestring_asm_op Asm_And = "and"
    | makestring_asm_op Asm_Or = "or"
    | makestring_asm_op Asm_Xor = "xor"
    | makestring_asm_op Asm_Neq = "neq"
    | makestring_asm_op Asm_Lsh = "lsh"
    | makestring_asm_op Asm_Rsh = "rsh"
    | makestring_asm_op Asm_Add = "add"
    | makestring_asm_op Asm_Sub = "sub"
    | makestring_asm_op Asm_Cor = "cor"
    | makestring_asm_op Asm_Cand = "cand"
    | makestring_asm_op Asm_Cnor = "cnor"
    | makestring_asm_op Asm_Cnand = "cnand"

  fun makestring_asm_single (value, opcode) =
       "(" ^ makestring_asm_value value ^ ", " ^ makestring_asm_op opcode ^ ")"

  fun makestring_asm [] = ""
    | makestring_asm (head :: rest) =
       makestring_asm_single head ^ ", " ^ makestring_asm rest

(*
		8.	internal function assemble

	Mach constants from:
	/afs/cs/project/mach3/latest/src/mk/kernel/device/net_status.h
	OSF constants from: ???
*)

  fun assemble_value (Push_Lit _) = 1
    | assemble_value (Push_Word offset) =
       (case os_type of
	   Osf => 16 + offset
	 | Mach => if offset < 7 then (* NETF_PUSHHDR *) 960 + offset
  (* For unexplained reasons, MACH stores the data starting at
     location 2 rather than zero.  As a result, we add two to the
     offset before subtracting the ethernet header size of 7 halfwords
     (14 bytes).  This gives the correct index for the filter. *)
		   else (* NETF_PUSHWORD *) 16 + (2 + offset - 7))
    | assemble_value Push_Zero = 2
    | assemble_value Push_Ind = 14
    | assemble_value (Push_Stack n) = 992 + n
    | assemble_value Push_None = 0

  fun assemble_op Asm_Nop = 0
    | assemble_op Asm_Eq = 1
    | assemble_op Asm_Lt = 2
    | assemble_op Asm_Le = 3
    | assemble_op Asm_Gt = 4
    | assemble_op Asm_Ge = 5
    | assemble_op Asm_And = 6
    | assemble_op Asm_Or = 7
    | assemble_op Asm_Xor = 8
    | assemble_op Asm_Cor = 9
    | assemble_op Asm_Cand = 10
    | assemble_op Asm_Cnor = 11
    | assemble_op Asm_Cnand = 12
    | assemble_op Asm_Neq = 13
    | assemble_op Asm_Lsh = 14
    | assemble_op Asm_Rsh = 15
    | assemble_op Asm_Add = 16
    | assemble_op Asm_Sub = 17

  fun assemble_pair (value, opcode) =
       let val val_encoding =
	     Word16.andb (Word16.fromInt (assemble_value value),
			     Word16.fromInt 0x3ff)
           val op_encoding =
	     Word16.<< (Word16.andb (Word16.fromInt (assemble_op opcode),
				    Word16.fromInt 0x3f), 0w10)
       in Word16.orb (op_encoding, val_encoding)
       end

  datatype single_result =
      One of Word16.word
    | Two of Word16.word * Word16.word 

  fun assemble_single (Push_Lit n, opcode) =
       Two (assemble_pair (Push_Lit n, opcode), n)
    | assemble_single arg = One (assemble_pair arg)

  datatype assemble_data =
      Single of (asm_value * asm_op) list
    | Prefix of Word16.word * (asm_value * asm_op) list

  fun assemble (Single []) = NONE
    | assemble (Single (first :: rest)) =
       (case assemble_single first of
	   One first => SOME (first, Single rest)
	 | Two (first, second) => SOME (first, Prefix (second, rest)))
    | assemble (Prefix (first, rest)) =
       SOME (first, Single rest)

(*
		9.	internal function gen_filtered_value
*)

  fun gen_binop (left, right, opcode) =
       gen_filtered_value (left, Asm_Nop) @ gen_filtered_value (right, opcode)

  and gen_binops (left, right, combiner, Asm_Nop) =
       gen_binop (left, right, combiner)
    | gen_binops (left, right, combiner, last_op) =
       gen_binop (left, right, combiner) @ [(Push_None, last_op)]

  and gen_filtered_value (Literal lit, last_op) = [(Push_Lit lit, last_op)]
    | gen_filtered_value (Data offset, last_op) = [(Push_Word offset, last_op)]
    | gen_filtered_value (Indirect value, last_op) =
       gen_filtered_value (value, Asm_Nop) @ [(Push_Ind, last_op)]
    | gen_filtered_value (Andb (left, right), last_op) =
       gen_binops (left, right, Asm_And, last_op)
    | gen_filtered_value (Xorb (left, right), last_op) =
       gen_binops (left, right, Asm_Xor, last_op)
    | gen_filtered_value (Orb (left,right), last_op) =
       gen_binops (left, right, Asm_Or, last_op)
    | gen_filtered_value (Lshift (left, right), last_op) =
       gen_binops (left, Literal (Word16.fromInt right), Asm_Lsh, last_op)
    | gen_filtered_value (Rshift (left, right), last_op) =
       gen_binops (left, Literal (Word16.fromInt right), Asm_Rsh, last_op)
    | gen_filtered_value (Plus (left, right), last_op) =
       gen_binops (left, right, Asm_Add, last_op)
    | gen_filtered_value (Minus (left, right), last_op) = 
       gen_binops (left, right, Asm_Sub, last_op)

(*
		10.	internal function gen_filter
*)

  fun gen_filter True = [(Push_Lit (Word16.fromInt 1), Asm_Nop)]
    | gen_filter False = [(Push_Zero, Asm_Nop)]
    | gen_filter (Equal (left, right)) = gen_binop (left, right, Asm_Eq)
    | gen_filter (Less (left, right)) = gen_binop (left, right, Asm_Lt)
    | gen_filter (Leq (left, right)) = gen_binop (left, right, Asm_Le)
    | gen_filter (Greater (left, right)) = gen_binop (left, right, Asm_Gt)
    | gen_filter (Geq (left, right)) = gen_binop (left, right, Asm_Ge)
    | gen_filter (Neq (left, right)) = gen_binop (left, right, Asm_Neq)
    | gen_filter (When_Equal (left, right, cont)) =
       gen_binop (left, right, Asm_Cor) @ gen_filter cont
    | gen_filter (Unless_Equal (left, right, cont)) =
       gen_binop (left, right, Asm_Cand) @ gen_filter cont
    | gen_filter (f as (When_Not_Equal (left, right, cont))) =
       gen_binop (left, right, Asm_Cnor) @ gen_filter cont
    | gen_filter (f as (Unless_Not_Equal (left, right, cont))) =
       gen_binop (left, right, Asm_Cnand) @ gen_filter cont

(*
		11.	function make
*)

  fun make filter =
       let val assembly = gen_filter filter
       in if Trace.debug_on () then
	   local_print (makestring filter ^ " -> " ^ makestring_asm assembly)
	  else ();
	  (Word_Array.from16 o Word_Array.W16.unalign)
	  (Word_Array.W16.Native.F.new assemble (Single assembly))
       end

 end (* struct *)
