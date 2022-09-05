(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Sidd Puri (puri@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	Definition of data to be sent and received by telnet.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Telnet_Data
	2.	substructure Option
	3.	types data, T
	4.	makestring functions
	5.	function new
	6.	function join
	7.	function split
	8.	unsupported functions

		iii.	RCS Log
	
$Log: telnetdata.fun,v $
Revision 1.1  1996/07/05  17:44:10  esb
Initial revision


		1.	functor Telnet_Data
*)

functor Telnet_Data (structure B: FOX_BASIS
		     val debug_level: int ref option): TELNET_DATA =
 struct

(*
		2.	substructure Option
*)

  structure Option = Telnet_Option

(*
		3.	types data, T
*)

  datatype element =
      Text of string
    | Data of Word_Array.T
    | Synch
    | No_Op			(* telnet NUL *)
    | Break			(* telnet BRK *)
    | Interrupt_Process		(* telnet IP *)
    | Abort_Output		(* telnet AO *)
    | Are_You_There		(* telnet AYT *)
    | Erase_Char		(* telnet EC *)
    | Erase_Line		(* telnet EL *)
    | Go_Ahead			(* telnet GA *)
    | Will of Option.option_type
    | Won't of Option.option_type
    | Do of Option.option_type
    | Don't of Option.option_type
    | Option_Value of Option.option_value
    | Incomplete_Parse of Word_Array.T

  type data = element list
  type T = data

(*
		4.	makestring functions
*)

  local

   fun take_n ([], _) = []
     | take_n (_, 0w0) = []
     | take_n (head :: rest, count) =
        head :: take_n (rest, count - 0w1)

   fun escape_char char =
        if B.V.Char.ord char < B.V.Char.ord #" " then
	 "^" ^ (B.V.String.implode [B.V.Char.chr (B.V.Char.ord char + 64)])
        else if B.V.Char.ord char > B.V.Char.ord #"~" then
	 "\\" ^ Int.fmt StringCvt.OCT (B.V.Char.ord char)
        else B.V.String.implode [char]

   fun escape_string string =
        B.V.List.fold op^ (map escape_char (explode string)) ""

   fun datastring_max (data, max) =
	let fun to_list NONE = []
	      | to_list (SOME (first, rest)) =
	         first :: to_list (Word_Array.W8.U_Big.F.next rest)
	    val list = to_list (Word_Array.W8.U_Big.F.next
				  (Word_Array.to8 data))
	    fun makestring_list [] = ""
	      | makestring_list [single] = Int.toString (Word8.toInt single)
	      | makestring_list (first :: rest) =
	         Int.toString (Word8.toInt first) ^ "." ^ makestring_list rest
	in case max of
	      NONE => makestring_list list
	    | SOME m =>
	       if B.V.List.length list > Word.toInt m then 
		makestring_list (take_n (list, m)) ^ "..."
	       else makestring_list list
	end

   fun makestring_element (Text text, max) =
	"text: '" ^
	(case max of
	    NONE => escape_string text
	  | SOME m =>
	     if B.V.String.length text > Word.toInt m then 
	      escape_string (implode (take_n (explode text, m))) ^ "..."
	     else escape_string text) ^ "'"
     | makestring_element (Data data, max) =
	"data: " ^ datastring_max (data, max)
     | makestring_element (Synch, _) = "synch"
     | makestring_element (No_Op, _) = "no-op (NUL)"
     | makestring_element (Break, _) = "break (BRK)"
     | makestring_element (Interrupt_Process, _) = "interrupt (IP)"
     | makestring_element (Abort_Output, _) = "abort output (AO)"
     | makestring_element (Are_You_There, _) = "are you there (AYT)"
     | makestring_element (Erase_Char, _) = "erase char (EC)"
     | makestring_element (Erase_Line, _) = "erase line (EL)"
     | makestring_element (Go_Ahead, _) = "go ahead (GA)"
     | makestring_element (Will option, _) =
	"will " ^ Option.makestring_option_type option
     | makestring_element (Won't option, _) =
	"won't " ^ Option.makestring_option_type option
     | makestring_element (Do option, _) =
	"do " ^ Option.makestring_option_type option
     | makestring_element (Don't option, _) =
	"don't " ^ Option.makestring_option_type option
     | makestring_element (Option_Value option, _) =
	"sub-negotiation: " ^ Option.makestring_option_value option
     | makestring_element (Incomplete_Parse data, max) =
	"incomplete: " ^ datastring_max (data, max)

  in
   fun makestring [] = ""
     | makestring [single] = makestring_element (single, NONE)
     | makestring (first :: rest) =
        makestring_element (first, NONE) ^ ", " ^ makestring rest

(* we use the max in printing elements. *)
   fun makestring_max ([], _) = ""
     | makestring_max ([single], max) = makestring_element (single, SOME max)
     | makestring_max (first :: rest, max) =
        makestring_element (first, SOME max) ^ ", " ^
	makestring_max (rest, max)
  end

(*
		5.	function new
*)

  fun new array = [Data array]

(*
		6.	function join
*)

  fun join (l1, l2) = l1 @ l2

(*
		7.	function split
*)

  local
   fun split_aux (list, 0w0, acc) = (B.V.List.reverse acc, list)
     | split_aux ([], count, acc) = (B.V.List.reverse acc, [])
     | split_aux (first :: rest, count, acc) =
        split_aux (rest, count - 0w1, first :: acc)

  in
   fun split (list, count) = split_aux (list, count, [])
  end

(*
		8.	unsupported functions
*)

  exception Telnet_Data_Not_Supported of string

  fun makestring_exn (Telnet_Data_Not_Supported function) =
       SOME ("function " ^ function ^ " not supported by Telnet_Data")
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "telnetdata.fun"
			   val makestring = makestring_exn)

  fun unsupported name args =
       Trace.print_raise (Telnet_Data_Not_Supported name, NONE)

  val uninitialized = unsupported "uninitialized"
  val size = unsupported "size"
  val sub = unsupported "sub"
  val update = unsupported "update"
  val fold = unsupported "fold"

 end
