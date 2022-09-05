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
	1.	functor Telnet_Io
	2.	substructure Data
	3.	exception Extern
	4.	exported types
	5.	internal extern substructures
	6.	Special values
	7.	internal function marshal_string
	8.	internal function marshal_list
	9.	internal function marshal_command
	10.	internal function option_code
	11.	internal function marshal_env_var
	12.	internal function marshal_env_var_value
	13.	internal function marshal_env_value_pair
	14.	internal function marshal_option_value
	15.	internal function marshal_subnegotiation
	16.	internal function marshal_option
	17.	internal function marshal_data
	18.	internal function has_synch
	19.	function marshal
	20.	function size
	21.	internal function split
	22.	internal function make_incomplete
	23.	internal function get_next_byte
	24.	internal function unmarshal_text
	25.	internal function unmarshal_option
	26.	internal function get_subnegotiation
	27.	internal function ignore_option
	28.	internal function parse_number
	29.	internal function parse_number_pair
	30.	internal function parse_x_display
	31.	internal function parse_var_name
	32.	internal function get_env_var
	33.	internal function parse_send_env
	34.	internal function parse_env_pair
	35.	internal function option_value
	36.	internal function unmarshal_subnegotiation
	37.	internal function unmarshal_iac
	38.	internal function first_iac
	39.	internal function unmarshal_loop
	40.	internal function make_byte_list
	41.	internal function revert_byte_list
	42.	internal function find_data_mark
	43.	internal function unmarshal_urgent
	44.	function makestring_wordarray
	45.	function makestring_list
	46.	function unmarshal

		iii.	RCS Log
	
$Log: telnetio.fun,v $
Revision 1.3  1996/07/23  18:28:29  cline
*** empty log message ***

Revision 1.2  1996/07/10  21:23:00  esb
implemented CR processing correctly.

Revision 1.1  1996/07/05  17:45:13  esb
Initial revision


		1.	functor Telnet_Io
*)

functor Telnet_Io (structure Data: TELNET_DATA
		   structure In: EXTERNAL
		   structure Out: EXTERNAL
(* suppress_lf_after_cr should be true if this is used for the server
   and the "standard" telnet behavior is desired, false otherwise. *)
		   val suppress_lf_after_cr: bool
		   structure B: FOX_BASIS
		   val debug_level: int ref option): TELNET_IO =
 struct

(*
		2.	substructure Data
*)

  structure Data = Data

(*
		3.	exception Extern
*)

  exception Extern
  exception Incomplete_Parse_Used_As_Output_Type
  exception Impossible_IAC_Bug

  fun makestring_exn Extern = SOME "Extern (from telnetio.fun)"
    | makestring_exn Incomplete_Parse_Used_As_Output_Type =
       SOME "constructor 'Incomplete_Parse' was given to 'marshal' or 'size'"
    | makestring_exn Impossible_IAC_Bug = SOME "internal error/IAC bug"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "telnetio.fun"
			   val makestring = makestring_exn)

(*
		4.	exported types
*)

  type T = Data.T
  datatype urgency = Regular | Urgent
  datatype cr_state = No_CR | CR_Was_Last_Char
  type cursor = Word.word

  type lower_extern_in = In.T
  type incoming = lower_extern_in * urgency * cr_state ref *
                  Word_Array.T option	(* any unparsed prior data *)
  type extern_in = incoming

  type lower_extern_out = Out.T
  type outgoing = lower_extern_out * urgency ref
  type extern_out = outgoing

(*
		5.	internal extern substructures
*)

  structure Marshal_Word8 = 
      Protocol_Extern8 (structure In = In
			structure Out = Out
			structure B = B)

  structure Marshal_Word16 = 
      Protocol_Extern16_Big (structure In = In
			     structure Out = Out
			     structure B = B)

(*
		6.	Special values

	Taken from RFC 854, p. 14 unless otherwise indicated.
*)

  val mk_code = Word8.fromInt
  val word8_to_char = B.V.Char.chr o Word8.toInt

(* character and command codes. *)
  val NULL  = mk_code 0		(* p. 10 *)
  val NULL_char = word8_to_char NULL
  val NULL_str = B.V.String.implode [NULL_char]
  val LF    = mk_code 10	(* p. 10 *)
  val LF_char = word8_to_char LF
  val CR    = mk_code 13	(* p. 10 *)
  val CR_char = word8_to_char CR
  val CR_str = B.V.String.implode [CR_char]
  val SE    = mk_code 240	(* end of subnegotiation parameters *)
  val NOP   = mk_code 241	(* no operation *)
  val DM    = mk_code 242	(* data mark, data stream portion of synch *)
  val BRK   = mk_code 243	(* break *)
  val IP    = mk_code 244	(* interrupt process *)
  val AO    = mk_code 245	(* abort output *)
  val AYT   = mk_code 246	(* are you there? *)
  val EC    = mk_code 247	(* erase character *)
  val EL    = mk_code 248	(* erase line *)
  val GA    = mk_code 249	(* go ahead *)
  val SB    = mk_code 250	(* start of subnegotiation *)
  val WILL  = mk_code 251
  val WON'T = mk_code 252
  val DO    = mk_code 253
  val DON'T = mk_code 254
  val IAC   = mk_code 255	(* interpret as command *)
  val IAC_char = word8_to_char IAC
  val IAC_str = B.V.String.implode [IAC_char]

(* option codes. *)
  val BINARY = mk_code 0		(* RFC 856 *)
  val ECHO = mk_code 1			(* RFC 857 *)
  val SUPPRESS_GO_AHEAD = mk_code 3	(* RFC 858 *)
  val STATUS = mk_code 5		(* RFC 859 *)
  val TIMING_MARK = mk_code 6		(* RFC 860 *)
  val EXTENSION = mk_code 255		(* RFC 861 *)
  val TERMINAL_TYPE = mk_code 24	(* RFC 1091 *)
  val WINDOW_SIZE = mk_code 31		(* RFC 1073 *)
  val TERMINAL_SPEED = mk_code 32	(* RFC 1079 *)
  val REMOTE_FLOW_CONTROL = mk_code 33	(* RFC 1372 *)
  val X_DISPLAY_LOCATION = mk_code 35	(* RFC 1096 *)
  val OLD_ENVIRONMENT = mk_code 36	(* RFC 1408 *)
  val ENVIRONMENT = mk_code 39		(* RFC 1572 *)

(* sub-option values. *)
  val IS    = mk_code 0		(* RFC 859, 1079, 1091, 1096, 1408, 1572 *)
  val SEND  = mk_code 1		(* RFC 859, 1079, 1091, 1096, 1408, 1572 *)
  val INFO  = mk_code 2		(* RFC 1408, 1572 *)

(* option values. *)
  val FLOW_OFF = mk_code 0		(* RFC 1372 *)
  val FLOW_ON = mk_code 1		(* RFC 1372 *)
  val FLOW_ANY = mk_code 2		(* RFC 1372 *)
  val FLOW_XON = mk_code 3		(* RFC 1372 *)

  val ENV_VAR = mk_code 0		(* RFC 1572, 1408 *)
  val ENV_VALUE = mk_code 1		(* RFC 1572, 1408 *)
  val ENV_ESC = mk_code 2		(* RFC 1572, 1408 *)
  val ENV_USERVAR = mk_code 3		(* RFC 1572, 1408 *)

  val WRONG_ENV_VAR = ENV_VALUE		(* RFC 1571 *)
  val WRONG_ENV_VALUE = ENV_VAR		(* RFC 1571 *)

(*
		7.	internal function marshal_string

	Convert a string to a wordarray, escaping each IAC with an IAC
	and padding each CR that is not part of CR/LF with a NUL
	character.
*)

  local

   fun escape_iac_cr_string text =
        let fun escape_loop ([], result) =
	         (B.V.String.implode (B.V.List.reverse result),
		  Word.fromInt (B.V.List.length result))
	      | escape_loop (first :: rest, accumulator) =
	         if first = IAC_char then
		  escape_loop (rest, first :: first :: accumulator)
		 else if first = CR_char then
		  case rest of
		     [] =>
		      escape_loop (rest, NULL_char :: first :: accumulator)
		   | (second :: new_rest) =>
		      if second = LF_char then
		       escape_loop (new_rest, second :: first :: accumulator)
		      else
		       escape_loop (rest, NULL_char :: first :: accumulator)
		 else
		  escape_loop (rest, first :: accumulator)
	in escape_loop (B.V.String.explode text, [])
	end

   fun escape_iac_cr_data data =
        let fun find_iac_cr NONE = false
	      | find_iac_cr (SOME (first, rest)) =
	         if first = IAC orelse first = CR then true
		 else find_iac_cr (Word_Array.W8.U_Big.F.next rest)
	    val data8 = Word_Array.to8 data
	    fun escape_iac_cr (NONE, NONE) = NONE
	      | escape_iac_cr (x, SOME extra) = SOME (extra, (x, NONE))
	      | escape_iac_cr (SOME (first, rest), NONE) =
	         if first = IAC then
		  SOME (IAC, (Word_Array.W8.U_Big.F.next rest, SOME IAC))
	         else if first = CR then
		  case Word_Array.W8.U_Big.F.next rest of
		     NONE =>
		      SOME (first, (NONE, SOME NULL))
		   | SOME (second, new_rest) =>
		      if second = LF then
		       SOME (first, (Word_Array.W8.U_Big.F.next new_rest,
				     SOME second))
		      else
		       SOME (first, (SOME (second, new_rest), SOME NULL))
		 else SOME (first, (Word_Array.W8.U_Big.F.next rest, NONE))
	in if find_iac_cr (Word_Array.W8.U_Big.F.next data8) then
	    Word_Array.from8 (Word_Array.W8.U_Big.F.new escape_iac_cr
			      (Word_Array.W8.U_Big.F.next data8, NONE))
	   else data
	end

   val mk_byte = Word8.fromInt o B.V.Char.ord

   fun escape_env_var [] = []
     | escape_env_var (first :: rest) =
        if mk_byte first = ENV_ESC then first :: first :: escape_env_var rest
        else if mk_byte first = ENV_VAR then
	 word8_to_char ENV_ESC :: first :: escape_env_var rest
        else if mk_byte first = ENV_VALUE then
	 word8_to_char ENV_ESC :: first :: escape_env_var rest
        else if mk_byte first = ENV_USERVAR then
	 word8_to_char ENV_ESC :: first :: escape_env_var rest
	else first :: escape_env_var rest

   fun marshal_string (extern, string) =
        let val (text, length) = escape_iac_cr_string string
	    fun gen index = mk_byte (B.V.String.ordof (text, Word.toInt index))
	    val data = Word_Array.from8 (Word_Array.W8.U_Big.F.tabulate
					 (gen, length))
	    fun execute cursor =
	         (Out.update (extern, cursor, data);
		  cursor + length)
	in execute
	end

(*
		8.	internal function marshal_list
*)

   fun marshal_list marshal_single (extern, []) = (fn cursor => cursor)
     | marshal_list marshal_single (extern, first :: rest) =
        marshal_list marshal_single (extern, rest) o
	marshal_single (extern, first)

(*
		9.	internal function marshal_command
*)

   fun marshal_command (extern, command) =
	Marshal_Word8.marshal (extern, command) o
	Marshal_Word8.marshal (extern, IAC)

(*
		10.	internal function option_code
*)

   fun option_code Data.Option.Binary = BINARY
     | option_code Data.Option.Echo = ECHO
     | option_code Data.Option.Suppress_Go_Ahead = SUPPRESS_GO_AHEAD
     | option_code Data.Option.Status = STATUS
     | option_code Data.Option.Timing_Mark = TIMING_MARK
     | option_code Data.Option.Terminal_Type = TERMINAL_TYPE
     | option_code Data.Option.Window_Size = WINDOW_SIZE
     | option_code Data.Option.Terminal_Speed = TERMINAL_SPEED
     | option_code Data.Option.Remote_Flow_Control = REMOTE_FLOW_CONTROL
     | option_code Data.Option.X_Display_Location = X_DISPLAY_LOCATION
     | option_code Data.Option.Environment = ENVIRONMENT
     | option_code Data.Option.Old_Environment = OLD_ENVIRONMENT
     | option_code (Data.Option.Unknown code) = code

(*
		11.	internal function marshal_env_var

	RFC 1572, 1571, 1408
*)

   fun marshal_env_var VAR (extern, Data.Option.All_Vars) =
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.User_Name) =
	marshal_string (extern, "USER") o
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.Job) =
	marshal_string (extern, "JOB") o
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.Account) =
	marshal_string (extern, "ACCT") o
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.Printer) =
	marshal_string (extern, "PRINTER") o
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.System_Type) =
	marshal_string (extern, "SYSTEMTYPE") o
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.Display) =
	marshal_string (extern, "DISPLAY") o
	Marshal_Word8.marshal (extern, VAR)
     | marshal_env_var VAR (extern, Data.Option.User var) =
	marshal_string (extern, implode (escape_env_var (explode var))) o
	Marshal_Word8.marshal (extern, ENV_USERVAR)

(*
		12.	internal function marshal_env_var_value

	RFC 1572, 1571, 1408
*)

   fun marshal_env_var_value VALUE (extern, NONE) = (fn cursor => cursor)
     | marshal_env_var_value VALUE (extern, SOME "") =
	Marshal_Word8.marshal (extern, VALUE)
     | marshal_env_var_value VALUE (extern, SOME string) =
	marshal_string (extern, implode (escape_env_var (explode string))) o
	Marshal_Word8.marshal (extern, VALUE)

(*
		13.	internal function marshal_env_value_pair

	RFC 1572, 1408
*)

   fun marshal_env_value_pair (marshal_env, marshal_value)
                              (extern, (env, value)) =
	marshal_value (extern, value) o marshal_env (extern, env)

(*
		14.	internal function marshal_option_value
*)

 (* RFC 1091 *)
   fun marshal_option_value (extern, Data.Option.Request_Terminal_Type) =
	Marshal_Word8.marshal (extern, SEND) o
	Marshal_Word8.marshal (extern, TERMINAL_TYPE)
     | marshal_option_value (extern, Data.Option.Terminal_Type_Is value) =
	marshal_string (extern, value) o
	Marshal_Word8.marshal (extern, IS) o
	Marshal_Word8.marshal (extern, TERMINAL_TYPE)
 (* RFC 1073 *)
     | marshal_option_value (extern,
			     Data.Option.Window_Size_Is {columns, rows}) =
	Marshal_Word16.marshal (extern, Word16.fromInt rows) o
	Marshal_Word16.marshal (extern, Word16.fromInt columns) o
	Marshal_Word8.marshal (extern, WINDOW_SIZE)
 (* RFC 1079 *)
     | marshal_option_value (extern, Data.Option.Request_Terminal_Speed) =
	Marshal_Word8.marshal (extern, SEND) o
	Marshal_Word8.marshal (extern, TERMINAL_SPEED)
     | marshal_option_value (extern,
			     Data.Option.Terminal_Speed_Is {send, receive}) =
	marshal_string (extern,
			Int.toString send ^ "," ^ Int.toString receive) o
	Marshal_Word8.marshal (extern, IS) o
	Marshal_Word8.marshal (extern, TERMINAL_SPEED)
 (* RFC 1372 *)
     | marshal_option_value (extern, Data.Option.Disable_Flow_Control) =
	Marshal_Word8.marshal (extern, FLOW_OFF) o
	Marshal_Word8.marshal (extern, REMOTE_FLOW_CONTROL)
     | marshal_option_value (extern, Data.Option.Enable_Flow_Control) =
	Marshal_Word8.marshal (extern, FLOW_ON) o
	Marshal_Word8.marshal (extern, REMOTE_FLOW_CONTROL)
     | marshal_option_value (extern, Data.Option.Flow_Control_Any) =
	Marshal_Word8.marshal (extern, FLOW_ANY) o
	Marshal_Word8.marshal (extern, REMOTE_FLOW_CONTROL)
     | marshal_option_value (extern, Data.Option.Flow_Control_Xon) =
	Marshal_Word8.marshal (extern, FLOW_XON) o
	Marshal_Word8.marshal (extern, REMOTE_FLOW_CONTROL)
 (* RFC 1096 *)
     | marshal_option_value (extern, Data.Option.Request_X_Display) =
	Marshal_Word8.marshal (extern, SEND) o
	Marshal_Word8.marshal (extern, X_DISPLAY_LOCATION)
     | marshal_option_value (extern,
			     Data.Option.X_Display_Is {host, display,
						       screen}) =
	marshal_string (extern,
			host ^ ":" ^ Int.toString display ^
			(case screen of
			    NONE => ""
			  | SOME s => "." ^ Int.toString s)) o
	Marshal_Word8.marshal (extern, IS) o
	Marshal_Word8.marshal (extern, X_DISPLAY_LOCATION)
 (* RFC 1572, 1408 *)
     | marshal_option_value (extern, Data.Option.Request_Environment list) =
        marshal_list (marshal_env_var ENV_VAR) (extern, list) o
	Marshal_Word8.marshal (extern, SEND) o
	Marshal_Word8.marshal (extern, ENVIRONMENT)
     | marshal_option_value (extern, Data.Option.Environment_Is list) =
        marshal_list (marshal_env_value_pair
		      (marshal_env_var ENV_VAR,
		       marshal_env_var_value ENV_VALUE))
	             (extern, list) o
	Marshal_Word8.marshal (extern, IS) o
	Marshal_Word8.marshal (extern, ENVIRONMENT)
     | marshal_option_value (extern, Data.Option.Environment_Update list) =
        marshal_list (marshal_env_value_pair
		      (marshal_env_var ENV_VAR,
		       marshal_env_var_value ENV_VALUE))
	             (extern, list) o
	Marshal_Word8.marshal (extern, INFO) o
	Marshal_Word8.marshal (extern, ENVIRONMENT)
 (* RFC 1571, 1408 *)
     | marshal_option_value (extern,
			     Data.Option.Old_Request_Environment list) =
        marshal_list (marshal_env_var ENV_VAR) (extern, list) o
	Marshal_Word8.marshal (extern, SEND) o
	Marshal_Word8.marshal (extern, OLD_ENVIRONMENT)
     | marshal_option_value (extern, Data.Option.Old_Environment_Is list) =
        marshal_list (marshal_env_value_pair
		      (marshal_env_var ENV_VAR,
		       marshal_env_var_value ENV_VALUE))
	             (extern, list) o
	Marshal_Word8.marshal (extern, IS) o
	Marshal_Word8.marshal (extern, OLD_ENVIRONMENT)
     | marshal_option_value (extern, Data.Option.Old_Environment_Update list) =
        marshal_list (marshal_env_value_pair
		      (marshal_env_var ENV_VAR,
		       marshal_env_var_value ENV_VALUE))
	             (extern, list) o
	Marshal_Word8.marshal (extern, INFO) o
	Marshal_Word8.marshal (extern, OLD_ENVIRONMENT)
 (* RFC 1571, 1408 *)
     | marshal_option_value (extern,
			     Data.Option.Wrong_Request_Environment list) =
        marshal_list (marshal_env_var WRONG_ENV_VAR) (extern, list) o
	Marshal_Word8.marshal (extern, SEND) o
	Marshal_Word8.marshal (extern, OLD_ENVIRONMENT)
     | marshal_option_value (extern, Data.Option.Wrong_Environment_Is list) =
        marshal_list (marshal_env_value_pair
		      (marshal_env_var WRONG_ENV_VAR,
		       marshal_env_var_value WRONG_ENV_VALUE))
	             (extern, list) o
	Marshal_Word8.marshal (extern, IS) o
	Marshal_Word8.marshal (extern, OLD_ENVIRONMENT)
     | marshal_option_value (extern,
			     Data.Option.Wrong_Environment_Update list) =
        marshal_list (marshal_env_value_pair
		      (marshal_env_var WRONG_ENV_VAR,
		       marshal_env_var_value WRONG_ENV_VALUE))
	             (extern, list) o
	Marshal_Word8.marshal (extern, INFO) o
	Marshal_Word8.marshal (extern, OLD_ENVIRONMENT)

(*
		15.	internal function marshal_subnegotiation
*)

   fun marshal_subnegotiation (extern, option_value) =
	Marshal_Word8.marshal (extern, SE) o
	Marshal_Word8.marshal (extern, IAC) o
	marshal_option_value (extern, option_value) o
	marshal_command (extern, SB)

(*
		16.	internal function marshal_option
*)

   fun marshal_option (extern, option) = (* no extended options for now *)
	Marshal_Word8.marshal (extern, option_code option)

(*
		17.	internal function marshal_data
*)

   fun marshal_data (extern, Data.Text text) = marshal_string (extern, text)
     | marshal_data (extern, Data.Data data) =
        let val data = escape_iac_cr_data data
	    val length = Word_Array.W8.U_Big.F.length (Word_Array.to8 data)
	    fun execute cursor =
	         (Out.update (extern, cursor, data);
		  cursor + length)
	in execute
	end
     | marshal_data (extern, Data.Synch) =
	marshal_command (extern, DM)
     | marshal_data (extern, Data.No_Op) =		(* telnet NUL *)
	marshal_command (extern, NULL)
     | marshal_data (extern, Data.Break) =		(* telnet BRK *)
	marshal_command (extern, BRK)
     | marshal_data (extern, Data.Interrupt_Process) =	(* telnet IP *)
	marshal_command (extern, IP)
     | marshal_data (extern, Data.Abort_Output) =	(* telnet AO *)
	marshal_command (extern, AO)
     | marshal_data (extern, Data.Are_You_There) =	(* telnet AYT *)
	marshal_command (extern, AYT)
     | marshal_data (extern, Data.Erase_Char) =		(* telnet EC *)
	marshal_command (extern, EC)
     | marshal_data (extern, Data.Erase_Line) =		(* telnet EL *)
	marshal_command (extern, EL)
     | marshal_data (extern, Data.Go_Ahead) =		(* telnet GA *)
	marshal_command (extern, GA)
     | marshal_data (extern, Data.Will option_type) =
	marshal_option (extern, option_type) o
	marshal_command (extern, WILL)
     | marshal_data (extern, Data.Won't option_type) =
	marshal_option (extern, option_type) o
	marshal_command (extern, WON'T)
     | marshal_data (extern, Data.Do option_type) =
	marshal_option (extern, option_type) o
	marshal_command (extern, DO)
     | marshal_data (extern, Data.Don't option_type) =
	marshal_option (extern, option_type) o
	marshal_command (extern, DON'T)
     | marshal_data (extern, Data.Option_Value option_value) =
	marshal_subnegotiation (extern, option_value)
     | marshal_data (extern, Data.Incomplete_Parse data) =
	Trace.print_raise (Incomplete_Parse_Used_As_Output_Type,
			   SOME "marshal")

(*
		18.	internal function has_synch
*)

   fun has_synch [] = false
     | has_synch (Data.Synch :: rest) = true
     | has_synch (first :: rest) = has_synch rest

(*
		19.	function marshal
*)

  in
   fun marshal ((extern, urgent), list) =
	(if has_synch list then urgent := Urgent else urgent := Regular;
         marshal_list marshal_data (extern, list))

(*
		20.	function size
*)

   local
    fun size_data (Data.Text text) =
         let val (_, length) = escape_iac_cr_string text
	 in length
	 end
      | size_data (Data.Data data) = 
	 Word_Array.W8.U_Big.F.length (Word_Array.to8
				       (escape_iac_cr_data data))
      | size_data (Data.Incomplete_Parse data) = 
	 Trace.print_raise (Incomplete_Parse_Used_As_Output_Type, SOME "size")
(* all other data is limited in length, so we simply use marshal on a
   limited buffer. *)
      | size_data data =
	 marshal ((Out.uninitialized 0w1000, ref Regular), [data]) 0w0

   in
    fun size list = B.V.List.fold (op+) (B.V.List.map size_data list) 0w0
   end (* local *)

(*
		21.	internal function split
*)

   local

    fun split (array, position) =
         let val length = Word_Array.W8.U_Big.F.length array
	     val front = Word_Array.W8.U_Big.R.seek (array, length - position)
	     val back = Word_Array.W8.U_Big.F.seek (array, position)
	 in (front, back)
	 end

(*
		22.	internal function make_incomplete
*)

    local
     fun next [] = NONE
       | next (first :: rest) =
          case Word_Array.W8.U_Big.F.next first of
	     NONE => next rest
	   | SOME (value, second) => SOME (value, second :: rest)
    in
     fun make_incomplete [last] =
          if Word_Array.W8.U_Big.F.length last > 0w0 then
	   [Data.Incomplete_Parse (Word_Array.from8 last)]
          else []
       | make_incomplete [] = []
       | make_incomplete list =
          make_incomplete [Word_Array.W8.U_Big.F.new next list]
    end

(*
		23.	internal function get_next_byte
*)

    fun get_next_byte [] = NONE
      | get_next_byte (first :: rest) =
         case Word_Array.W8.U_Big.F.next first of
	    NONE => get_next_byte rest
	  | SOME (front, back) => SOME (front, back :: rest)

(*
		24.	internal function unmarshal_text

	Convert a wordarray (with no IACs in it) to a string.
	Unfortunately the (apparently) fastest way to build a string
	is to use implode (there is no tabulate), so we build a list
	first and then implode it to a string.  This also allows us to
	do CR/NL processing as described in RFC 854, pp. 11+, by
	removing every NULL that follows a CR.  We also do the
	non-standard removal of \n following a CR.  This is described
	in a comment in, e.g.  the NetBSD implementation of telnetd
	(/afs/cs.cmu.edu/project/netbsd/src-1.1/usr/src/libexec/ --
	telnetd/state.c), which is pretty standard.

		 We now map \r\n ==> \r for pragmatic reasons.
		 Many client implementations send \r\n when
		 the user hits the CarriageReturn key.

		 We USED to map \r\n ==> \n, since \r\n says
		 that we want to be in column 1 of the next
		 printable line, and \n is the standard
		 unix way of saying that (\r is only good
		 if CRMOD is set, which it normally is).
*)

    fun unmarshal_text (array, cr_state) =
         let fun build_list (NONE, state) =
	          (cr_state := state;
		   [])
	       | build_list (SOME (first, rest), state) =
		  if first = NULL orelse
                     (first = LF andalso suppress_lf_after_cr andalso
		      state = CR_Was_Last_Char) then (* skip this char *)
		   build_list (Word_Array.W8.U_Big.F.next rest, No_CR)
		  else			(* include this char *)
		   word8_to_char first ::
		   build_list (Word_Array.W8.U_Big.F.next rest,
			       if first = CR then CR_Was_Last_Char else No_CR)
	 in Data.Text (implode (build_list (Word_Array.W8.U_Big.F.next array,
					    ! cr_state)))
	 end

(*
		25.	internal function unmarshal_option
*)

    fun unmarshal_option (con, list) =
         case get_next_byte list of
	    NONE => NONE
	  | SOME (c, new) =>
	     if c = BINARY then
	      SOME ([con Data.Option.Binary], new) (* RFC 856 *)
	     else if c = ECHO then
	       SOME ([con Data.Option.Echo], new) (* RFC 857 *)
	     else if c = SUPPRESS_GO_AHEAD then
	      SOME ([con Data.Option.Suppress_Go_Ahead], new) (* RFC 858 *)
	     else if c = STATUS then
	      SOME ([con Data.Option.Status], new) (* RFC 859 *)
	     else if c = TIMING_MARK then
	      SOME ([con Data.Option.Timing_Mark], new) (* RFC 860 *)
	     else if c = TERMINAL_TYPE then
	      SOME ([con Data.Option.Terminal_Type], new) (* RFC 1091 *)
	     else if c = WINDOW_SIZE then
	      SOME ([con Data.Option.Window_Size], new)	(* RFC 1073 *)
	     else if c = TERMINAL_SPEED then
	      SOME ([con Data.Option.Terminal_Speed], new) (* RFC 1079 *)
	     else if c = REMOTE_FLOW_CONTROL then
	      SOME ([con Data.Option.Remote_Flow_Control], new)	(* RFC 1372 *)
	     else if c = X_DISPLAY_LOCATION then
	      SOME ([con Data.Option.X_Display_Location], new) (* RFC 1096 *)
	     else if c = ENVIRONMENT then
	      SOME ([con Data.Option.Environment], new) (* RFC 1572 *)
	     else if c = OLD_ENVIRONMENT then
	      SOME ([con Data.Option.Old_Environment], new) (* 1408, 1571 *)
	     else if c = EXTENSION then			(* RFC 861 *)
(* note: we don't support any telnet extension options (since none are
   defined for now).  So we just read it and discard it. *)
	      case get_next_byte new of
	         NONE => NONE
	       | SOME (_, final) => SOME ([], final)
	     else
	      SOME ([con (Data.Option.Unknown c)], new)

(*
		26.	internal function get_subnegotiation
*)

    local
     fun get_subnegotiation list =
          case get_next_byte list of
	     NONE => NONE
	   | SOME (c, second) =>
	      if c = IAC then
	       case get_next_byte second of
		  NONE => NONE
		| SOME (new_c, third) =>
		   if new_c = SE then	(* yes! end of subnegotiation. *)
		    SOME ([], third)	(* do not return the closing IAC/SE *)
		   else
		    case get_subnegotiation second of
		       NONE => NONE
		     | SOME (char_list, final) => SOME (c :: char_list, final)
	      else
	       case get_subnegotiation second of
		  NONE => NONE
	        | SOME (char_list, final) => SOME (c :: char_list, final)

(*
		27.	internal function ignore_option
*)

     fun ignore_option (error, code, list) =
	  (Trace.local_print ("ignoring unknown " ^ error ^ " with code " ^
			      Int.toString (Word8.toInt code) ^ ", length " ^
			      Int.toString (B.V.List.length list) ^
			      ", string " ^ implode (map word8_to_char list));
	   NONE)

(*
		28.	internal function parse_number
*)

     fun parse_number (acc, []) = (acc, [])
       | parse_number (acc, first :: rest) =
          if #"0" <= word8_to_char first andalso
	     word8_to_char first <= #"9" then
	   parse_number (acc * 10 + Word8.toInt first - B.V.Char.ord #"0",
			 rest)
	  else if word8_to_char first = #"," then (* skip *) (acc, rest)
	  else (acc, first :: rest)

(*
		29.	internal function parse_number_pair
*)

     fun parse_number_pair chars =	(* RFC 1079 *)
          let val (first, rest) = parse_number (0, chars)
	      val (second, final) = parse_number (0, rest)
	  in if final = [] then SOME (first, second) else NONE
	  end

(*
		30.	internal function parse_x_display
*)

     fun parse_x_display chars =	(* RFC 1096 *)
          let fun compute_name s = 
	           B.V.String.implode (map word8_to_char (B.V.List.reverse s))
	      fun parse_host (name, []) = (compute_name name, [])
		| parse_host (name, first :: rest) =
	           if #":" = word8_to_char first then (compute_name name, rest)
		   else parse_host (first :: name, rest)
	      val (host, non_host) = parse_host ([], chars)
	      val (display, rest) = parse_number (0, non_host)
	  in case rest of
	        [] => {host = host, display = display, screen = NONE}
	      | (first :: rest) =>
		 if #"." = word8_to_char first then
		  {host = host, display = display,
		   screen = SOME (#1 (parse_number (0, rest)))}
		 else {host = host, display = display, screen = NONE}
	  end

(*
		31.	internal function parse_var_name
*)

     fun parse_var_name [] = ("", [])
       | parse_var_name (first :: rest) =
          if first = ENV_VAR orelse first = ENV_USERVAR orelse
	     first = ENV_VALUE then ("", first :: rest)
	  else if first = ENV_ESC then
	   case rest of
	      [] =>
	       (Trace.local_print
		  "parse_var_name error, escape at end of suboption";
		("", [])   )
	    | (second :: new) =>
	       let val (string, list) = parse_var_name new
	       in (B.V.String.implode [word8_to_char second] ^ string, list)
	       end
	  else
	   let val (string, list) = parse_var_name rest
	   in (B.V.String.implode [word8_to_char first] ^ string, list)
	   end

(*
		32.	internal function get_env_var
*)

     fun get_env_var (VAR, code, name) =
          if code = VAR then
	   if B.V.String.caseless_equal (name, "user") then
	    SOME Data.Option.User_Name
	   else if B.V.String.caseless_equal (name, "job") then
	    SOME Data.Option.Job
	   else if B.V.String.caseless_equal (name, "acct") then
	    SOME Data.Option.Account
	   else if B.V.String.caseless_equal (name, "printer") then
	    SOME Data.Option.Printer
	   else if B.V.String.caseless_equal (name, "systemtype") then
	    SOME Data.Option.System_Type
	   else if B.V.String.caseless_equal (name, "display") then
	    SOME Data.Option.Display
	   else NONE
	  else SOME (Data.Option.User "")

(*
		33.	internal function parse_send_env
*)

     fun parse_send_env (VAR, []) = SOME []	(* RFC 1572 *)
       | parse_send_env (VAR, first :: rest) =
          if first <> VAR andalso first <> ENV_USERVAR then NONE
	  else
	   case parse_send_env (VAR, rest) of
	      NONE => (* parse the variable name first, then continue *)
	       let val (name, new_rest) = parse_var_name rest
	       in case parse_send_env (VAR, new_rest) of
	             NONE => NONE
		   | SOME list =>
		      case get_env_var (VAR, first, name) of
		         NONE => NONE
		       | SOME option => SOME (option :: list)
	       end
	    | SOME list =>		(* no variable name follows *)
	       if first = VAR then SOME (Data.Option.All_Vars :: list)
	       else SOME (Data.Option.User "" :: list)

(*
		34.	internal function parse_env_pair
*)

     fun parse_env_pair (_, _, []) = SOME []	(* RFC 1572 *)
       | parse_env_pair (VAR, VALUE, first :: rest) =
          if first <> VAR andalso first <> ENV_USERVAR then NONE
	  else
	   let val (name, new_rest) = parse_var_name rest
	   in case get_env_var (VAR, first, name) of
	         NONE => NONE
	       | SOME option =>
		  case new_rest of
		     [] => SOME [(option, NONE)]
		   | (new_first :: value_rest) =>
		      let val (v, after_rest) = parse_var_name value_rest
			  val (value, list) =
			        if new_first = VALUE then (SOME v, after_rest)
				else (NONE, value_rest)
		      in case parse_env_pair (VAR, VALUE, list) of
			    NONE => NONE
			  | SOME result => SOME ((option, value) :: result)
		      end
	   end

(*
		35.	internal function option_value
*)

     fun option_value [] =
          Trace.print_raise (Extern, SOME "option_value, no option code")
       | option_value [single] =	(* unknown option, ignore *)
	  ignore_option ("single-byte sub-negotiation", single, [single])
       | option_value (full as (code :: c :: rest)) =
	  if code = TERMINAL_TYPE then	(* RFC 1091 *)
	   if c = SEND andalso rest = [] then 
	    SOME (Data.Option.Request_Terminal_Type)
	   else if c = IS then
	    SOME (Data.Option.Terminal_Type_Is
		      (implode (map word8_to_char rest)))
	   else
	    ignore_option ("terminal-type subnegotiation", c, full)
	  else if code = WINDOW_SIZE then (* RFC 1073 *)
	   case rest of
	      [w_low, h_high, h_low] =>
	       let val cols = Word8.toInt c * 256 + Word8.toInt w_low
		   val rows = Word8.toInt h_high * 256 + Word8.toInt h_low
	       in SOME (Data.Option.Window_Size_Is {columns = cols,
						    rows = rows})
	       end
	    | _ =>
	       ignore_option ("window-size", code, full)
	  else if code = TERMINAL_SPEED then (* RFC 1079 *)
	   if c = SEND andalso rest = [] then 
	    SOME (Data.Option.Request_Terminal_Speed)
	   else if c = IS then
	    case parse_number_pair rest of
	       NONE =>
		ignore_option ("terminal-speed (bad numbers)", c, full)
	      | SOME (first, second) =>
		 SOME (Data.Option.Terminal_Speed_Is {send = first,
						     receive = second})
	   else ignore_option ("terminal-type subnegotiation", c, full)
	  else if code = REMOTE_FLOW_CONTROL then (* RFC 1372 *)
	   if c = FLOW_OFF then SOME Data.Option.Disable_Flow_Control
	   else if c = FLOW_ON then SOME Data.Option.Enable_Flow_Control
	   else if c = FLOW_ANY then SOME Data.Option.Flow_Control_Any
	   else if c = FLOW_XON then SOME Data.Option.Flow_Control_Xon
	   else ignore_option ("flow-control subnegotiation", c, full)
	  else if code = X_DISPLAY_LOCATION then (* RFC 1096 *)
	   if c = SEND andalso rest = [] then
	    SOME Data.Option.Request_X_Display
	   else if c = IS then
	    SOME (Data.Option.X_Display_Is (parse_x_display rest))
	   else ignore_option ("x-display subnegotiation", c, full)
	  else if code = ENVIRONMENT then (* RFC 1572 *)
	   if c = SEND then
	    case parse_send_env (ENV_VAR, rest) of
	       NONE => ignore_option ("environment request", c, full)
	     | SOME list => SOME (Data.Option.Request_Environment list)
	   else if c = IS then
	    case parse_env_pair (ENV_VAR, ENV_VALUE, rest) of
	       NONE => ignore_option ("environment is", c, full)
	     | SOME list => SOME (Data.Option.Environment_Is list)
	   else if c = INFO then
	    case parse_env_pair (ENV_VAR, ENV_VALUE, rest) of
	       NONE => ignore_option ("environment update", c, full)
	     | SOME list => SOME (Data.Option.Environment_Update list)
	   else ignore_option ("environment subnegotiation", c, full)
	  else if code = OLD_ENVIRONMENT then (* RFC 1571, 1408 *)
	   if c = SEND then
	    case parse_send_env (ENV_VAR, rest) of
	       SOME list => SOME (Data.Option.Old_Request_Environment list)
	     | NONE =>
		case parse_send_env (WRONG_ENV_VAR, rest) of
		   SOME list =>
		    SOME (Data.Option.Wrong_Request_Environment list)
		 | NONE =>
		    ignore_option ("old environment request", c, full)
	   else if c = IS then
	    case parse_env_pair (ENV_VAR, ENV_VALUE, rest) of
	       SOME list => SOME (Data.Option.Old_Environment_Is list)
	     | NONE =>
		case parse_env_pair (WRONG_ENV_VAR, WRONG_ENV_VALUE, rest) of
		   SOME list => SOME (Data.Option.Wrong_Environment_Is list)
		 | NONE => ignore_option ("old environment is", c, full)
	   else if c = INFO then
	    case parse_env_pair (ENV_VAR, ENV_VALUE, rest) of
	       SOME list => SOME (Data.Option.Old_Environment_Update list)
	     | NONE =>
		case parse_env_pair (WRONG_ENV_VAR, WRONG_ENV_VALUE, rest) of
		   SOME list =>
		    SOME (Data.Option.Wrong_Environment_Update list)
		 | NONE => ignore_option ("old environment update", c, full)
	   else ignore_option ("old environment subnegotiation", c, full)
	  else
	   (Trace.local_print ("ignoring unkown sub-option with code " ^
			       Int.toString (Word8.toInt c));
	    NONE)
    in

(*
		36.	internal function unmarshal_subnegotiation
*)

     fun unmarshal_subnegotiation (list, loop, original) =
          case get_subnegotiation list of
	     NONE => make_incomplete original
	   | SOME (list, final_list) =>
	      case option_value list of
	         NONE => loop final_list
	       | SOME option => Data.Option_Value option :: loop final_list
    end

(*
		37.	internal function unmarshal_iac
*)

    fun unmarshal_iac (list, loop) =
         case get_next_byte list of
	    NONE => make_incomplete list
	  | SOME (c, second) => 
	     if c <> IAC then
	      Trace.print_raise (Impossible_IAC_Bug, SOME "unmarshal_iac")
	     else
	      case get_next_byte second of
	         NONE => make_incomplete list
	       | SOME (c, third) =>
		  if c = SE then	(* subnegotiation end *)
		   Trace.print_raise (Extern,
				      SOME "unmarshal_iac, unexpected SE")
		  else if c = NOP then loop third	(* no-op *)
(* RFC 854, p. 9 specifies that in normal mode the data mark is a No-op. *)
		  else if c = DM then loop third	(* data mark *)
		  else if c = BRK then			(* break *)
		   Data.Break :: loop third
		  else if c = IP then	(* interrupt process *)
		   Data.Interrupt_Process :: loop third
		  else if c = AO then	(* abort output *)
		   Data.Abort_Output :: loop third
		  else if c = AYT then	(* are you there *)
		   Data.Are_You_There :: loop third
		  else if c = EC then	(* erase character *)
		   Data.Erase_Char :: loop third
		  else if c = EL then	(* erase line *)
		   Data.Erase_Line :: loop third
		  else if c = GA then	(* go ahead *)
		   Data.Go_Ahead :: loop third
		  else if c = SB then	(* start sub-negotiation *)
		   unmarshal_subnegotiation (third, loop, list)
		  else if c = WILL then	(* will do option *)
		   case unmarshal_option (Data.Will, third) of
		      NONE => make_incomplete list
		    | SOME (list, final) => list @ loop final
		  else if c = WON'T then(* won't do option *)
		   case unmarshal_option (Data.Won't, third) of
		      NONE => make_incomplete list
		    | SOME (list, final) => list @ loop final
		  else if c = DO then	(* request to do option *)
		   case unmarshal_option (Data.Do, third) of
		      NONE => make_incomplete list
		    | SOME (list, final) => list @ loop final
		  else if c = DON'T then(* request to not do option *)
		   case unmarshal_option (Data.Don't, third) of
		      NONE => make_incomplete list
		    | SOME (list, final) => list @ loop final
		  else if c = IAC then	(* escaped byte 255 *)
		   Data.Text IAC_str :: loop third
		  else			(* report as if unescaped. *)
		   (Trace.local_print ("returning escaped character 0x" ^
				       Word8.toString c);
		    Data.Text (B.V.String.implode [word8_to_char c]) ::
		    loop third)

(*
		38.	internal function first_iac
*)

    fun first_iac (NONE, position, original) = split (original, position)
      | first_iac (SOME (char, rest), position, original) =
         if char = IAC then split (original, position)
	 else
	  first_iac (Word_Array.W8.U_Big.F.next rest, position + 0w1, original)

(*
		39.	internal function unmarshal_loop
*)

    fun unmarshal_loop _ [] = []
      | unmarshal_loop cr_state (first :: rest) =
         let val (front, back) = first_iac (Word_Array.W8.U_Big.F.next first,
					    0w0, first)
	     val front_length = Word_Array.W8.U_Big.F.length front
	     val back_length = Word_Array.W8.U_Big.F.length back
	 in case (front_length, back_length) of
	       (0w0, 0w0) => unmarshal_loop cr_state rest
	     | (_, 0w0) =>
		[unmarshal_text (first, cr_state)]
	     | (0w0, _) => unmarshal_iac (first :: rest,
					     unmarshal_loop cr_state)
	     | _ =>
		unmarshal_text (front, cr_state)
		:: unmarshal_iac (back :: rest, unmarshal_loop cr_state)
	 end

(*
		40.	internal function make_byte_list
*)

    local
     fun make_byte_list list =
	  case get_next_byte list of
	     NONE => []
	   | SOME (first, rest) => first :: make_byte_list rest

(*
		41.	internal function revert_byte_list
*)

     fun revert_byte_list list =
          let fun next [] = NONE
		| next (first :: rest) = SOME (first, rest)
	  in [Word_Array.W8.U_Big.F.new next list]
	  end

(*
		42.	internal function find_data_mark
*)

     fun find_data_mark [] = NONE
       | find_data_mark (first :: []) = NONE
       | find_data_mark (first :: second :: rest) =
          if first = IAC andalso second = DM then (* found data mark *)
	   case find_data_mark rest of
	      NONE => SOME ([], rest)	(* this data mark is it *)
	    | SOME x => SOME x	(* erase the data mark, since it is a no-op *)
	  else
	   case find_data_mark (second :: rest) of
	      NONE => NONE
	    | SOME (head, tail) => SOME (first :: head, tail)

(*
		43.	internal function unmarshal_urgent

	Find the last data mark, if any, by scanning the list.  Parse in
	two sections, before and after the data mark.  In what
	goes before, throw away all text but keep any commands; keep
	any thing after the data mark.
*)

    in
     fun unmarshal_urgent (list, cr_state) =
	  case find_data_mark (make_byte_list list) of
	        NONE => unmarshal_loop cr_state list
	      | SOME (head, tail) =>
		 let val head_parse =
		           unmarshal_loop cr_state (revert_byte_list head)
		     val tail_parse =
		           unmarshal_loop cr_state (revert_byte_list tail)
		     fun filter_head [] = []
		       | filter_head (Data.Text _ :: rest) =
		          filter_head rest
		       | filter_head (first :: rest) =
			  first :: filter_head rest
		 in filter_head head_parse @ [Data.Synch] @ tail_parse
		 end
    end (* local *)

(*
		44.	function makestring_wordarray
*)

    fun makestring_wordarray NONE = ""
      | makestring_wordarray (SOME (first, rest)) =
	 Int.toString (Word8.toInt first) ^ "." ^
	 makestring_wordarray (Word_Array.W8.U_Big.F.next rest)

(*
		45.	function makestring_list
*)

    fun makestring_list [] = ""
      | makestring_list [last] =
	 makestring_wordarray (Word_Array.W8.U_Big.F.next
			       (Word_Array.to8 last))
      | makestring_list (first :: rest) =
	 makestring_wordarray (Word_Array.W8.U_Big.F.next
			       (Word_Array.to8 first)) ^ ", " ^
	 makestring_list rest

(*
		46.	function unmarshal
*)

   in
    fun unmarshal ((extern, urgent, cr_state, unparsed), cursor) =
         if cursor <> 0w0 then
	  let val (_, data) = In.split (extern, cursor)
	      val (result, new) = unmarshal ((data, urgent, cr_state,
					      unparsed), 0w0)
	  in (result, new + cursor)
	  end
	 else
	  let val init = case unparsed of NONE => [] | SOME x => [x]
	      val list = B.V.List.reverse (In.fold (extern, op:: , init))
	      val new = In.size extern	(* consume entire input *)
	  in Trace.debug_print (fn _ => "unmarshaling " ^
				makestring_list list);
	     case urgent of
	        Regular => (unmarshal_loop cr_state list, new)
	      | Urgent => (unmarshal_urgent (list, cr_state), new)
	  end
   end (* local *)

  end (* local *)
 end
