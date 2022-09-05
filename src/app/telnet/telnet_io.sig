(*---------------------------------------------------------------------------*>
 *
 *  telnet_io.sig
 *  7 / 3 / 95
 *  Sidd Puri
 *
 *  The TELNETIO signature defines the interface of the TelnetIO structure,
 *  which is used by the telnet functor to provide Incoming and Outgoing
 *  structures.  This signature includes the EXTERNAL signature.
 *
 *  The TelnetIO structure defines type T = data list.
 *
<*---------------------------------------------------------------------------*)

signature TELNETIO = sig

  (* TELNETIO is a subsignature of EXTERNAL ---------------------------------*)

  include EXTERNAL

  (* Types and exceptions ---------------------------------------------------*)

  datatype data =
    Synch
  | No_Op
  | Break
  | Interpt_Proc
  | Abort_Output
  | Are_You_There
  | Erase_Char
  | Erase_Line
  | Go_Ahead
  | Data of string

  type option_code

  datatype negotiation =
    Will  of option_code
  | Won't of option_code
  | Do    of option_code
  | Don't of option_code
  | Subnegotiation of option_code * int list

  type parse_state

  type IOT = data list
  sharing type T = IOT

  exception TelnetIO of string

  (* Special values ---------------------------------------------------------*)

  val initial_state : parse_state
  val data_mark : int list

  (* Functions --------------------------------------------------------------*)

  val makestring_data : data        -> string
  val makestring_neg  : negotiation -> string

  val parse	  :   (IOT -> unit)		  (* data handler *)
		    * (negotiation -> unit)	  (* negotiation handler *)
		    * parse_state ref
		    * bool ref			  (* synch *)
		    * { binary: bool ref }	  (* telnet option *)
		    -> int list			  (* incoming data *)
		    -> unit

  val unparse     :   (int list -> unit)	  (* send *)
		    * (unit -> unit)		  (* send synch *)
		    * int list ref		  (* out_buffer *)
		    * {				  (* telnet options *)
		      binary: bool ref,
		      lecho : bool ref,
		      recho : bool ref
		    }
		    -> IOT			  (* outgoing data *)
		    -> unit

  val unparse_neg :    int list ref		  (* out_buffer *)
		    -> negotiation		  (* outgoing data *)
		    -> unit

  (*-------------------------------------------------------------------------*)

end
