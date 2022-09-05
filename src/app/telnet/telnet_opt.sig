(*---------------------------------------------------------------------------*>
 *
 *  telnet_opt.sig
 *  8 / 5 / 95
 *  Sidd Puri
 *
 *  The TELNETOPT signature defines Telnet options and the interface for
 *  handling options.  Options are controled using the option_request type, and
 *  the option_manager object provides a way of storing and changing options.
 *
<*---------------------------------------------------------------------------*)

signature TELNETOPT = sig

  (* Types ------------------------------------------------------------------*)

  type option_code
  type option_rep

  datatype option =
    Binary
  | Echo
  | Suppress_Go_Ahead
  | Status
  | Timing_Mark
  | Terminal_Type
  | Window_Size
  | Terminal_Speed
  | Remote_Flow_Control
  | Linemode
  | X_Display_Location
  | Old_Environment
  | New_Environment
  | Extended_Options
  | Unrecognized of option_rep

  datatype action = Activate | Deactivate | Accept | Refuse
  datatype host = Local | Remote
  datatype request =
    Option of action * option * host
  | Subneg of option * int list

  type negotiation      (* from TELNETIO *)

  (* Functions --------------------------------------------------------------*)

  val makestring_option  : option  -> string
  val makestring_request : request -> string

  (* Option manager object --------------------------------------------------*)

  val make_option_manager :
      (negotiation -> unit)			  (* send negotiation *)
    -> {
      control_options: request -> unit,
      handle_option  : (request -> unit) -> (negotiation -> unit),
      get_option     : option * host -> bool ref
    }

  (*-------------------------------------------------------------------------*)

end
