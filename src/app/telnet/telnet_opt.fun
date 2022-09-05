(*---------------------------------------------------------------------------*>
 *
 *  telnet_opt.fun
 *  8 / 5 / 95
 *  Sidd Puri
 *
 *  The TelnetIO structure provides types for option negotiation in the Telnet
 *  protocol.  The structure obeys TELNETOPT signature from telnet_opt.sig.
 *
<*---------------------------------------------------------------------------*)

functor TelnetOpt (

  structure TelnetIO : TELNETIO
  sharing type TelnetIO.option_code = int

  val debug : bool

) : TELNETOPT = struct

  fun dprint (str : string) = if debug then print str else ()

  (* Types ------------------------------------------------------------------*)

  type option_code = TelnetIO.option_code
  datatype option_rep = Opt of option_code | Ext of option_code

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

  type negotiation = TelnetIO.negotiation

  type status = { active : bool ref, willing : bool ref, pending : int ref }

  type option_manager = {
    control_options: request -> unit,
    handle_option  : (request -> unit) -> (negotiation -> unit),
    get_option     : option * host -> bool ref
  }

  exception TelnetOpt_Bug

  (* Constants --------------------------------------------------------------*)

  val SE    = 240
  val SB    = 250
  val WILL  = 251
  val WON'T = 252
  val DO    = 253
  val DON'T = 254

  val EXT   = 255

  val IS    =   0
  val SEND  =   1

  (* Makestring -------------------------------------------------------------*)

  fun makestring_option Binary              = "Binary"
    | makestring_option Echo                = "Echo"
    | makestring_option Suppress_Go_Ahead   = "Suppress Go Ahead"
    | makestring_option Status              = "Status"
    | makestring_option Timing_Mark         = "Timing Mark"
    | makestring_option Terminal_Type       = "Terminal Type"
    | makestring_option Window_Size         = "Window Size"
    | makestring_option Terminal_Speed      = "Terminal Speed"
    | makestring_option Remote_Flow_Control = "Remote Flow Control"
    | makestring_option Linemode            = "Linemode"
    | makestring_option X_Display_Location  = "X Display Location"
    | makestring_option Old_Environment     = "Old Environment"
    | makestring_option New_Environment     = "New Environment"
    | makestring_option Extended_Options    = "Extended Options"
    | makestring_option (Unrecognized (Opt n)) =
      "Unrecognized option (" ^ makestring n ^ ")"
    | makestring_option (Unrecognized (Ext n)) =
      "Unrecognized extended option (" ^ makestring n ^ ")"

  fun makestring_host Local  = "Local"
    | makestring_host Remote = "Remote"

  fun makestring_neg neg = "\"" ^ implode (map chr neg) ^ "\""

  fun makestring_request (Option (Activate,   opt, h)) =
      "Activate, "   ^ makestring_option opt ^ ", " ^ makestring_host h
    | makestring_request (Option (Deactivate, opt, h)) =
      "Deactivate, " ^ makestring_option opt ^ ", " ^ makestring_host h
    | makestring_request (Option (Accept,     opt, h)) =
      "Accept, "     ^ makestring_option opt ^ ", " ^ makestring_host h
    | makestring_request (Option (Refuse,     opt, h)) =
      "Refuse, "     ^ makestring_option opt ^ ", " ^ makestring_host h
    | makestring_request (Subneg (opt, neg)) =
      "Subneg, "     ^ makestring_option opt ^ "," ^
      concat (map (fn n => " " ^ makestring n) neg)

  (* Parse and unparse options, codes and representations -------------------*)

  fun parse_option   0 = Binary
    | parse_option   1 = Echo
    | parse_option   3 = Suppress_Go_Ahead
    | parse_option   5 = Status
    | parse_option   6 = Timing_Mark
    | parse_option  24 = Terminal_Type
    | parse_option  31 = Window_Size
    | parse_option  32 = Terminal_Speed
    | parse_option  33 = Remote_Flow_Control
    | parse_option  34 = Linemode
    | parse_option  35 = X_Display_Location
    | parse_option  36 = Old_Environment
    | parse_option  39 = New_Environment
    | parse_option 255 = Extended_Options
    | parse_option  n  = Unrecognized (Opt n)

  fun parse_extended_option n = Unrecognized (Ext n)

  fun unparse_option Binary              = Opt   0
    | unparse_option Echo                = Opt   1
    | unparse_option Suppress_Go_Ahead   = Opt   3
    | unparse_option Status              = Opt   5
    | unparse_option Timing_Mark         = Opt   6
    | unparse_option Terminal_Type       = Opt  24
    | unparse_option Window_Size         = Opt  31
    | unparse_option Terminal_Speed      = Opt  32
    | unparse_option Remote_Flow_Control = Opt  33
    | unparse_option Linemode            = Opt  34
    | unparse_option X_Display_Location  = Opt  35
    | unparse_option Old_Environment     = Opt  36
    | unparse_option New_Environment     = Opt  39
    | unparse_option Extended_Options    = Opt 255
    | unparse_option (Unrecognized rep)  = rep

  (* Parse and unparse telnet commands to option requests -------------------*)

  fun parse_tc (TelnetIO.Will  n) = Option (Activate,   parse_option n, Remote)
    | parse_tc (TelnetIO.Won't n) = Option (Deactivate, parse_option n, Remote)
    | parse_tc (TelnetIO.Do    n) = Option (Activate,   parse_option n, Local )
    | parse_tc (TelnetIO.Don't n) = Option (Deactivate, parse_option n, Local )
    | parse_tc (TelnetIO.Subnegotiation (255, [251 (* WILL  *), n])) =
        Option (Activate,   parse_extended_option n, Remote)
    | parse_tc (TelnetIO.Subnegotiation (255, [252 (* WON'T *), n])) =
        Option (Deactivate, parse_extended_option n, Remote)
    | parse_tc (TelnetIO.Subnegotiation (255, [253 (* DO    *), n])) =
        Option (Activate,   parse_extended_option n, Local )
    | parse_tc (TelnetIO.Subnegotiation (255, [254 (* DON'T *), n])) =
        Option (Deactivate, parse_extended_option n, Local )
    | parse_tc (TelnetIO.Subnegotiation (255, 250 (* SB *) :: n :: ns)) = (
        case rev ns of
	  240 (* SE *):: neg => Subneg (parse_extended_option n, rev neg)
	| neg => Subneg (parse_option EXT, SB :: n :: ns)
      )
    | parse_tc (TelnetIO.Subnegotiation (n, neg)) =
	Subneg (parse_option n, neg)

  fun unparse_tc req = let
    fun unp (opt, ext) option = case unparse_option option of
      Opt code => opt code
    | Ext code => TelnetIO.Subnegotiation (255, [ext, code])
  in
    case req of
      Option (Activate,   option, Remote) => unp (TelnetIO.Do,    DO   ) option
    | Option (Deactivate, option, Remote) => unp (TelnetIO.Don't, DON'T) option
    | Option (Activate,   option, Local ) => unp (TelnetIO.Will,  WILL ) option
    | Option (Deactivate, option, Local ) => unp (TelnetIO.Won't, WON'T) option
    | Subneg (option, neg) => (
        case unparse_option option of
	   Opt code => TelnetIO.Subnegotiation (code, neg)
	 | Ext code => TelnetIO.Subnegotiation (EXT, [SB, code] @ neg @ [SE])
      )
    | _ => raise TelnetOpt_Bug
  end

  (* Option table management ------------------------------------------------*)

  fun make_option opt = {
    option = opt,
    local_status = { active = ref false, willing = ref true, pending = ref 0 },
    remote_status = { active = ref false, willing = ref true, pending = ref 0 }
  }

  fun get_status options (option, host) = let
    fun lookup [] =
        { active = ref false, willing = ref false, pending = ref 0 }
      | lookup ({ option = option', local_status, remote_status } :: opts) =
	if option' = option
	  then case host of Local => local_status | Remote => remote_status
	else lookup opts
  in lookup options end

  fun send_status send_neg options = let
    fun make_resp {
      option,
      local_status  = { active = l, ... } : status,
      remote_status = { active = r, ... } : status
    } = case unparse_option option of
	Opt code =>
	  (if !l then [WILL, code] else []) @
	  (if !r then [DO,   code] else [])
      | Ext code =>
	  (if !l then [SB, WILL, code, SE] else []) @
	  (if !r then [SB, DO,   code, SE] else [])
  in
    send_neg (unparse_tc
      (Subneg (Status, IS :: foldr op@ [] (map make_resp options))))
  end

  (* Option manager object --------------------------------------------------*)

  (*>
   * Optional telnet options are:
   *   Terminal_Type, Window_Size, Terminal_Speed, Remote_Flow_Control,
   *   Linemode, X_Display_Location, Old_Environment, New_Environment,
  <*)

  fun make_option_manager send_neg = let
    val options = map make_option [
      Binary, Echo, Suppress_Go_Ahead, Status, Timing_Mark,
      Terminal_Type, Window_Size,
      Extended_Options
    ]

    fun send req = (
      dprint ("[ Opt: sending  " ^ makestring_request req ^ " ]\n");
      send_neg (unparse_tc req)
    )

    fun control_options (req as (Option (action, option, host))) = let
          fun isactive { active, willing, pending } =
	    (!pending + (if !active then 1 else 0)) mod 2 = 1
	  val status as { willing, pending, ... } =
	    get_status options (option, host)
	  fun notify () = (send req; pending := !pending + 1)
        in
	  case action of
	    Activate   => if not (isactive status) then notify () else ()
	  | Deactivate => if     (isactive status) then notify () else ()
	  | Accept => willing := true
	  | Refuse => willing := false
        end
      | control_options req = send req

    fun handle_option hoc neg = (
      dprint ("[ Opt: received " ^ makestring_request (parse_tc neg) ^ " ]\n");
      case parse_tc neg of
	req as (Option (action, option, host)) => let
	  val act = case action of Activate => true | _ => false
	  val { active, willing, pending } = get_status options (option, host)
	in
	  if (!pending > 0)
	    then (pending := !pending - 1; active := act; hoc req)
	  else if (act = !active) then ()
	  else if (!willing orelse act = false)
	    then (send req; active := act; hoc req)
	  else
	    send (Option (if act then Deactivate else Activate, option, host))
	end
      | req as (Subneg (Status, [1 (* SEND *)])) =>
	  (send_status send_neg options; hoc req)
      | req => hoc req
    )

    val get_option = #active o (get_status options)
  in {
      control_options = control_options,
      handle_option   = handle_option,
      get_option      = get_option
  } end

  (*-------------------------------------------------------------------------*)

end
