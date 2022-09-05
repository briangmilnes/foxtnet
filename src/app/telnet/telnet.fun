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

        Two Telnet functors, each implementing part of the telnet protocol.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	functor Telnet_Protocol
	2.	substructures
	3.	extension types
	4.	datatypes for the protocol signature
	5.	internal function higher_send
	6.	internal function make_higher_connection
	7.	internal function lower_receive
	8.	internal function lower_status
	9.	internal function make_lower_handler
	10.	internal function make_connect
	11.	internal function make_listen
	12.	internal function lower_session
	13.	function session
	14.	function inetd_handler
	15.	functor Telnet
	16.	structures Setup and Telnet_Setup
	17.	structures Incoming and Outgoing
	18.	structures Status and Telnet_Status
	19.	other substructures
	20.	extension types
	21.	datatypes for the protocol signature
	22.	structure Option_State, option state machine
	23.	internal values local_funs and remote_funs
	24.	internal function negotiate
	25.	internal function handle_single
	26.	internal function local_data_handler
	27.	internal function local_status_handler
	28.	internal function local_connection_handler
	29.	internal function upper_handler
	30.	internal function upper_connect
	31.	internal function upper_listen
	32.	internal function upper_session_handler
	33.	function session
	34.	function inetd_handler
	35.	structure Lower

		iii.	RCS Log
	
$Log: telnet.fun,v $
Revision 1.3  1996/07/10  21:23:20  esb
adapted to new telnetio.sig, with CR processing state for unmarshal.

Revision 1.2  1996/07/05  17:45:38  esb
major re-implementation.


		iv.	Overview

	The implementation of telnet is subdivided into the basic
	part (the Telnet_Protocol functor) and the stateful part
	(the Telnet functor).  The stateful part keeps track of
	options negotiations on a per-connection basis.

	Telnet_Protocol takes the following parameters:

          send_urgent (con, data)
            Sends the data <data> with a TCP ugent flag set.

          parse_urgent (con, status)
            Examines the lower status <status>, and if it indicates an urgent
            message, returns the body of that message.

          lower_flush con
            Flushes any buffered out

		1.	functor Telnet_Protocol
*)

functor Telnet_Protocol (structure IO: TELNET_IO
		         structure Lower: PROTOCOL
		           sharing type IO.lower_extern_out = Lower.Outgoing.T
		              and type IO.lower_extern_in = Lower.Incoming.T
		              and type IO.cursor = Word.word
		         val send_urgent: Lower.connection * Lower.Outgoing.T
			                -> unit
		         val parse_urgent: Lower.connection * Lower.Status.T
		                         -> Lower.Incoming.T option
		         val lower_flush: Lower.connection -> unit
		         structure B: FOX_BASIS
		         val debug_level: int ref option): TELNET_PROTOCOL =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "telnet.fun(basic)"
			   val makestring = Lower.X.makestring)

(*
		2.	substructures
*)

  structure Lower = Lower
  structure IO = IO

  structure Setup = Lower.Setup

  structure Address = Lower.Address

  structure Pattern = Lower.Pattern

  structure Connection_Key = Lower.Connection_Key

  structure Incoming = IO.Data

  structure Outgoing = IO.Data

  structure Status = Lower.Status

  structure Count = Lower.Count

  structure X = Lower.X

  exception Already_Open of Connection_Key.T

(*
		3.	extension types
*)

  datatype telnet_connection_extension = TCE of {flush: unit -> unit}
  type connection_extension = telnet_connection_extension

  type listen_extension = unit

  type session_extension = unit

(*
		4.	datatypes for the protocol signature
*)

  datatype connection =
      C of {send: Outgoing.T -> unit,
	    abort: unit -> unit,
	    extension: connection_extension}

  datatype listen =
      L of {stop: unit -> unit,
	    extension: listen_extension}

  datatype handler =
      H of Connection_Key.T -> {connection_handler: connection -> unit,
				data_handler: connection * Incoming.T -> unit,
				status_handler: connection * Status.T -> unit}

  datatype session =
      S of {connect: Address.T * handler -> unit,
	    listen: Pattern.T * handler * Count.T -> listen,
	    extension: session_extension}

(*
		5.	internal function higher_send
*)

  fun higher_send (lower_send, lower_conn) data =
       let val size = IO.size data
	   val buffer = Lower.Outgoing.uninitialized size
	   val urgency = ref IO.Regular
       in Trace.trace_print (fn _ => "sending " ^ IO.Data.makestring data);
	  IO.marshal ((buffer, urgency), data) 0w0;
	  case ! urgency of
	     IO.Regular => lower_send buffer
	   | IO.Urgent => send_urgent (lower_conn, buffer)
       end

(*
		6.	internal function make_higher_connection
*)

  fun make_higher_connection (lower_conn as
			      (Lower.C {send = lower_send,
					abort = lower_abort, ...})) =
       C {send = higher_send (lower_send, lower_conn), abort = lower_abort,
	  extension = TCE {flush = fn () => lower_flush lower_conn}}

(*
		7.	internal function lower_receive
*)

  fun lower_receive (higher_handler, previous, cr_state, urgency)
                    (connection, data) =
       let val (result, size) =
	         ((IO.unmarshal ((data, urgency, cr_state, ! previous), 0w0))
		  handle x =>
		          (Trace.print_handled
			     (x,
			      SOME ("lower_receive (" ^
				    Lower.Incoming.makestring data ^ ")"));
			   ([], 0w0)))
	   fun split_unparsed [] = ([], NONE)
	     | split_unparsed ((first as
				(IO.Data.Incomplete_Parse x)) :: rest) =
	        (rest, SOME x)
	     | split_unparsed (first :: rest) =
		let val (list, result) = split_unparsed rest
		in (first :: list, result)
		end
	    val (data_list, new_previous) = split_unparsed result
       in previous := new_previous;
	  higher_handler (make_higher_connection connection, data_list)
       end

(*
		8.	internal function lower_status
*)

  fun lower_status (status_handler, data_handler, previous,
		    cr_state) (conn, status) =
       case parse_urgent (conn, status) of
	  NONE => status_handler (make_higher_connection conn, status)
	| SOME data =>
	   lower_receive (data_handler, previous, cr_state,
			  IO.Urgent) (conn, data)

(*
		9.	internal function make_lower_handler
*)

  fun make_lower_handler {connection_handler, data_handler, status_handler} =
       let val unparsed = ref NONE
	   val cr_state = ref IO.No_CR
       in {connection_handler = connection_handler o make_higher_connection,
	   data_handler = lower_receive (data_handler, unparsed, cr_state,
					 IO.Regular),
	   status_handler =
	       lower_status (status_handler, data_handler, unparsed, cr_state)}
       end

(*
		10.	internal function make_connect
*)

  fun make_connect lower_connect (addr, H handler) =
       lower_connect (addr, Lower.H (make_lower_handler o handler))

(*
		11.	internal function make_listen
*)

  fun make_listen lower_listen (pattern, H handler, count) =
       let val Lower.L {stop, extension} =
                lower_listen (pattern, Lower.H (make_lower_handler o handler),
			      count)
       in L {stop = stop, extension = ()}
       end

(*
		12.	internal function lower_session
*)

  fun lower_session session_fun (Lower.S {connect = lower_connect,
					  listen = lower_listen,
					  extension = lower_extension}) =
       session_fun (S {connect = make_connect lower_connect,
		       listen = make_listen lower_listen,
		       extension = ()})
(*
		13.	function session
*)

  fun session (setup, session_fun) =
       Lower.session (setup, lower_session session_fun)

(*
		14.	function inetd_handler
*)

  fun inetd_handler (H upper_handler) =
       Lower.H (make_lower_handler o upper_handler)
 end

(*
		15.	functor Telnet
*)

functor Telnet (structure Lower: TELNET_PROTOCOL
		structure B: FOX_BASIS
		val debug_level: int ref option): TELNET =
 struct

  exception Illegal_Negotiation of Lower.IO.Data.Option.option_type

  fun makestring_exn (Illegal_Negotiation option) =
       SOME ("illegal option negotiation for option " ^
	     Lower.IO.Data.Option.makestring_option_type option)
    | makestring_exn x = Lower.X.makestring x

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "telnet.fun"
			   val makestring = makestring_exn)

  fun makestring_list makestring_single =
       let fun loop [] = ""
	     | loop [last] = makestring_single last
	     | loop (first :: rest) =
	        makestring_single first ^ ", " ^ loop rest
       in loop
       end

(*
		16.	structures Setup and Telnet_Setup
*)

  structure Telnet_Setup: TELNET_SETUP =
   struct
    type option = Lower.IO.Data.Option.option_type
    type lower_setup = Lower.Setup.T

    datatype option_setup =
       Request_Local of option
     | Request_Remote of option

    fun makestring_option_setup (Request_Local option) =
         "local request " ^ Lower.IO.Data.Option.makestring_option_type option
      | makestring_option_setup (Request_Remote option) =
         "remote request " ^ Lower.IO.Data.Option.makestring_option_type option

    type setup = option_setup list * lower_setup
    type T = setup

    fun makestring (options, lower_setup) =
         makestring_list makestring_option_setup options ^ "; " ^
	 Lower.Setup.makestring lower_setup

    fun equal ((reqs1, lower_setup1), (reqs2, lower_setup2)) =
         reqs1 = reqs2 andalso Lower.Setup.equal (lower_setup1, lower_setup2)

    fun hash (reqs, lower_setup) = Lower.Setup.hash lower_setup
   end
  structure Setup = Telnet_Setup

(*
		17.	structures Incoming and Outgoing
*)

  structure Incoming =			(* outgoing is the same *)
   struct
    type T = string
    fun makestring x = x

    val mk_byte = Word8.fromInt o B.V.Char.ord
    val mk_char = B.V.Char.chr  o Word8.toInt
 
    fun new array =
         let fun make NONE = []
	       | make (SOME (first, rest)) =
	          mk_char first :: make (Word_Array.W8.U_Big.F.next rest)
	     val start = Word_Array.W8.U_Big.F.next (Word_Array.to8 array)
	 in B.V.String.implode (make start)
	 end

    fun uninitialized size =
         let fun loop 0w0 = []
	       | loop count = #" " :: loop (count - 0w1)
	 in B.V.String.implode (loop size)
	 end

    val size = Word.fromInt o B.V.String.length

    fun sub (string, {start, length}) =
        let fun gen index = mk_byte (B.V.String.ordof
				     (string, Word.toInt (index + start)))
	in Word_Array.from8 (Word_Array.W8.U_Big.F.tabulate (gen, length))
	end

    exception Update_Not_Supported
    fun update _ = Trace.print_raise (Update_Not_Supported, NONE)

    val join = B.V.String.^

    fun split (string, position) =
         let val pos = Word.toInt position
	 in (B.V.String.substring (string, 0, pos),
	     B.V.String.substring (string, pos,
				   B.V.String.length string - pos))
	 end

    fun fold (string, f, init) =
	 f (sub (string, {start = 0w0, length = size string}), init)

    fun makestring_max (string, max_size) =
         if size string > max_size then 
	  B.V.String.substring (string, 0, Word.toInt max_size)
	 else string
   end

  structure Outgoing = Incoming

(*
		18.	structures Status and Telnet_Status
*)

  structure Telnet_Status: TELNET_STATUS =
   struct
    type option = Lower.IO.Data.Option.option_type
    type option_value = Lower.IO.Data.Option.option_value
    type lower_status = Lower.Status.T

    datatype acceptance = Yes | No
    datatype validity = On | Off

    datatype status =
       Lower_Status of lower_status

     | Remote_Request of {option: option, accept: acceptance -> unit}
     | Local_Request of {option: option, accept: acceptance -> unit}
     | Remote_Response of {option: option, now_valid: validity}
     | Local_Response of {option: option, now_valid: validity}
     | Remote_Disable of option
     | Local_Disable of option

     | Value of option_value
     | Synch
     | Interrupt_Process
     | Abort_Output
     | Go_Ahead
     | Erase_Char
     | Erase_Line

    type T = status

    fun makestring (Lower_Status status) = Lower.Status.makestring status
      | makestring (Remote_Request {option, accept}) =
         "remote request: " ^
	 Lower.IO.Data.Option.makestring_option_type option
      | makestring (Local_Request {option, accept}) =
         "local request: " ^
	 Lower.IO.Data.Option.makestring_option_type option
      | makestring (Remote_Response {option, now_valid}) =
         "remote response: " ^
	 Lower.IO.Data.Option.makestring_option_type option ^
	 (case now_valid of On => " => on" | _ => " => off")
      | makestring (Local_Response {option, now_valid}) =
         "local response: " ^
	 Lower.IO.Data.Option.makestring_option_type option ^
	 (case now_valid of On => " => on, accepted" | _ => " => off")
      | makestring (Remote_Disable option) =
         "remote disable: " ^
	 Lower.IO.Data.Option.makestring_option_type option
      | makestring (Local_Disable option) =
         "local disable: " ^
	 Lower.IO.Data.Option.makestring_option_type option
      | makestring (Value value) =
         "value: " ^ Lower.IO.Data.Option.makestring_option_value value
      | makestring Synch = "synch"
      | makestring Interrupt_Process = "interrupt-process"
      | makestring Abort_Output = "abort-output"
      | makestring Go_Ahead = "go-ahead"
      | makestring Erase_Char = "erase-char"
      | makestring Erase_Line = "erase-line"
   end
  structure Status = Telnet_Status

(*
		19.	other substructures
*)

  structure Option = Lower.IO.Data.Option

  structure Address = Lower.Address

  structure Pattern = Lower.Pattern

  structure Connection_Key = Lower.Connection_Key

  structure Count = Lower.Count

  structure X = Lower.X

  exception Already_Open of Connection_Key.T

(*
		20.	extension types
*)

  datatype telnet_connection_extension =
      TCE of {flush: unit -> unit,
	      send_value: Telnet_Status.option_value -> unit,
	      active: {option: Telnet_Status.option, remote: bool} -> bool,
	      pending: {option: Telnet_Status.option, remote: bool} -> bool,
	      request_local: Telnet_Status.option -> unit,
	      request_remote: Telnet_Status.option -> unit,
	      disable_local: Telnet_Status.option -> unit,
	      disable_remote: Telnet_Status.option -> unit,
	      send_erase_char: unit -> unit,
	      send_erase_line: unit -> unit,
	      send_synch: unit -> unit,
	      send_interrupt_process: unit -> unit,
	      send_abort_output: unit -> unit,
	      send_go_ahead: unit -> unit}
  type connection_extension = telnet_connection_extension

  type listen_extension = unit

  type session_extension = unit

(*
		21.	datatypes for the protocol signature
*)

  datatype connection =
      C of {send: Outgoing.T -> unit,
	    abort: unit -> unit,
	    extension: connection_extension}

  datatype listen =
      L of {stop: unit -> unit,
	    extension: listen_extension}

  datatype handler =
      H of Connection_Key.T -> {connection_handler: connection -> unit,
				data_handler: connection * Incoming.T -> unit,
				status_handler: connection * Status.T -> unit}

  datatype session =
      S of {connect: Address.T * handler -> unit,
	    listen: Pattern.T * handler * Count.T -> listen,
	    extension: session_extension}

(*
		22.	structure Option_State, option state machine
*)

  structure Option_State =
   struct
    datatype T =
        Inactive
      | Active
      | Requested
      | Off_Requested
      | Received_Request

    datatype option_event =
        Request
      | Request_Off
      | Accept
      | Reject
      | Receive_Request
      | Receive_Off

    datatype option_action =
        Report_Request
      | Report_On
      | Report_Off
      | Send_On
      | Send_Off
      | Send_Off_And_Report
      | Illegal_Event			(* e.g. request in wrong state *)
      | None

    fun transition (Inactive, Request) = (Requested, Send_On)
      | transition (Inactive, Request_Off) = (Inactive, Illegal_Event)
      | transition (Inactive, Accept) = (Inactive, Illegal_Event)
      | transition (Inactive, Reject) = (Inactive, Illegal_Event)
      | transition (Inactive, Receive_Request) =
         (Received_Request, Report_Request)
      | transition (Inactive, Receive_Off) = (Inactive, None)

      | transition (Active, Request) = (Active, Illegal_Event)
      | transition (Active, Request_Off) = (Off_Requested, Send_Off)
      | transition (Active, Accept) = (Active, Illegal_Event)
      | transition (Active, Reject) = (Active, Illegal_Event)
      | transition (Active, Receive_Request) = (Active, None)
      | transition (Active, Receive_Off) = (Inactive, Send_Off_And_Report)

      | transition (Requested, Request) = (Requested, Illegal_Event)
      | transition (Requested, Request_Off) = (Requested, Illegal_Event)
      | transition (Requested, Accept) = (Requested, Illegal_Event)
      | transition (Requested, Reject) = (Requested, Illegal_Event)
      | transition (Requested, Receive_Request) = (Active, Report_On)
      | transition (Requested, Receive_Off) = (Inactive, Report_Off)

      | transition (Off_Requested, Request) = (Off_Requested, Illegal_Event)
      | transition (Off_Requested, Request_Off) =
	 (Off_Requested, Illegal_Event)
      | transition (Off_Requested, Accept) = (Off_Requested, Illegal_Event)
      | transition (Off_Requested, Reject) = (Off_Requested, Illegal_Event)
      | transition (Off_Requested, Receive_Request) =
	 (Active, Send_Off_And_Report)
      | transition (Off_Requested, Receive_Off) = (Inactive, Report_Off)

      | transition (Received_Request, Request) = (Requested, Illegal_Event)
      | transition (Received_Request, Request_Off) = (Requested, Illegal_Event)
      | transition (Received_Request, Accept) = (Active, Send_On)
      | transition (Received_Request, Reject) = (Inactive, Send_Off)
      | transition (Received_Request, Receive_Request) =
	 (Received_Request, None)
(* note: because we can receive an off in state received_request, which
   we reach with an action of report_request, and that puts us back in
   inactive with no indication to the higher protocol, any call to accept
   or reject should be prepared for Illegal_Event. *)
      | transition (Received_Request, Receive_Off) = (Inactive, Send_Off)

    fun state_machine (state, event) =
         let val (new_state, action) = transition (! state, event)
	 in state := new_state;
	    action
	 end

    
    fun makestring Inactive = "inactive"
      | makestring Active = "active"
      | makestring Requested = "requested"
      | makestring Off_Requested = "off-requested"
      | makestring Received_Request = "received-request"

    fun makestring_event Request = "request"
      | makestring_event Request_Off = "request-off"
      | makestring_event Accept = "accept"
      | makestring_event Reject = "reject"
      | makestring_event Receive_Request = "receive-request"
      | makestring_event Receive_Off = "receive-off"

    fun makestring_action Report_Request = "report-request"
      | makestring_action Report_On = "report-on"
      | makestring_action Report_Off = "report-off"
      | makestring_action Send_On = "send-on"
      | makestring_action Send_Off = "send-off"
      | makestring_action Send_Off_And_Report = "send-off-and-report"
      | makestring_action Illegal_Event = "illegal-event"
      | makestring_action None = "none"

   end

  type telnet_state = (Lower.IO.Data.Option.option_type,
		       Option_State.T) B.Store.T ref


(*
		23.	internal values local_funs and remote_funs
*)

  val remote_funs = (Lower.IO.Data.Do, Lower.IO.Data.Don't,
		     Telnet_Status.Remote_Request,
		     Telnet_Status.Remote_Response,
		     Telnet_Status.Remote_Disable)

  val local_funs = (Lower.IO.Data.Will, Lower.IO.Data.Won't,
		    Telnet_Status.Local_Request,
		    Telnet_Status.Local_Response,
		    Telnet_Status.Local_Disable)

(*
		24.	internal function negotiate
*)

  fun negotiate (state, remote, send, event, status, upper, option,
		 print_raise) =
       let val old_state = case B.Store.look (! state, option) of
			      NONE =>
			       (state := B.Store.add (! state, option,
						      Option_State.Inactive);
			        Option_State.Inactive)
			    | SOME (store, option_state) =>
			       (state := store;
				option_state)
	   val (new_state, action) = Option_State.transition (old_state, event)
	   val (on_con, off_con, request, response, disable) =
	         if remote then remote_funs else local_funs
	   fun accept Telnet_Status.Yes =
	        negotiate (state, remote, send, Option_State.Accept, status,
			   upper, option, print_raise)
	     | accept Telnet_Status.No =
	        negotiate (state, remote, send, Option_State.Reject, status,
			   upper, option, print_raise)
       in state := B.Store.add (! state, option, new_state);
	  case action of
	     Option_State.Report_Request =>
	      status (upper, request {option = option, accept = accept})
	   | Option_State.Report_On =>
	      status (upper, response {option = option,
				       now_valid = Telnet_Status.On})
	   | Option_State.Report_Off =>
	      status (upper, response {option = option,
				       now_valid = Telnet_Status.Off})
	   | Option_State.Send_On => send [on_con option]
	   | Option_State.Send_Off => send [off_con option]
	   | Option_State.Send_Off_And_Report =>
	      (send [off_con option];
	       status (upper, response {option = option,
					now_valid = Telnet_Status.Off}))
	   | Option_State.Illegal_Event =>
	      if print_raise orelse Trace.trace_on () then
	       let val state = ("negotiate/" ^
				Option_State.makestring old_state ^ "(" ^
				Option_State.makestring_event event ^ ")")
	       in if print_raise then
		   Trace.print_raise (Illegal_Negotiation option, SOME state)
		  else
		   Trace.local_print
		      ("illegal negotiation in " ^ state ^ " for option " ^
		       Lower.IO.Data.Option.makestring_option_type option)
	       end
	      else ()
	   | Option_State.None =>
	      ()
       end

(*
		25.	internal function handle_single
*)

   fun handle_single (_, data, _, upper, _, Lower.IO.Data.Text string) =
        data (upper, string)
     | handle_single (_, _, _, _, _, Lower.IO.Data.Data data) =
	(* unexpected *)
        Trace.local_print "error, data_handler got 'Data' constructor"
     | handle_single (_, data, status, upper, _, Lower.IO.Data.Synch) =
	status (upper, Telnet_Status.Synch)
     | handle_single (_, _, _, _, _, Lower.IO.Data.No_Op) =
	()				(* no-op *)
     | handle_single (_, _, _, _, _, Lower.IO.Data.Break) =
	()				(* no-op for client *)
     | handle_single (_, _, status, upper, _,
		      Lower.IO.Data.Interrupt_Process) =
	status (upper, Telnet_Status.Interrupt_Process)
     | handle_single (_, _, status, upper, _, Lower.IO.Data.Abort_Output) =
	status (upper, Telnet_Status.Abort_Output)
     | handle_single (_, _, _, _, send, Lower.IO.Data.Are_You_There) =
	send [Lower.IO.Data.No_Op]
     | handle_single (_, _, status, upper, _, Lower.IO.Data.Erase_Char) =
	status (upper, Telnet_Status.Erase_Char)
     | handle_single (_, _, status, upper, _, Lower.IO.Data.Erase_Line) =
	status (upper, Telnet_Status.Erase_Line)
     | handle_single (_, _, status, upper, _, Lower.IO.Data.Go_Ahead) =
	status (upper, Telnet_Status.Go_Ahead)
     | handle_single ((_, remote_state), _, status, upper, send,
		      Lower.IO.Data.Will option) =
	negotiate (remote_state, true, send, Option_State.Receive_Request,
		   status, upper, option, false)
     | handle_single ((_, remote_state), _, status, upper, send,
		      Lower.IO.Data.Won't option) =
	negotiate (remote_state, true, send, Option_State.Receive_Off,
		   status, upper, option, false)
     | handle_single ((local_state, _), _, status, upper, send,
		      Lower.IO.Data.Do option) =
	negotiate (local_state, false, send, Option_State.Receive_Request,
		   status, upper, option, false)
     | handle_single ((local_state, _), _, status, upper, send,
		      Lower.IO.Data.Don't option) =
	negotiate (local_state, false, send, Option_State.Receive_Off,
		   status, upper, option, false)
     | handle_single (_, _, status, upper, _,
		      Lower.IO.Data.Option_Value value) =
	status (upper, Telnet_Status.Value value)
     | handle_single (_, _, _, _, _, Lower.IO.Data.Incomplete_Parse array) =
        Trace.local_print "Incomplete_Parse not handled by lower layer"

(*
		26.	internal function local_data_handler
*)

  fun local_data_handler (data_handler, status_handler,
			  mk_conn, state) (lower_conn, lower_data) =
       let val upper_conn = mk_conn lower_conn
	   val Lower.C {send, ...} = lower_conn
           fun data_loop [] = ()
	     | data_loop (first :: rest) =
	        ((handle_single (state, data_handler, status_handler,
				 upper_conn, send, first)
		  handle x => Trace.print_handled (x, SOME "data_loop"));
		 data_loop rest)
       in Trace.trace_print (fn _ => "received " ^
			     Lower.IO.Data.makestring lower_data);
	  data_loop lower_data
       end

(*
		27.	internal function local_status_handler
*)

  fun local_status_handler (status_handler, mk_conn)
                           (lower_conn, lower_status) =
       status_handler (mk_conn lower_conn,
		       Telnet_Status.Lower_Status lower_status)

(*
		28.	internal function local_connection_handler
*)

  fun local_connection_handler (connection_handler, status_handler,
				start_options, mk_conn,
				(local_state, remote_state)) lower_conn =
       let val upper_conn = mk_conn lower_conn
	   val Lower.C {send, abort, extension} = lower_conn
	   fun send_option (Telnet_Setup.Request_Local option) =
	        negotiate (local_state, false, send, Option_State.Request,
			   status_handler, upper_conn, option, false)
	     | send_option (Telnet_Setup.Request_Remote option) =
	        negotiate (remote_state, true, send, Option_State.Request,
			   status_handler, upper_conn, option, false)
       in Trace.debug_constant_string "sending initial options";
	  app send_option start_options;
	  Trace.debug_constant_string "initial options sent";
	  connection_handler upper_conn
       end

(*
		29.	internal function upper_handler
*)

  fun upper_handler (options, handler) key =
       let val {connection_handler, data_handler, status_handler} = handler key
	   val option_equal: (Lower.IO.Data.Option.option_type *
			      Lower.IO.Data.Option.option_type -> bool) = op=
	   val option_hash = Word.fromInt
	                   o Lower.IO.Data.Option.hash_option_type
	   val local_state = ref (B.Store.new (option_hash, option_equal))
	   val remote_state = ref (B.Store.new (option_hash, option_equal))
	   val state = (local_state, remote_state)
	   fun active {option, remote} =
                case B.Store.look (if remote then ! remote_state
				   else ! local_state, option) of
		   SOME (_, Option_State.Active) => true
		 | _ => false
	   fun pending {option, remote} =
                case B.Store.look (if remote then ! remote_state
				   else ! local_state, option) of
		   SOME (_, Option_State.Requested) => true
		 | SOME (_, Option_State.Off_Requested) => true
		 | SOME (_, Option_State.Received_Request) => true
		 | _ => false
	   fun request_local (upper_conn_fun, send) option =
	        negotiate (local_state, false, send, Option_State.Request,
			   status_handler, upper_conn_fun (), option, true)
	   fun request_remote (upper_conn_fun, send) option =
	        negotiate (remote_state, true, send, Option_State.Request,
			   status_handler, upper_conn_fun (), option, true)
	   fun disable_local (upper_conn_fun, send) option =
	        negotiate (local_state, false, send, Option_State.Request,
			   status_handler, upper_conn_fun (), option, true)
	   fun disable_remote (upper_conn_fun, send) option =
	        negotiate (remote_state, true, send, Option_State.Request,
			   status_handler, upper_conn_fun (), option, true)
	   fun upper_send send text = send [Lower.IO.Data.Text text]
	   val upper_conn_cache = ref NONE
	   fun upper_extension (conn as
				(Lower.C {send, abort,
					  extension = Lower.TCE {flush}})) =
	        TCE {flush = flush,
		     send_value =
		         (fn v => send [Lower.IO.Data.Option_Value v]),
		     active = active,
		     pending = pending,
		     request_local =
		         request_local (fn _ => mk_conn conn, send),
		     request_remote =
		         request_remote (fn _ => mk_conn conn, send),
		     disable_local =
		         disable_local (fn _ => mk_conn conn, send),
		     disable_remote =
		         disable_remote (fn _ => mk_conn conn, send),
		     send_erase_char =
		         (fn _ => send [Lower.IO.Data.Erase_Char]),
		     send_erase_line =
		         (fn _ => send [Lower.IO.Data.Erase_Line]),
		     send_synch = (fn _ => send [Lower.IO.Data.Synch]),
		     send_interrupt_process =
		         (fn _ => send [Lower.IO.Data.Interrupt_Process]),
		     send_abort_output =
		         (fn _ => send [Lower.IO.Data.Abort_Output]),
		     send_go_ahead = (fn _ => send [Lower.IO.Data.Go_Ahead])}
	   and mk_conn (conn as (Lower.C {send, abort, extension})) =
	        case ! upper_conn_cache of
		   SOME conn => conn
		 | NONE =>
		    let val conn = C {send = upper_send send, abort = abort,
				      extension = upper_extension conn}
		    in upper_conn_cache := SOME conn;
		       conn
		    end
	   val local_conn = local_connection_handler
	                        (connection_handler, status_handler, options,
				 mk_conn, state)
	   val local_data = local_data_handler (data_handler, status_handler,
						mk_conn, state)
	   val local_status = local_status_handler (status_handler, mk_conn)
       in {connection_handler = local_conn, data_handler = local_data,
	   status_handler = local_status}
       end

(*
		30.	internal function upper_connect
*)

  fun upper_connect (connect, options) (address, H handler) =
       connect (address, Lower.H (upper_handler (options, handler)))

(*
		31.	internal function upper_listen
*)

  fun upper_listen (listen, options) (address, H handler, count) =
       let val l = listen (address, Lower.H (upper_handler (options, handler)),
			   count)
	   val Lower.L {stop, extension} = l
       in L {stop = stop, extension = ()}
       end

(*
		32.	internal function upper_session_handler
*)

  fun upper_session_handler (options, session_handler)
                            (Lower.S {connect, listen, extension}) =
       session_handler (S {connect = upper_connect (connect, options),
			   listen = upper_listen (listen, options),
			   extension = ()})
       before Trace.trace_constant_string "session_handler exiting"

(*
		33.	function session
*)

  fun session ((options, setup), session_handler) =
       Lower.session (setup, upper_session_handler (options, session_handler))
       before Trace.trace_constant_string "session completed"

(*
		34.	function inetd_handler
*)

  fun inetd_handler options (H handler) =
       Lower.inetd_handler (Lower.H (upper_handler (options, handler)))

(*
		35.	structure Lower

	Put it at the end to avoid aliasing with the lower telnet_basic
	structure.
*)

  structure Lower = Lower.Lower

 end
