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

	This file provides various signatures for use with Telnet, in
	particular the TELNET signature, which is a subsignature of
	PROTOCOL.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TELNET_PROTOCOL
	2.	signature TELNET_SETUP
	3.	signature TELNET_STATUS
	4.	signature TELNET

		iii.	RCS Log
	
$Log: telnet.sig,v $
Revision 1.2  1996/07/05  17:45:30  esb
major redefinition.


		1.	signature TELNET_PROTOCOL
*)

signature TELNET_PROTOCOL =
 sig
  include PROTOCOL

  structure Lower: PROTOCOL
  structure IO: TELNET_IO

  datatype telnet_connection_extension = TCE of {flush: unit -> unit}

  sharing type Setup.T = Lower.Setup.T
      and type Address.T = Lower.Address.T
      and type Pattern.T = Lower.Pattern.T
      and type Connection_Key.T = Lower.Connection_Key.T
      and type Incoming.T = Outgoing.T = IO.T
      and type Status.T = Lower.Status.T
      and type Count.T = Lower.Count.T

  sharing type connection_extension = telnet_connection_extension
      and type listen_extension = session_extension = unit

  val inetd_handler: handler -> Lower.handler
 end

(*
		2.	signature TELNET_SETUP
*)

signature TELNET_SETUP =
 sig
  include KEY

  type option
  type lower_setup

(* any options that are not requested at startup are automatically
   denied by this protocol.  All options which are requested at
   startup are automatically requested by this protocol, and the
   corresponding status will be reported when (if ever) the remote
   system replies. Request_Local is used to indicate the option
   applies to data sent by this end of the connection, Request_Remote
   applies to data sent by the other side. *)

  datatype option_setup =
      Request_Local of option
    | Request_Remote of option

  type setup = option_setup list * lower_setup

  sharing type T = setup
 end

(*
		3.	signature TELNET_STATUS
*)

signature TELNET_STATUS =
 sig
  include PRINTABLE

  type option
  type option_value
  type lower_status

  datatype acceptance = Yes | No
  datatype validity = On | Off

(* as above, remote and local identify the end of the connection that
   sends data under the specified option: all statuses are options
   that are sent by the remote host to request, confirm, or deny the
   usage of an option. *)

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

  sharing type T = status
 end

(*
		4.	signature TELNET

	This is very similar to TELNET_BASIC, but Incoming.T =
	Outgoing.T = string, and options are sent via a connection
	extension, received as status messages, and sent at
	initialization time. Also, there is no IO substructure, but
	there is an Option substructure.
*)

signature TELNET =
 sig
  include PROTOCOL

  structure Lower: PROTOCOL
  structure Option: TELNET_OPTION
  structure Telnet_Setup: TELNET_SETUP
  structure Telnet_Status: TELNET_STATUS

  (* raised when accepting or rejecting an option more than once, or
     requesting/disabling an option for which negotiation is still pending. *)
  exception Illegal_Negotiation of Option.option_type

  datatype telnet_connection_extension =
      TCE of {flush: unit -> unit,
	      send_value: Telnet_Status.option_value -> unit,
	      active: {option: Telnet_Status.option, remote: bool} -> bool,
	      pending: {option: Telnet_Status.option, remote: bool} -> bool,
      (* pending implies not active *)
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

  sharing type Setup.T = Telnet_Setup.T
      and type Address.T = Lower.Address.T
      and type Pattern.T = Lower.Pattern.T
      and type Connection_Key.T = Lower.Connection_Key.T
      and type Incoming.T = Outgoing.T = string
      and type Status.T = Telnet_Status.T
      and type Count.T = Lower.Count.T

  sharing type Telnet_Setup.option = Telnet_Status.option = Option.option_type
      and type Telnet_Status.option_value = Option.option_value
      and type Telnet_Setup.lower_setup = Lower.Setup.T
      and type Telnet_Status.lower_status = Lower.Status.T

  sharing type connection_extension = telnet_connection_extension

  val inetd_handler: Telnet_Setup.option_setup list
                   -> handler -> Lower.handler
 end
