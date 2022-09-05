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

	Telnet options and interface for handling them.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Overview
	1.	signature TELNET_OPTION
	2.	type option_type
	3.	type environment_variable
	4.	type option_value
	5.	makestring and hash functions

		iii.	RCS Log
	
$Log: telnetopt.sig,v $
Revision 1.1  1996/07/05  17:42:54  esb
Initial revision


		iv.	Overview

	The TELNET_OPTION signature defines Telnet options and the
	interface for handling options.  Options are controled using
	the option_request type, and the option_manager object
	provides a way of storing and changing options.

		1.	signature TELNET_OPTION
*)

signature TELNET_OPTION =
 sig

(* 
		2.	type option_type
*)

  datatype option_type =
      Binary				(* RFC 856 *)
    | Echo				(* RFC 857 *)
    | Suppress_Go_Ahead			(* RFC 858 *)
    | Status				(* RFC 859 *)
    | Timing_Mark			(* RFC 860 *)
    | Window_Size			(* RFC 1073 *)
    | Terminal_Speed			(* RFC 1079 *)
    | Terminal_Type			(* RFC 1091 *)
    | Remote_Flow_Control		(* RFC 1372 *)
    | X_Display_Location		(* RFC 1096 *)
    | Environment			(* RFC 1572 *)
    | Old_Environment			(* RFC 1408 *)
    | Unknown of Word8.word

(* 
		3.	type environment_variable
*)

  datatype environment_variable =
     (* well-known environment variables, RFC 1572 *)
      All_Vars | User_Name | Job | Account | Printer | System_Type | Display
     (* "User" environment variables *)
    | User of string

(* 
		4.	type option_value
*)

  datatype option_value =
      Window_Size_Is of {columns: int, rows: int}	(* RFC 1073 *)

    | Request_Terminal_Speed				(* RFC 1079 *)
    | Terminal_Speed_Is of {send: int, receive: int}	(* RFC 1079 *)

    | Request_Terminal_Type				(* RFC 1091 *)
    | Terminal_Type_Is of string			(* RFC 1091 *)

    | Disable_Flow_Control				(* RFC 1372 *)
    | Enable_Flow_Control				(* RFC 1372 *)
    | Flow_Control_Any					(* RFC 1372 *)
    | Flow_Control_Xon					(* RFC 1372 *)

    | Request_X_Display					(* RFC 1096 *)
    | X_Display_Is of {host: string, display: int,
		       screen: int option}		(* RFC 1096 *)

(* note that the following 3 may be used in conjunction with either
   Environment or Old_Environment; however, the subsequent 3 should
   only be seen with Old_Environment. *)
    | Request_Environment of environment_variable list	(* RFC 1572*)
      (* in the following two, the string option is NONE if the
         variable is undefined, and SOME "" if it is defined but
	 has no value (see RFC 1572, p. 2). *)
    | Environment_Is of (environment_variable * string option) list
    | Environment_Update of (environment_variable * string option) list

(* may be used in conjunction with Old_Environment. 
   If any one of these is received, the corresponding ones must be sent,
   otherwise the "correct" ones must be sent. *)
    | Old_Request_Environment of environment_variable list
      (* RFC 1408, 1571, 1572 *)
      (* in the following two, the string option is NONE if the
         variable is undefined, and SOME "" if it is defined but
	 has no value (see RFC 1408, p. 3). *)
    | Old_Environment_Is of (environment_variable * string option) list
    | Old_Environment_Update of (environment_variable * string option) list

(* may be used in conjunction with Old_Environment. 
   If any one of these is received, the corresponding ones must be sent,
   otherwise the "correct" ones must be sent. *)
    | Wrong_Request_Environment of environment_variable list
      (* RFC 1408, 1571 *)
      (* in the following two, the string option is NONE if the
         variable is undefined, and SOME "" if it is defined but
	 has no value (see RFC 1408, p. 3). *)
    | Wrong_Environment_Is of (environment_variable * string option) list
    | Wrong_Environment_Update of (environment_variable * string option) list

(*
		5.	makestring and hash functions
*)

  val makestring_option_type: option_type -> string
  val hash_option_type: option_type -> int

  val makestring_environment_variable: environment_variable -> string
  val makestring_environment_pair: environment_variable * string option
                                 -> string
  val makestring_environment_pairs: (environment_variable * string option) list
                                  -> string
  val makestring_option_value: option_value -> string

 end
