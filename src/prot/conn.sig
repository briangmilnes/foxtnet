(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	conn.fun: basic connection-handling for protocols.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature CONNECTION
	2.	exceptions
	3.	types
	4.	protocol control functions
	5.	connection functions
	6.	passive functions

	iii.	RCS Log

$Log: conn.sig,v $
Revision 1.3  1994/09/30  17:05:06  esb
added lower_key_count.

Revision 1.2  1994/08/12  06:19:22  esb
added set_handler.

Revision 1.1  1994/08/02  20:36:34  esb
Initial revision


	1.	signature CONNECTION
*)

signature CONNECTION =
 sig

(*
	2.	exceptions
*)

  exception Initialization
  exception Open
  exception Missing

(*
	3.	types
*)

  type protocol_state
  type key
  type connection
  type lower_key
  type lower_connection
  type passive
  type message
  type status

  datatype info = Connection of {conn: connection,
				 lower_key: lower_key,
				 lower_conn: lower_connection,
				 lower_ref: int,
				 data_handler: message -> unit,
				 status_handler: status -> unit}

(*
	4.	protocol control functions
*)

  val init: (unit -> protocol_state) -> int
  val finalize: (unit -> unit) -> int
  val state: unit -> protocol_state
  val initialized: unit -> bool

(*
	5.	connection functions

	Create creates a connection.
	If the key list is specified, it is augmented with this key.
	If the int ref is specified, it is decremented and,
	If the result is zero, the corresponding passive is stopped.
*)

  val create: key * connection
            * lower_key * (lower_key -> lower_connection)
            * (connection -> ((message -> unit) * (status -> unit)))
            * (key * connection) list ref option * int ref option -> unit

  val get: key -> info
  val dispatch: key -> message -> unit
  val status: key -> status -> unit

  val remove: key -> unit

  val set_handler: key * (connection -> ((message -> unit) * (status -> unit)))
                 -> unit

  val connections: unit -> key list

  val lower_key_count: lower_key -> int

(*
	6.	passive functions

	Start sets up a passive.

        Stop removes a passive from storage so it will no longer
	be returned by passive.

	Passive returns the data given to the corresponding start,
	if any, together with the key list for the passive.
*)

  val start: passive * int option
           * (connection -> ((message -> unit) * (status -> unit)))
           -> (key * connection) list ref
  val stop: passive -> unit
  val passive: passive
             -> ((connection -> ((message -> unit)
				 * (status -> unit)))
                 * (key * connection) list ref * int ref option) option

  val passives: unit -> passive list

 end (* struct *)
