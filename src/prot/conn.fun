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
	1.	functor Connection
	2.	local types
	3.	local state
	4.	function init
	5.	function finalize
	6.	function state
	7.	function initialized
	8.	function create
	9.	function get
	10.	function dispatch
	11.	function status
	12.	function remove
	13.	function set_handler
	14.	function connections
	15.	function lower_key_count
	16.	function start
	17.	function stop
	18.	function passive
	19.	function passives

	iii.	RCS Log

$Log: conn.fun,v $
Revision 1.6  1995/03/12  17:58:17  esb
adapted to new trace.sig.

Revision 1.5  1995/03/07  23:52:11  esb
updated tracing.

Revision 1.4  1995/01/22  00:44:03  esb
fixed a storage leak by no longer storing all passive connections,
which is a bug.  This bug will be fixed with the new PROTOCOL signature.

Revision 1.3  1994/09/30  17:05:06  esb
added lower_key_count.

Revision 1.2  1994/08/12  06:19:22  esb
added set_handler.

Revision 1.1  1994/08/02  20:36:34  esb
Initial revision


	1.	functor Connection
*)

functor Connection (type protocol_state
		    type key
		    type connection
		    type passive
		    type lower_key
		    type lower_connection
		    type message
		    type status
		    val key_hash: key -> int
		    val key_eq: key * key -> bool
		    val passive_hash: passive -> int
		    val passive_eq: passive * passive -> bool
		    val lower_hash: lower_key -> int
		    val lower_eq: lower_key * lower_key -> bool
		    val lower_close: lower_connection -> unit
		    structure Trace: TRACE
		    structure B: FOX_BASIS): CONNECTION =
 struct

  val local_print = Trace.local_print

  exception Initialization
  exception Open
  exception Missing

  fun report (name, x) =
       (Trace.trace_print (fn _ => "exception " ^ System.exn_name x ^
			           " raised by " ^ name);
	raise x)

(*
	2.	local types
*)

  type protocol_state = protocol_state
  type key = key
  type connection = connection
  type lower_connection = lower_connection
  type lower_key = lower_key
  type message = message
  type status = status

  datatype info = Connection of {conn: connection,
				 lower_key: lower_key,
				 lower_conn: lower_connection,
				 lower_ref: int,
				 data_handler: message -> unit,
				 status_handler: status -> unit}

  datatype internal_info = Internal of {conn: connection,
					lower_key: lower_key,
					lower_conn: lower_connection,
					lower_ref: int ref,
					data_handler: (message -> unit) ref,
					status_handler: (status -> unit) ref}

  type connection_store = (key, internal_info) B.Store.T

  type passive = passive

  type passive_value = (connection -> ((message -> unit) * (status -> unit)))
                     * (key * connection) list ref
                     * int ref option

  type passive_store = (passive, passive_value) B.Store.T

  type lower_store = (lower_key, lower_connection * int ref) B.Store.T

  datatype state = T of {init_count: int ref,
			 connections: connection_store ref,
			 passives: passive_store ref,
			 lower: lower_store ref,
			 protocol_state: protocol_state}

(*
  val threshold = 500

  fun passive_size ((_, (_, list, _)), previous_size) =
       previous_size + length (! list) + 1

  fun print_passive_size passives =
       let val size = B.Store.fold passive_size (! passives) 0
       in if size >= threshold then
	   local_print ("passive size is " ^ B.V.Integer.makestring size)
	  else ()
       end
*)
  fun print_passive_size _ = ()

(*
	3.	local state
*)

  val local_state = ref (NONE: state option)

(*
	4.	function init
*)

  fun init protocol_state =
       case ! local_state of
	  NONE =>
	   let val count = ref 1
	       val conn = ref (B.Store.new (key_hash, key_eq))
	       val passives = ref (B.Store.new (passive_hash, passive_eq))
	       val lower = ref (B.Store.new (lower_hash, lower_eq))
	   in local_state := SOME (T {init_count = count, connections = conn,
				      passives = passives, lower = lower,
				      protocol_state = protocol_state ()});
	      1
	   end
	| SOME (T {init_count, ...}) =>
	   (init_count := ! init_count + 1;
	    ! init_count)

(*
	5.	function finalize
*)

  fun finalize shutdown_function =
       case ! local_state of
	  NONE =>
	   (Trace.debug_constant_string "state already finalized";
	    0)
       | SOME (T {init_count, connections, lower, passives, protocol_state}) =>
	  if ! init_count <= 1 then
	   let fun close_single (_, (lower_conn, _)) = lower_close lower_conn
	   in B.Store.map close_single (! lower);
	      shutdown_function ();
	      local_state := NONE;
	      0
	   end
	  else
	   (init_count := ! init_count - 1;
	    ! init_count)

(*
	6.	function state
*)

  fun state () =
       case ! local_state of
	  NONE => report ("state", Initialization)
	| SOME (T {protocol_state, ...}) => protocol_state

(*
	7.	function initialized
*)

  fun initialized () = case ! local_state of NONE => false | _ => true

(*
	8.	function create
*)

  fun create (key, connection, lower_key, lower_connect,
	      handler, passive, max) = 
       case ! local_state of
	  NONE => report ("create", Initialization)
	| SOME (T {connections, lower, passives, ...}) =>
	   case B.Store.look (! connections, key) of
	      SOME _ => report ("create", Open)
	    | NONE =>
	       let fun connect lower_key =
		        ((lower_connect lower_key)
			 handle x => report ("create-lower", x))
		   val (lower_connection, refcnt) =
		         case B.Store.look (! lower, lower_key) of
			    NONE => (lower_connect lower_key, ref 1)
			  | SOME (_, (lower_connection, refcnt)) =>
			     (refcnt := ! refcnt + 1;
			      (lower_connection, refcnt))
		   fun zero (_, (_, _, SOME (ref 0))) = true
		     | zero _ = false
		   val _ = case max of
		              NONE => ()
			    | SOME (ref 0) => report ("create", Missing)
			    | SOME count =>
			       (count := ! count - 1;
				if ! count = 0 then
				 passives :=
				    B.Store.remove_selected (! passives, zero)
				else ())
		   fun noop _ = ()
		   val data_handler = ref noop
		   val status_handler = ref noop
		   val new = ((Internal {conn = connection,
					 lower_key = lower_key,
					 lower_conn = lower_connection,
					 lower_ref = refcnt,
					 data_handler = data_handler,
					 status_handler = status_handler})
			      handle x =>
			              (local_print ("error, exception " ^
						    System.exn_name x ^
						    " instantiating handlers");
				       report ("create", x)))
		in lower := B.Store.add (! lower, lower_key,
					 (lower_connection, refcnt));
		   connections := B.Store.add (! connections, key, new);
		   case passive of
		      NONE => ()
		    | SOME passive_list =>
     (* to avoid storage leaks, we only add the information to the passive
        list in those cases in which max = (SOME count).
	This is a violation of the spec, but is also the
	simplest way to prevent the storage leak, and the violation
	of the spec should not substantially affect anyone at this time. *)
     (* original code
		       passive_list := (key, connection) :: ! passive_list;
      *)
		       (case max of
			   NONE => ()
			 | _ =>
			    passive_list := (key, connection) ::
			                    ! passive_list);
	      (* the handlers can only be instantiated after the connection
	         exists in the database, otherwise the state may be
		 inconsistent. *)
		   print_passive_size passives;
		   let val (data, status) = handler connection
		   in data_handler := data;
		      status_handler := status
		   end
		end

(*
	9.	function get
*)

  fun get key = 
       case ! local_state of
	  NONE => report ("get", Initialization)
	| SOME (T {connections, ...}) =>
	   (case B.Store.look (! connections, key) of
	       NONE => report ("get", Missing)
	     | SOME (_, Internal {conn, lower_key, lower_conn, lower_ref,
				  data_handler, status_handler}) =>
		Connection {conn = conn, lower_key = lower_key,
			    lower_conn = lower_conn,
			    lower_ref = ! lower_ref,
			    data_handler = ! data_handler,
			    status_handler = ! status_handler})

(*
	10.	function dispatch
*)

  fun dispatch key =
       case ! local_state of
	  NONE => report ("dispatch", Initialization)
	| SOME (T {connections, ...}) =>
	   (case B.Store.look (! connections, key) of
	       NONE => report ("dispatch", Missing)
	     | SOME (store, Internal {data_handler, ...}) =>
		(connections := store;
		 ! data_handler))

(*
	11.	function status
*)

  fun status key =
       case ! local_state of
	  NONE => report ("status", Initialization)
	| SOME (T {connections, ...}) =>
	   (case B.Store.look (! connections, key) of
	       NONE => report ("status", Missing)
	     | SOME (store, Internal {status_handler, ...}) =>
		(connections := store;
		 ! status_handler))

(*
	12.	function remove
*)

  fun remove key =
       case ! local_state of
	  NONE => report ("remove", Initialization)
	| SOME (T {connections, lower, ...}) =>
	   (case B.Store.look (! connections, key) of
	       NONE => report ("remove", Missing)
	     | SOME (_, Internal {lower_ref, lower_key, lower_conn, ...}) =>
		(connections := B.Store.remove (! connections, key);
		 lower_ref := ! lower_ref - 1;
		 if ! lower_ref = 0 then
		  (lower := B.Store.remove (! lower, lower_key);
		   lower_close lower_conn)
		 else ()))

(*
	13.	function set_handler
*)

  fun set_handler (key, handler) =
       case ! local_state of
	  NONE => report ("new_handler", Initialization)
	| SOME (T {connections, ...}) =>
	   (case B.Store.look (! connections, key) of
	       NONE => report ("new_handler", Missing)
	     | SOME (_, Internal {conn, data_handler, status_handler, ...}) =>
		let val (new_data, new_status) = handler conn
		in data_handler := new_data;
		   status_handler := new_status
		end)

(*
	14.	function connections
*)

  fun connections () =
       case ! local_state of
	  NONE => report ("get", Initialization)
	| SOME (T {connections, ...}) =>
	   let fun fold_connections ((key, _), rest) = key :: rest
	   in B.Store.fold fold_connections (! connections) []
	   end

(*
	15.	function lower_key_count
*)

  fun lower_key_count key =
       case ! local_state of
	  NONE => report ("get", Initialization)
	| SOME (T {lower, ...}) =>
	   case B.Store.look (! lower, key) of
	      NONE => 0
	    | SOME (new_store, (_, refcount)) =>
	       (lower := new_store;
		! refcount)

(*
	16.	function start
*)

  fun start (passive, max, handler) =
       case ! local_state of
	  NONE => report ("start", Initialization)
	| SOME (T {passives, ...}) =>
	   (case B.Store.look (! passives, passive) of
	       SOME _ => report ("start", Open)
	     | NONE =>
	        let val stored_max =
	                 case max of NONE => NONE | SOME x => SOME (ref x)
		    val keys = ref ([]: (key * connection) list)
		    val value = (handler, keys, stored_max)
	        in passives := B.Store.add (! passives, passive, value);
		   print_passive_size passives;
		   keys
	        end)

(*
	17.	function stop
*)

  fun stop passive =
       case ! local_state of
	  NONE => report ("stop", Initialization)
	| SOME (T {passives, ...}) =>
	   passives := B.Store.remove (! passives, passive)

(*
	18.	function passive
*)

  fun passive passive =
       case ! local_state of
	  NONE => report ("passive", Initialization)
	| SOME (T {passives, ...}) =>
	   (case B.Store.look (! passives, passive) of
	       NONE => NONE
	     | SOME (store, value) => SOME value)

(*
	19.	function passives
*)

  fun passives () =
       case ! local_state of
	  NONE => report ("passives", Initialization)
	| SOME (T {passives, ...}) =>
	   let fun append ((passive, _), list) = passive :: list
	   in B.Store.fold append (! passives) []
	   end

 end (* struct *)
