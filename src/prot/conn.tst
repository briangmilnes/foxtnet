(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test code to test the connection utility.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Connection
	2.      structure Test_Connection

		iii.	RCS Log
	
$Log: conn.tst,v $
Revision 1.4  1995/03/12  17:58:17  esb
adapted to new trace.sig.

Revision 1.3  1995/03/07  23:52:16  esb
updated tracing.

Revision 1.2  1994/09/30  17:05:06  esb
added lower_key_count.

Revision 1.1  1994/08/02  20:36:34  esb
Initial revision


		1.	functor Test_Connection
*)

functor Test_Connection (structure B: FOX_BASIS
			 val do_prints: bool): TEST_STRUCTURE =
 struct

  val debug_level = SOME (ref (if do_prints then 2 else 1))
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "conn.tst")
  val local_print = Trace.local_print

  fun key_hash key = key
  fun key_eq (a, b: int) = a = b

  fun passive_hash true = 1
    | passive_hash false = 0
  fun passive_eq (a, b: bool) = a = b

  fun lower_hash lower = truncate lower
  fun lower_eq (a, b: real) = a = b
  fun lower_close NONE = ()
    | lower_close (SOME x) = local_print "closing lower connection"

  structure S = Connection (type protocol_state = string
			    type key = int
                            type connection = unit
			    type passive = bool
			    type lower_key = real
			    type lower_connection = int option
			    type message = int list
			    type status = bool list 
			    val key_hash = key_hash
			    val key_eq = key_eq
			    val passive_hash = passive_hash
			    val passive_eq = passive_eq
			    val lower_hash = lower_hash
			    val lower_eq = lower_eq
			    val lower_close = lower_close
			    structure Trace = Trace
			    structure B = B)

  datatype 'a result = Res of 'a
                     | Exn of string

  fun test_same (actual, expected) () = expected = actual

  fun test_conn (Res _, Exn _) () = false
    | test_conn (Exn _, Res _) () = false
    | test_conn (Exn actual, Exn expected) () = expected = actual
    | test_conn (Res (S.Connection {conn, lower_key, lower_conn, lower_ref,
				    data_handler, status_handler}),
		 Res (S.Connection {conn = c, lower_key = k, lower_conn = l,
				    lower_ref = r, data_handler = d,
				    status_handler = h})) () =
       conn = c andalso lower_key = k andalso lower_ref = r

  fun run_tests () =
       let fun send1 _ = local_print "send called"
           fun lower_open _ = NONE
	   fun noop _ = ()
	   val test1 = S.Connection {conn = (), lower_key = 3.14,
				     lower_conn = NONE, 
				     lower_ref = 1, data_handler = noop,
				     status_handler = noop}
           val test2 = {conn = (), lower_conn = 2.71, lower_ref = ref 3,
			send = fn _ => local_print "send function called"}
           val test3 = {conn = (), lower_conn = 1.41, lower_ref = ref 2,
			send = fn _ => local_print "send fun called"}
	   fun wrap (f, arg) =
	        ((Res (f arg))
		 handle x => Exn (System.exn_name x))
	   fun lower _ = NONE
	   fun handler _ = (noop, noop)
       in B.Test.test ("init 1", test_same (S.init (fn _ => "hello"), 1));
          B.Test.test ("init 2", test_same (S.init (fn _ => "world"), 2));
	  B.Test.test ("finalize 1", test_same (S.finalize (fn _ => ()), 1));
	  B.Test.test ("finalize 2", test_same (S.finalize (fn _ => ()), 0));
	  B.Test.test ("finalize 3", test_same (S.finalize (fn _ => ()), 0));
	  B.Test.test ("get", test_conn (wrap (S.get, 1),
					 Exn "Initialization"));
	  B.Test.test ("create", test_same (wrap (S.create,
						  (22, (), 3.33, lower,
						  handler, NONE, NONE)),
					    Exn "Initialization"));
	  B.Test.test ("start", test_same (wrap (S.start,
						 (true, NONE, handler)),
					   Exn "Initialization"));
	  B.Test.test ("remove", test_same (wrap (S.remove, 77),
					    Exn "Initialization"));
	  B.Test.test ("init 3", test_same (S.init (fn _ => "hello"), 1));
	  let val p_true = S.start (true, NONE, handler)
	      val p_false = S.start (false, SOME 1, handler)
	  in B.Test.test ("stop", test_same (S.stop true, ()));
	     B.Test.test ("passives", test_same (S.passives (),
						 [false]));
	     B.Test.test ("create 1",
			  test_same (S.create (99, (), 3.14, lower_open,
					       handler, NONE, NONE), ()));
	     B.Test.test ("get 1", test_conn (wrap (S.get, 99), Res test1));
	     B.Test.test ("lower_key_count",
			  test_same (S.lower_key_count 3.14, 1));
	     B.Test.test ("lower_key_count",
			  test_same (S.lower_key_count 3.16, 0));
	     B.Test.test ("remove", test_same (S.remove 99, ()))
	  end
       end

  fun run () =
       if B.Debug.include_tests then
	B.Test.tests ("Conn", 17, run_tests)
       else ()

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		2. structure Test_Connection
*)

structure Test_Connection = Test_Connection (structure B = Fox_Basis
					     val do_prints = false)




