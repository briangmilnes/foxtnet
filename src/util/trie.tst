(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A test file for tries.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Trie
	2.	structure Test_Trie

		iii.	RCS Log
	
$Log: trie.tst,v $
Revision 1.3  1994/08/02  20:11:12  esb
adapted to uncurried test.sig.

Revision 1.2  1994/06/28  15:33:16  milnes
Fixed some B.s that should have been some Fox_Basis.

Revision 1.1  1994/06/28  02:36:39  milnes
Initial revision

		1.	functor Test_Trie
*)


functor Test_Trie (structure Fox_Basis: FOX_BASIS) =

 struct
  
  val stringequal = op= : (string * string) -> bool

  structure T = Trie (type key = string 
		      val keyequal = stringequal 
		      type data = int
		      val dataequal = (op= : (int * int -> bool)))
  fun test_run () =
       let fun test_tries (string, v1, v2) =
	        (Fox_Basis.Test.test (string, (fn () => T.equal (v1, v2)));
		 v1)
	   val t = test_tries ("add root", T.new (), T.EmptyTrie)
	   val t = test_tries ("add edu", T.add (t, ["edu"], 1),
			       T.Trie {children = [("edu",
						    T.Trie {children = [],
							    data = [1]})],
				       data = []})
	   val t = test_tries ("add cmu", T.add (t, ["edu", "cmu"], 2),
			       T.Trie {children = [("edu",
						    T.Trie {children = [("cmu",
							      T.Trie {children = [], data = [2]})],
							    data = [1]})],
				       data = []})
	   val t = test_tries ("add cs", T.add (t, ["edu", "cmu", "cs"], 3),
			       T.Trie {children = [("edu",
						    T.Trie {children = [("cmu",
									 T.Trie {children = [("cs", T.Trie {children = [], data = [3]})], data = [2]})],
							    data = [1]})],
				       data = []})
	   val t = test_tries ("add ri", T.add (t, ["edu", "cmu", "ri"], 4),
			       T.Trie {children = [("edu",
						    T.Trie
						    {children = [("cmu",
								  T.Trie
								  {children = [("ri", T.Trie {children = [], data = [4]}),
									       ("cs", T.Trie {children = [], data = [3]})],
								   data = [2]})],
						     data = [1]})],
			       data = []})
	   val t = test_tries ("add ri again", T.add (t, ["edu", "cmu", "ri"], 5),
			       T.Trie
			       {children = [("edu",
					     T.Trie
					     {children = [("cmu",
							   T.Trie
							   {children = [("ri",
									 T.Trie {children = [], data = [5, 4]}),
									("cs", T.Trie {children = [], data = [3]})],
							    data = [2]})], data = [1]})], data = []})
	   val t = test_tries ("add mango", T.add (t, ["edu", "cmu", "cs",
						       "srv", "mango"], 6),
			       T.Trie
			       {children = [("edu",
					     T.Trie
					     {children = [("cmu",
							   T.Trie
							   {children = [("cs",
									 T.Trie
									 {children = [("srv",
										       T.Trie
										       {children = [("mango",
												     T.Trie
												     {children = [],
												      data = [6]})],
											
                                                                           data = []})],
									 data = [3]}),
							   ("ri",
							    T.Trie {children = [], data = [5, 4]})],
							   data = [2]})], data = [1]})], data = []})
	   val t = test_tries ("add banana", T.add (t, ["edu", "cmu", "cs", "srv", "banana"], 7),
			       T.Trie
			       {children = [("edu",
					     T.Trie
					     {children = [("cmu",
							   T.Trie
							   {children = [("cs",
									 T.Trie
									 {children = [("srv",
										       T.Trie
										       {children = [("banana",
												     T.Trie
												     {children = [],
												      data = [7]}),
												    ("mango",
												     T.Trie
												     {children = [],
												      data = [6]})],
										       data = []})],
									 data = [3]}),
							   ("ri",
							    T.Trie {children = [], data = [5, 4]})],
							   data = [2]})], data = [1]})], data = []})
	   val t = test_tries ("delete banana", T.delete (t, ["edu", "cmu",
							      "cs", "srv",
							      "banana"], ~2), t)
	   val t = test_tries ("delete banana", T.delete (t, ["edu", "cmu",
							      "cs", "srv",
							      "banana"], 7),
			       T.Trie
			       {children = [("edu",
					     T.Trie
					     {children = [("cmu",
							   T.Trie
							   {children = [("cs",
									 T.Trie
									 {children = [("srv",
										       T.Trie
										       {children = [("mango",
												     T.Trie
												     {children = [],
												      data = [6]})],
											data = []})],
									 data = [3]}),
							   ("ri",
							    T.Trie {children = [], data = [5, 4]})],
							   data = [2]})], data = [1]})], data = []})
	   val t = test_tries ("delete srv", T.delete (t, ["edu", "cmu", "cs",
							   "srv", "mango"], 6),
			       T.Trie
			       {children = [("edu",
					     T.Trie
					     {children = [("cmu",
							   T.Trie
							   {children = [("cs",
									 T.Trie
									 {children = [("srv",
										       T.Trie
										       {children = [],
											data = []})],
									  data = [3]}),
							   ("ri",
							    T.Trie {children = [], data = [5, 4]})],
							   data = [2]})], data = [1]})], data = []})
	   val t = test_tries ("cmu", T.delete (t, ["edu", "cmu"], 2),
			       T.Trie
			       {children = [("edu",
					     T.Trie
					     {children = [("cmu",
							   T.Trie
							   {children = [("cs",
									 T.Trie
									 {children = [("srv",
										       T.Trie
										       {children = [],
											data = []})],
									  data = [3]}),
							   ("ri",
							    T.Trie {children = [], data = [5, 4]})],
							   data = []})], data = [1]})], data = []})
       in ()
       end
 
  fun run () = 
       if Fox_Basis.Debug.include_tests then
	Fox_Basis.Test.tests ("T.Trie", 12, test_run)
       else ()

  val _ = if ! Fox_Basis.Debug.do_tests then run () else ()

 end

(*
		2.	structure Test_Trie
*)

structure Test_Trie = Test_Trie (structure Fox_Basis = Fox_Basis)

