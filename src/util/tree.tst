(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	A test functor for trees.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Tree

		iii.	RCS Log
	
$Log: tree.tst,v $
Revision 1.3  1995/06/20  17:37:47  esb
minor fix.

Revision 1.2  1994/12/05  22:08:56  esb
set into the standard Fox Net test form.

Revision 1.1  1994/09/23  19:56:33  milnes
Initial revision


		1.	functor Test_Tree
*)


functor Test_Tree (structure B : FOX_BASIS): TEST_STRUCTURE =
 struct

  val stringequal = op= : (string * string) -> bool

  structure T = Tree (type label = string 
		      val label_eq = stringequal 
                      fun makestring_label s = s
                      fun makestring_data i = ""
                      val null_data = 0
		      type data = int
		      structure V = B.V)

  fun tree_equal (NONE, NONE) = true
    | tree_equal (NONE, SOME s) = false
    | tree_equal (SOME s, NONE) = false
    | tree_equal (SOME (T.Node{alabel,data,zchildren}),
                  SOME (T.Node{alabel=l1,data=d1,zchildren=c1})) =
       stringequal (alabel,l1) andalso data=d1

  fun test_run () =
       let fun test_tree (string, v1, v2) =
	        (Fox_Basis.Test.test (string, (fn () => tree_equal (v1, v2))); 
		 v1)
	   val t1 = test_tree ("add root", T.new ("", 0),
			       SOME (T.Node {alabel = "", data = 0: T.data,
					     zchildren = []}))
	   val t2 = test_tree ("add edu", T.add (t1, ["", "edu"], 1),
			       SOME (T.Node {alabel = "", data = 0, 
					     zchildren = [T.Node
							  {alabel = "edu",
							   data = 1,
							   zchildren = []}]}))
	   val t3 = test_tree ("add cmu", T.add (t2, ["", "edu", "cmu"], 2),
			       SOME
			       (T.Node
				{alabel = "", data = 0,
				 zchildren =
				 [T.Node
				  {alabel = "edu", data = 1,
				   zchildren = [T.Node
						{alabel = "cmu",
						 data = 2,
						 zchildren = []}]}]}))
	   val t4 = test_tree ("add cs",
			       T.add (t3, ["","edu", "cmu", "cs"], 3),
			       SOME
			       (T.Node
				{alabel="", data=0,
				 zchildren=
				 [T.Node
				  {alabel="edu",data=1,
				   zchildren=
				   [T.Node
				    {alabel="cmu",data=2,
				     zchildren=
				     [T.Node {alabel="cs",data=3,
					      zchildren=[]}]}]}]}))
	   val t5 = test_tree ("add ri",
			       T.add (t4, ["","edu", "cmu", "ri"], 4),
			       SOME
			       (T.Node
				{alabel="",data=0,
				 zchildren=
				 [T.Node
				  {alabel="edu",data=1,
				   zchildren=
				   [T.Node
				    {alabel="cmu",data=2,
				     zchildren=
				     [T.Node
				      {alabel="ri",data=4,
				       zchildren=[]},
				      T.Node
				      {alabel="cs",data=3,
				       zchildren=[]}]}]}]}))
	   (* t' *)
	   val t6 = test_tree ("delete ri",
			       T.delete (t5, ["","edu", "cmu", "ri"]),
			       SOME
			       (T.Node
				{alabel="",data=0,
				 zchildren=
				 [T.Node
				  {alabel="edu",data=1,
				   zchildren=
				   [T.Node
				    {alabel="cmu",data=2,
				     zchildren=
				     [T.Node
				      {alabel="cs",data=3,
				       zchildren=[]}]}]}]}))
	   (* t' *)
	   val t7 = test_tree ("delete cmu from ri",
			       T.delete (t5, ["","edu", "cmu"]),
			       SOME
			       (T.Node
				{alabel="",data=0,
				 zchildren=[T.Node {alabel="edu",
						    data=1,zchildren=[]}]}))
       in ()
       end

  fun run () = 
       if B.Debug.include_tests then
	B.Test.tests ("Tree", 7, test_run)
       else ()

  val _ = if ! B.Debug.do_tests then run () else ()


 end 

structure Test_Tree = Test_Tree (structure B = Fox_Basis)
