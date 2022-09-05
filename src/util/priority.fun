(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Chris Okasaki (Chris.Okasaki@cs.cmu.edu
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	priority.fun: functor Priority. A priority queue is used
	to store and retrieve objects based on keys which have a
	partial order. The only operation a key MUST support is less.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Priority_Queue
	2.	types, exception
	3.	functions new, size, empty
	4.	internal function less'
	5.	internal function node
	6.	internal function merge
	7.	internal function normalize
	8.      internal function sort
	9.	function add
	10.	function first
	11.	function next
	12.	function pop
	13.	function cleanup
	14.	function fold
	15.	function makestring

		iii.	RCS Log
	
$Log: priority.fun,v $
Revision 1.12  1996/06/21  15:34:58  cokasaki
minor optimizations

Revision 1.11  1996/06/11  03:35:50  esb
new version by Chris Okasaki.

Revision 1.10  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.9  1994/01/13  16:18:14  milnes
Updated on the path to updating coroutines with a delete.

Revision 1.8  93/12/02  03:16:32  esb
The post contained adjacent stars and parens.

Revision 1.7  1993/12/02  02:34:00  esb
added a lengthy post from the net indicating a better implementation strategy.

Revision 1.6  1993/11/16  15:32:20  esb
added comments, and provided and proved an invariant.

Revision 1.5  1993/09/13  22:07:58  cline
deleted '#'s from RCS log

Revision 1.4  1993/07/26  14:01:00  esb
Undid the previous change, since it is incompatible with a priority queue
of continuations, as required in coroutine.

Revision 1.3  1993/07/26  13:56:42  esb
made object and T eqtypes.

Revision 1.2  1993/07/20  15:05:27  esb
Changed the interface to "next" so returns option instead of exception.

Revision 1.1  1993/07/20  14:29:16  esb
Initial revision


	1.	functor Priority_Queue

	Imperative priority queues using leftist heaps.

	There are three invariants on trees:

	  1. Trees are heap-ordered: the key in any node is less than
	     or equal to the key in any descendant.  Nodes whose keys
	     have been deleted are ignored by the heap order.

	  2. The integer in each node measures the right height of
	     that node (i.e., the length of the right spine).  The
	     right height of any left child is at least as large as
	     the right height of its right sibling.

	  3. The root of a tree never contains NONE.

	Keys are deleted lazily by simply setting the appropriate
	option ref to NONE.  This design was chosen over the perhaps
	simpler approach of keeping a bool ref in each node and
	setting it to false when the node is deleted.  The advantage
	of the option ref is preventing space leaks when the keys are
	large.  It is still possible to have a space leak if the queue
	contains very many deleted nodes.  If this is a concern, call
	cleanup regularly.  This routine does "garbage collection" on
	queues.

*)

functor Priority_Queue (type key
                        val less : key * key -> bool): PRIORITY_QUEUE =
struct

(*
	2.	types, exception
*)

  type key = key

  datatype Tree = Empty | Node of key option ref * int * Tree * Tree

  type T = (Tree * int) ref

  exception Not_In_Queue

(*
	3.	functions new, size, empty
*)

  fun new () = ref (Empty, 0)

  fun size (ref (_, n)) = n

  fun empty (ref (_, n)) = (n = 0)

(*
	4.	internal function less'

	smart version of less that deals with option refs.  NONE is
	considered to be less than any other element.
*)

  local

   fun less' (_, ref NONE) = false
     | less' (ref NONE, _) = true
     | less' (ref (SOME key1), ref (SOME key2)) = less (key1, key2)

(*
	5.	internal function node

	build a node and swap children as needed to maintain Invariant 2
*)

   fun node (k, Empty, a) = Node (k, 1, a, Empty)
     | node (k, a, Empty) = Node (k, 1, a, Empty)
     | node (k, a as Node (_, ra, _, _), b as Node (_, rb, _, _)) =
        if ra >= rb then Node (k, rb + 1, a, b)
                    else Node (k, ra + 1, b, a)

(*
	6.	internal function node

	merge two trees into a single tree
*)

   fun merge (Empty, a) = a
     | merge (a, Empty) = a
     | merge (a as Node (ka, _, a1, a2), b as Node (kb, _, b1, b2)) =
        if less' (ka, kb) then node (ka, a1, merge (a2, b))
                          else node (kb, b1, merge (a, b2))

(*
	7.	internal function normalize

	restore Invariant 3
*)

   fun normalize (Node (ref NONE, _, a, b)) =
        let (* find all SOME nodes whose ancestors all contain NONE. *)
            (* Any of these could contain the overall minimum element. *)
            fun findCandidates (Empty, ts) = ts
              | findCandidates (t as Node (ref (SOME _), _, _, _), ts) =
	         t :: ts
              | findCandidates (t as Node (ref NONE, _, a, b), ts) =
                 findCandidates (a, findCandidates (b, ts))
            (* merge candidate trees in pairs until only one remains *)
	    fun mergePairs (t1 :: t2 :: ts) = merge (t1, t2) :: mergePairs ts
	      | mergePairs ts = ts
	    fun mergeAll [] = Empty
	      | mergeAll [t] = t
	      | mergeAll ts = mergeAll (mergePairs ts)
	in mergeAll (findCandidates (a, findCandidates (b,[]))) end
     | normalize tree = tree  (* Empty or Node (ref SOME,...) *)

(*
        8.      internal function sort

        linearize tree and remove NONEs
*)

   fun sort Empty = Empty
     | sort (Node (ref NONE, _, a, b)) = sort (merge (a, b))
     | sort (Node (k, _, a, b)) = Node (k, 1, sort (merge (a, b)), Empty)

(*
	9.	function add
*)

  in
   fun add (p as ref (tree, n), key) =
        let val k = ref (SOME key)
            fun ins Empty = Node (k, 1, Empty, Empty)
	      | ins (Node (k' as ref NONE, _, a, b)) =
		 node (k', a, ins b)
	      | ins (t as Node (k' as ref (SOME key'), _, a, b)) =
		 if less (key', key) then node (k', a, ins b)
                                     else Node (k, 1, t, Empty)
            fun delete () =
                  case ! k of
                     NONE => raise Not_In_Queue
		   | SOME _ => let val (tree, n) = ! p
			       in k := NONE;
                                  p := (normalize tree, n - 1)
			       end
        in p := (ins tree, n + 1);
           delete
        end

(*
	10.	function first
*)

   fun first (ref (Empty, _)) = NONE
     | first (ref (Node (ref someKey, _, _, _), _)) = someKey

(*
	11.	function next
*)

   fun next (ref (Empty, _)) = NONE
     | next (p as ref (Node (k as ref someKey, _, a, b), n)) =
        (k := NONE;  (* set to NONE so know when to raise Not_in_Queue *)
         p := (normalize (merge (a, b)), n - 1);
         someKey)

(*
	12.	function pop
*)

   fun pop (ref (Empty, _)) = ()
     | pop (p as ref (Node (k, _, a, b), n)) = (* k is ref (SOME key) *)
        (k := NONE;  (* set to NONE so know when to raise Not_in_Queue *)
         p := (normalize (merge (a, b)), n-1))

(*
	13.	function cleanup
*)

   fun cleanup (p as ref (tree, n)) =
        let fun clean Empty = Empty
              | clean (Node (k as ref (SOME _), _, a, b)) =
                 node (k, clean a, clean b)
              | clean tree = clean (normalize tree)
        in p := (clean tree, n) end

(*
        14.     function fold
*)

   fun fold f (p as ref (tree, n)) x =
        let val tree' = sort tree
            fun foldf (Node (ref (SOME key), _, a, b), x) = (* b is Empty *)
                 foldf (a, f (key, x))
              | foldf (_, x) = x  (* _ is Empty *)
        in p := (tree', n);
           foldf (tree', x)
        end

(*
        15.     function makestring
*)

   fun makestring (p as ref (tree, n), toString, sep) =
        let val tree' = sort tree
            fun fold (Node (ref (SOME key), _, a, b), rest) = (* b is Empty *)
                  (sep :: toString key :: fold (a, rest))
              | fold (_, rest) = rest  (* _ is Empty *)
        in p := (tree', n);
           case tree' of
             Node (ref (SOME key), _, a, _) =>
              concat (toString key :: fold (a, []))
           | _ => ""
        end

  end (* local *)
end
