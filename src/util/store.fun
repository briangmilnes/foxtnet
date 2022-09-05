(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	store.sml: functor Store which, given a key type, a value type,
	and a hash function on the keys, produces a STORE structure.

	This implementation uses tries, that is, trees where successively
	MORE significant bits of the key's hash value are used to index
	successive layers. Each node of a tree is either internal, storing
	two subtrees, or terminal, storing either one key-value pair, or a
	list of key-value pairs. A list is needed if two different keys
	have the same hash value.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Store_Exceptions
	2.	functor Store
	3.	datatypes ('key, 'value) node and ('key, 'value) T
	4.	internal function safe_hash
	5.	debugging function check_tree
	6.	function new
	7.	function remove
	8.	function remove_selected
	9.	function add
	10.	function look
	11.	function find
	12.	function size
	13.	function empty
	14.	function map
	15.	function fold
	16.	function makestring
	17.	functor Monomorphic_Store
	18.	datatypes ('key, 'value) node and ('key, 'value) T
	19.	internal function safe_hash
	20.	debugging function check_tree
	21.	function new
	22.	function remove
	23.	function remove_selected
	24.	function add
	25.	function look
	26.	function find
	27.	function size
	28.	function empty
	29.	function map
	30.	function app
	31.	function fold
	32.	function makestring
	33.	functor Imperative_Store
	34.	functor Imperative_Monomorphic_Store
	35.	functor Single_Store

	iii.	RCS Log
$Log: store.fun,v $
Revision 1.20  1997/02/13  00:36:35  esb
added imperative, monomorphic, and single stores.

Revision 1.19  1996/04/18  21:34:55  cline
converted hash from int to word

Revision 1.18  1996/02/07  19:21:20  cline
removed nulls from end of file

Revision 1.17  1996/02/06  23:41:23  esb
fixed a bug in remove_selected which violated some invariants.

Revision 1.16  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.15  1995/06/20  17:37:38  esb
minor fix.

Revision 1.14  1995/03/10  03:52:24  esb
adapted to new vendor.sig.

Revision 1.13  1994/09/29  21:11:47  cline
removed "signature TIMINGBOARD = TIMINGBOARD"

Revision 1.12  1994/09/29  20:16:45  cline
added "signature TIMINGBOARD = TIMINGBOARD" to keep SC happy

Revision 1.11  1994/06/16  16:51:43  danwang
Updated for functorized Fox_Basis

Revision 1.10  1994/05/10  07:41:53  esb
added empty, equality predicate in new.

Revision 1.9  94/04/06  23:06:20  esb
added remove_selected.

Revision 1.8  94/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.6  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.5  1994/01/13  16:18:50  milnes
Updated on the path to updating coroutines with a delete.

Revision 1.4  1993/10/06  02:31:08  esb
major redesign.

*)

(*
	1.	functor Store_Exceptions
*)

functor Store_Exceptions (structure V: VENDOR
			  val module_name: string) =
 struct

  exception Not_Present_In_Store

  exception Illegal_Store_Empty_Tlist
  exception Illegal_Store_Single_Tlist
  exception Illegal_Store_Same_Hash_Call
  exception Broken_Invariants

  fun makestring_exn Not_Present_In_Store =
       SOME "key not present in store for find operation"
    | makestring_exn Illegal_Store_Empty_Tlist =
       SOME ("unexpected illegal value: Tlist has empty list " ^
	     "(implementation error)")
    | makestring_exn Illegal_Store_Single_Tlist =
       SOME ("unexpected illegal value: Tlist has single element list " ^
	     "(implementation error)")
    | makestring_exn Illegal_Store_Same_Hash_Call =
       SOME ("unexpected illegal value: hash values should differ " ^
	     "(implementation error)")
    | makestring_exn Broken_Invariants =
       SOME "unexpected illegal value"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = V
			   val debug_level = NONE
			   val module_name = module_name
			   val makestring = makestring_exn)
 end


(*
	2.	functor Store
*)

functor Store (structure V: VENDOR): STORE =
 struct
  structure Exn = Store_Exceptions (structure V = V
				    val module_name = "store.fun")

  open Exn

(*
	3.	datatypes ('key, 'value) node and ('key, 'value) T
*)

  datatype ('key, 'value) node =
       Empty
     | Internal of ('key, 'value) node * ('key, 'value) node
     | Terminal of word * 'key * 'value
     | Tlist of word * ('key * 'value) list

  datatype ('key, 'value) T =
   T of ('key * 'value) option * ('key -> word) *
        ('key * 'key -> bool) * (('key, 'value) node)

(*
	4.	internal function safe_hash
*)

  fun safe_hash (hash, x) =
       (hash x handle e => Trace.print_raise_again (e, SOME "hash"))

(*
	5.	debugging function check_tree

	To use this for debugging, insert a call to check_tree to check
	every tree value returned; specifically, on functions "new",
	"remove", "remove_selected", "add", "look", and "map".
*)

  fun indent (0, s) = s ^ "\n"
    | indent (n, s) = " " ^ indent (n - 1, s)
  fun makestring_debug (d, Empty) = indent (d, "Empty")
    | makestring_debug (d, Terminal (hash, key, value)) =
       indent (d, Word.toString hash)
    | makestring_debug (d, Tlist (hash, values)) =
       indent (d, Word.toString hash ^ " x" ^
	       Integer.toString (length values))
    | makestring_debug (d, Internal (left, right)) =
       indent (d, "+") ^ makestring_debug (d + 1, left) ^
       makestring_debug (d + 1, right)

  local
   fun check_hash (index, depth, hash) =
        let val mask = Word.- (Word.<< (0w1, depth), 0w1)
        in Word.andb (index, mask) = Word.andb (hash, mask)
        end

   fun check_invariants (index, depth, Empty) = true
     | check_invariants (index, depth, Terminal (hash, _, _)) =
        check_hash (index, depth, hash)
     | check_invariants (index, depth, Tlist (hash, list)) =
        check_hash (index, depth, hash) andalso V.List.length list > 1
     | check_invariants (index, depth, Internal (left, right)) =
        check_invariants (index, Word.+ (depth, 0w1), left) andalso
        check_invariants (Word.orb (index, Word.<< (0w1, depth)),
			  Word.+ (depth, 0w1), right)

  in
   fun check_tree (string, tree) =
        if check_invariants (0w0, 0w0, tree) then tree
	else
	 (Trace.local_print ("tree returned by " ^ string ^
			     " breaks invariants:\n" ^
			     makestring_debug (0, tree));
	  Trace.print_raise (Broken_Invariants, SOME "check_tree"))
  end

(*
	6.	function new
*)

  fun new (hash, eq) = T (NONE, hash, eq, Empty)

(*
	7.	function remove
*)

  local

   fun remove_list (_, _, []) = []
     | remove_list (key, eq, (k, v) :: rest) =
        if eq (key, k) then rest
        else (k, v) :: remove_list (key, eq, rest)

   fun remove_tree (key, shifthed_hash, eq, Empty) = Empty
     | remove_tree (key, shifthed_hash, eq, Internal (left, right)) =
        let val even = Word.andb(shifthed_hash, 0w1) = 0w0
	    val new_shift = Word.>> (shifthed_hash, 0w1)
	    val new_tree =
	         if even then (remove_tree (key, new_shift, eq, left), right)
		 else         (left, remove_tree (key, new_shift, eq, right))
        in case new_tree of
	      (Empty, Empty) => Empty
	    | _ => Internal new_tree
        end
     | remove_tree (key, _, eq, (node as Terminal (_, k, _))) =
        if eq (k, key) then Empty else node
     | remove_tree (_, _, _, Tlist (_, [])) =
        Trace.print_raise (Illegal_Store_Empty_Tlist, SOME "remove_tree")
     | remove_tree (_, _, _, Tlist (_, (h :: []))) =
        Trace.print_raise (Illegal_Store_Single_Tlist, SOME "remove_tree")
     | remove_tree (key, _, eq, Tlist (hash_value, l)) =
        case remove_list (key, eq, l) of
	   [] => Empty
	 | (single_key, single_value) :: [] =>
	    Terminal (hash_value, single_key, single_value)
	 | new_list => Tlist (hash_value, new_list)

  in

   fun remove (T (cache, hash, eq, table), key) =
        T (NONE, hash, eq,
	   remove_tree (key, safe_hash (hash, key), eq, table))
  end (* local *)

(*
	8.	function remove_selected
*)

  local
   fun remove_list_selected (_, []) = []
     | remove_list_selected (f, head :: rest) =
       if f head then remove_list_selected (f, rest)
       else head :: remove_list_selected (f, rest)

   fun remove_tree_selected (f, Empty) = Empty
     | remove_tree_selected (f, Internal (left, right)) =
        (case (remove_tree_selected (f, left),
	       remove_tree_selected (f, right)) of
	    (Empty, Empty) => Empty
	  | pair => Internal pair)
     | remove_tree_selected (f, node as (Terminal (hash, key, value))) =
        if f (key, value) then Empty else node
     | remove_tree_selected (f, Tlist (hash, list)) =
        (case remove_list_selected (f, list) of
	    [] => Empty
	  | (key, value) :: [] => Terminal (hash, key, value)
	  | new_list => Tlist (hash, new_list))

  in

   fun remove_selected (T (cache, hash, eq, table), f) =
	T (NONE, hash, eq, remove_tree_selected (f, table))

  end (* local *)

(*
	9.	function add
*)

  local

   fun add_list (eq, pair, []) = pair :: []
     | add_list (eq, new_pair as (new_k, _), (old_pair as (k, _)) :: rest) =
       if eq (new_k, k) then new_pair :: rest
       else old_pair :: (add_list (eq, new_pair, rest))

   (* new_node takes a terminal node, its hash value, the shifted
      hash of the node to be added, the current depth in the tree
      (0 for a root node), and the parameters of the node to be added,
      and returns a new tree containing both the terminal node and
      the new node *)

   fun new_node (node, node_hash, shifted_hash, depth, hash, key, value, eq) =
    let val shifted_node = Word.>> (node_hash, depth)
	val node_bit = Word.andb (shifted_node, 0w1) = 0w1
	val hash_bit = Word.andb (shifted_hash, 0w1) = 0w1
    in if shifted_node = shifted_hash then
        (Trace.local_print ("shifted_node = " ^ Word.toString shifted_node ^
			    ", shifted_hash = " ^ Word.toString shifted_hash ^
			    ", node_hash = " ^ Word.toString node_hash ^
			    ", hash = " ^ Word.toString hash);
	 Trace.print_raise (Illegal_Store_Same_Hash_Call, SOME "new_node"))
       else
        case (node_bit, hash_bit) of
           (false, false) =>		(* both zero: create new left child *)
	    Internal (add_tree (node, Word.>> (shifted_hash, 0w1),
				depth + 0w1, hash, key, value, eq), Empty)
         | (true, true) =>		(* both one: create new right child *)
	    Internal (Empty, add_tree (node, Word.>> (shifted_hash, 0w1),
				       depth + 0w1, hash, key, value, eq))
	 | (true, false) =>		(* one, zero: split *)
	    Internal (Terminal (hash, key, value), node)
	 | (false, true) =>		(* zero, one: split *)
	    Internal (node, Terminal (hash, key, value))
    end

   and add_tree (Empty, _, _, hash, key, value, _) =
        Terminal (hash, key, value)
     | add_tree (Internal (left, right),
		 shifted_hash, depth, hash, key, value, eq) =
        if Word.andb (shifted_hash, 0w1) = 0w0 then
	 Internal (add_tree (left, Word.>> (shifted_hash, 0w1), depth + 0w1,
			     hash, key, value, eq), right)
        else
	 Internal (left, add_tree (right, Word.>> (shifted_hash, 0w1),
				   depth + 0w1, hash, key, value, eq))
      | add_tree ((node as Terminal (h, k, v)), shifted_hash, depth,
		  hash, key, value, eq) =
	 if eq (key, k) then Terminal (hash, key, value)
	 else if hash = h then
	  Tlist (hash, [(key, value), (k, v)])
         else
	  new_node (node, h, shifted_hash, depth, hash, key, value, eq)
      | add_tree ((n as Tlist (_, [])), _, _, _, _, _, _) =
	 Trace.print_raise (Illegal_Store_Empty_Tlist, SOME "add_tree")
      | add_tree ((n as Tlist (_, (_ :: []))), _, _, _, _, _, _) =
	 Trace.print_raise (Illegal_Store_Single_Tlist, SOME "add_tree")
      | add_tree ((node as Tlist (h, values as (_ :: _))), shifted_hash,
		  depth, hash, key, value, eq) =
	 if hash = h then Tlist (h, add_list (eq, (key, value), values))
         else new_node (node, h, shifted_hash, depth, hash, key, value, eq)

  in

   fun add (T (cache, hash, eq, table), key, value) =
        ((let val hash_value = safe_hash (hash, key)
              val new_tree = add_tree (table, hash_value, 0w0,
				       hash_value, key, value, eq)
          in T (SOME (key, value), hash, eq, new_tree)
	  end)
	   handle x => (Trace.local_print ("hash " ^
					   Word.toString (safe_hash
							  (hash, key)) ^
					   ", tree\n" ^
					   makestring_debug (0, table));
			Trace.print_raise_again (x, SOME "add")))

  end (* local *)

(*
	10.	function look
*)

  local
   fun look_list (_, _, []) = NONE
     | look_list (eq, key, ((k, v) :: rest)) =
       if eq (k, key) then SOME v else look_list (eq, key, rest)

   fun look_tree (_, _, _, _, Empty) = NONE
     | look_tree (eq, shifted_hash, hash, key, Internal (left, right)) =
        if Word.andb (shifted_hash, 0w1) = 0w0 then
	 look_tree (eq, Word.>> (shifted_hash, 0w1), hash, key, left)
        else
	 look_tree (eq, Word.>> (shifted_hash, 0w1), hash, key, right)
     | look_tree (eq, shifted_hash, hash, key, Terminal (_, k, v)) =
        if eq (key, k) then SOME v else NONE
     | look_tree (_, _, _, _, Tlist (_, [])) =
	Trace.print_raise (Illegal_Store_Empty_Tlist, SOME "look_tree")
     | look_tree (_, _, _, _, Tlist (_, _ :: [])) =
	Trace.print_raise (Illegal_Store_Single_Tlist, SOME "look_tree")
     | look_tree (eq, shifted_hash, hash, key, Tlist (h, l)) =
        if hash = h then look_list (eq, key, l)
        else NONE

   fun look_table (key, hash, eq, table) =
    let val hash_key = safe_hash (hash, key)
    in look_tree (eq, hash_key, hash_key, key, table)
    end

  in
   fun look (T (NONE, hash, eq, table), key) =
        let val res = look_table (key, hash, eq, table)
        in case res of
            SOME v => (SOME (T (SOME (key, v), hash, eq, table), v))
          | NONE => NONE
        end
     | look (store as (T ((SOME (cache_key, value)), hash, eq, table)), key) =
        if eq (cache_key, key) then SOME (store, value)
	else
	 let val res = look_table (key, hash, eq, table)
	 in case res of
	       SOME v => SOME (T (SOME (key, v), hash, eq, table), v)
	     | NONE => NONE
	 end
  end (* local *)

(*
	11.	function find
*)

  fun find (store, key) =
       case look (store, key) of
          NONE => raise Not_Present_In_Store
	| SOME v => v

(*
	12.	function size
*)

  fun size (T (_, _, _, table)) =
       let fun lsize Empty = 0
	     | lsize (Internal (left, right)) = (lsize left) + (lsize right)
	     | lsize (Terminal _) = 1
	     | lsize (Tlist (_, l)) = length l
       in lsize table
       end

(*
	13.	function empty
*)

  fun empty (T (_, _, _, Empty)) = true
    | empty _ = false

(*
	14.	function map
*)

  fun map f (T (cache, hash, eq, table)) =
       let fun lmap Empty = Empty
	     | lmap (Internal (left, right)) = Internal (lmap left, lmap right)
	     | lmap (Terminal (hash, key, value)) =
	        Terminal (hash, key, f (key, value))
             | lmap (Tlist (h, l)) =
	        Tlist (h, (List.map (fn (p as (k, v)) => (k, f p)) l))
       in T (NONE, hash, eq, lmap table)
       end

(*
	15.	function fold
*)

  fun fold f (T (_, hash, eq, table)) init = 
       let fun flatten Empty = []
	     | flatten (Internal (left, right)) =
		(flatten left) @ (flatten right)
	     | flatten (Terminal (hash, key, value)) = (key, value) :: []
	     | flatten (Tlist (hash, l)) = l
       in V.List.fold f (flatten table) init
       end

(*
	16.	function makestring
*)

  fun makestring (store, elstring, separator) =
       let fun foldstring (element, "") = elstring element
	     | foldstring (element, prev) =
	        (elstring element) ^ separator ^ prev
       in fold foldstring store ""
       end

 end (* struct *)

(*
	17.	functor Monomorphic_Store
*)

functor Monomorphic_Store (structure V: VENDOR
			   type key
			   type value
			   val eq: key * key -> bool
			   val hash: key -> word): MONOMORPHIC_STORE =
 struct

  structure Exn = Store_Exceptions (structure V = V
				    val module_name = "store.fun/monomorphic")

  open Exn

  type key = key
  type value = value

(*
	18.	datatypes ('key, 'value) node and ('key, 'value) T
*)

  datatype T =
       Empty
     | Internal of T * T
     | Terminal of word * key * value
     | Tlist of word * (key * value) list

(*
	19.	internal function safe_hash
*)

  fun safe_hash x =
       (hash x handle e => Trace.print_raise_again (e, SOME "hash"))

(*
	20.	debugging function check_tree

	To use this for debugging, insert a call to check_tree to check
	every tree value returned; specifically, on functions "new",
	"remove", "remove_selected", "add", "look", and "map".
*)

  fun indent (0, s) = s ^ "\n"
    | indent (n, s) = " " ^ indent (n - 1, s)
  fun makestring_debug (d, Empty) = indent (d, "Empty")
    | makestring_debug (d, Terminal (hash, key, value)) =
       indent (d, Word.toString hash)
    | makestring_debug (d, Tlist (hash, values)) =
       indent (d, Word.toString hash ^ " x" ^
	       Integer.toString (length values))
    | makestring_debug (d, Internal (left, right)) =
       indent (d, "+") ^ makestring_debug (d + 1, left) ^
       makestring_debug (d + 1, right)

  local
   fun check_hash (index, depth, hash) =
        let val mask = Word.- (Word.<< (0w1, depth), 0w1)
        in Word.andb (index, mask) = Word.andb (hash, mask)
        end

   fun check_invariants (index, depth, Empty) = true
     | check_invariants (index, depth, Terminal (hash, _, _)) =
        check_hash (index, depth, hash)
     | check_invariants (index, depth, Tlist (hash, list)) =
        check_hash (index, depth, hash) andalso V.List.length list > 1
     | check_invariants (index, depth, Internal (left, right)) =
        check_invariants (index, Word.+ (depth, 0w1), left) andalso
        check_invariants (Word.orb (index, Word.<< (0w1, depth)),
			  Word.+ (depth, 0w1), right)

  in
   fun check_tree (string, tree) =
        if check_invariants (0w0, 0w0, tree) then tree
	else
	 (Trace.local_print ("tree returned by " ^ string ^
			     " breaks invariants:\n" ^
			     makestring_debug (0, tree));
	  Trace.print_raise (Broken_Invariants, SOME "check_tree"))
  end

(*
	21.	function new
*)

  fun new () = Empty

(*
	22.	function remove
*)

  local

   fun remove_list (_, []) = []
     | remove_list (key, (k, v) :: rest) =
        if eq (key, k) then rest
        else (k, v) :: remove_list (key, rest)

   fun remove_tree (key, shifthed_hash, Empty) = Empty
     | remove_tree (key, shifthed_hash, Internal (left, right)) =
        let val even = Word.andb(shifthed_hash, 0w1) = 0w0
	    val new_shift = Word.>> (shifthed_hash, 0w1)
	    val new_tree =
	         if even then (remove_tree (key, new_shift, left), right)
		 else         (left, remove_tree (key, new_shift, right))
        in case new_tree of
	      (Empty, Empty) => Empty
	    | _ => Internal new_tree
        end
     | remove_tree (key, _, (node as Terminal (_, k, _))) =
        if eq (k, key) then Empty else node
     | remove_tree (_, _, Tlist (_, [])) =
        Trace.print_raise (Illegal_Store_Empty_Tlist, SOME "remove_tree")
     | remove_tree (_, _, Tlist (_, (h :: []))) =
        Trace.print_raise (Illegal_Store_Single_Tlist, SOME "remove_tree")
     | remove_tree (key, _, Tlist (hash_value, l)) =
        case remove_list (key, l) of
	   [] => Empty
	 | (single_key, single_value) :: [] =>
	    Terminal (hash_value, single_key, single_value)
	 | new_list => Tlist (hash_value, new_list)

  in

   fun remove (table, key) = remove_tree (key, safe_hash key, table)

  end (* local *)

(*
	23.	function remove_selected
*)

  local
   fun remove_list_selected ([], _) = []
     | remove_list_selected (head :: rest, f) =
       if f head then remove_list_selected (rest, f)
       else head :: remove_list_selected (rest, f)

  in

   fun remove_selected (Empty, f) = Empty
     | remove_selected (Internal (left, right), f) =
        (case (remove_selected (left, f), remove_selected (right, f)) of
	    (Empty, Empty) => Empty
	  | pair => Internal pair)
     | remove_selected (node as (Terminal (hash, key, value)), f) =
        if f (key, value) then Empty else node
     | remove_selected (Tlist (hash, list), f) =
        (case remove_list_selected (list, f) of
	    [] => Empty
	  | (key, value) :: [] => Terminal (hash, key, value)
	  | new_list => Tlist (hash, new_list))

  end (* local *)

(*
	24.	function add
*)

  local

   fun add_list (pair, []) = pair :: []
     | add_list (new_pair as (new_k, _), (old_pair as (k, _)) :: rest) =
       if eq (new_k, k) then new_pair :: rest
       else old_pair :: (add_list (new_pair, rest))

   (* new_node takes a terminal node, its hash value, the shifted
      hash of the node to be added, the current depth in the tree
      (0 for a root node), and the parameters of the node to be added,
      and returns a new tree containing both the terminal node and
      the new node *)

   fun new_node (node, node_hash, shifted_hash, depth, hash, key, value, eq) =
    let val shifted_node = Word.>> (node_hash, depth)
	val node_bit = Word.andb (shifted_node, 0w1) = 0w1
	val hash_bit = Word.andb (shifted_hash, 0w1) = 0w1
    in if shifted_node = shifted_hash then
        (Trace.local_print ("shifted_node = " ^ Word.toString shifted_node ^
			    ", shifted_hash = " ^ Word.toString shifted_hash ^
			    ", node_hash = " ^ Word.toString node_hash ^
			    ", hash = " ^ Word.toString hash);
	 Trace.print_raise (Illegal_Store_Same_Hash_Call, SOME "new_node"))
       else
        case (node_bit, hash_bit) of
           (false, false) =>		(* both zero: create new left child *)
	    Internal (add_tree (node, Word.>> (shifted_hash, 0w1),
				depth + 0w1, hash, key, value), Empty)
         | (true, true) =>		(* both one: create new right child *)
	    Internal (Empty, add_tree (node, Word.>> (shifted_hash, 0w1),
				       depth + 0w1, hash, key, value))
	 | (true, false) =>		(* one, zero: split *)
	    Internal (Terminal (hash, key, value), node)
	 | (false, true) =>		(* zero, one: split *)
	    Internal (node, Terminal (hash, key, value))
    end

   and add_tree (Empty, _, _, hash, key, value) =
        Terminal (hash, key, value)
     | add_tree (Internal (left, right),
		 shifted_hash, depth, hash, key, value) =
        if Word.andb (shifted_hash, 0w1) = 0w0 then
	 Internal (add_tree (left, Word.>> (shifted_hash, 0w1), depth + 0w1,
			     hash, key, value), right)
        else
	 Internal (left, add_tree (right, Word.>> (shifted_hash, 0w1),
				   depth + 0w1, hash, key, value))
      | add_tree ((node as Terminal (h, k, v)), shifted_hash, depth,
		  hash, key, value) =
	 if eq (key, k) then Terminal (hash, key, value)
	 else if hash = h then
	  Tlist (hash, [(key, value), (k, v)])
         else
	  new_node (node, h, shifted_hash, depth, hash, key, value, eq)
      | add_tree ((n as Tlist (_, [])), _, _, _, _, _) =
	 Trace.print_raise (Illegal_Store_Empty_Tlist, SOME "add_tree")
      | add_tree ((n as Tlist (_, (_ :: []))), _, _, _, _, _) =
	 Trace.print_raise (Illegal_Store_Single_Tlist, SOME "add_tree")
      | add_tree ((node as Tlist (h, values as (_ :: _))), shifted_hash,
		  depth, hash, key, value) =
	 if hash = h then Tlist (h, add_list ((key, value), values))
         else new_node (node, h, shifted_hash, depth, hash, key, value, eq)

  in

   fun add (table, key, value) =
        ((let val hash_value = safe_hash key
	  in add_tree (table, hash_value, 0w0, hash_value, key, value)
	  end)
	   handle x => (Trace.local_print ("hash " ^
					   Word.toString (safe_hash key) ^
					   ", tree\n" ^
					   makestring_debug (0, table));
			Trace.print_raise_again (x, SOME "add")))

  end (* local *)

(*
	25.	function look
*)

  local
   fun look_list (_, []) = NONE
     | look_list (key, ((k, v) :: rest)) =
       if eq (k, key) then SOME v else look_list (key, rest)

   fun look_tree (_, _, _, Empty) = NONE
     | look_tree (shifted_hash, hash, key, Internal (left, right)) =
        if Word.andb (shifted_hash, 0w1) = 0w0 then
	 look_tree (Word.>> (shifted_hash, 0w1), hash, key, left)
        else
	 look_tree (Word.>> (shifted_hash, 0w1), hash, key, right)
     | look_tree (shifted_hash, hash, key, Terminal (_, k, v)) =
        if eq (key, k) then SOME v else NONE
     | look_tree (_, _, _, Tlist (_, [])) =
	Trace.print_raise (Illegal_Store_Empty_Tlist, SOME "look_tree")
     | look_tree (_, _, _, Tlist (_, _ :: [])) =
	Trace.print_raise (Illegal_Store_Single_Tlist, SOME "look_tree")
     | look_tree (shifted_hash, hash, key, Tlist (h, l)) =
        if hash = h then look_list (key, l)
        else NONE

   fun look_table (table, key) =
    let val hash_key = safe_hash key
    in look_tree (hash_key, hash_key, key, table)
    end

  in
   fun look (args as (table, _)) =
        case look_table args of
           SOME v => (SOME (table, v))
	 | NONE => NONE
  end (* local *)

(*
	26.	function find
*)

  fun find (store, key) =
       case look (store, key) of
          NONE => raise Not_Present_In_Store
	| SOME v => v

(*
	27.	function size
*)

  fun size Empty = 0
    | size (Internal (left, right)) = (size left) + (size right)
    | size (Terminal _) = 1
    | size (Tlist (_, l)) = length l

(*
	28.	function empty
*)

  fun empty Empty = true
    | empty _ = false

(*
	29.	function map
*)

  fun map f =
       let fun lmap Empty = Empty
	     | lmap (Internal (left, right)) = Internal (lmap left, lmap right)
	     | lmap (Terminal (hash, key, value)) =
	        Terminal (hash, key, f (key, value))
             | lmap (Tlist (h, l)) =
	        Tlist (h, (List.map (fn (p as (k, v)) => (k, f p)) l))
       in lmap
       end

(*
	30.	function app
*)

  fun app f =
       let fun lapp Empty = ()
	     | lapp (Internal (left, right)) =
	        (lapp left;
		 lapp right;
		 ())
	     | lapp (Terminal (hash, key, value)) =
	        (f (key, value);
		 ())
             | lapp (Tlist (h, l)) =
	        List.app (fn (p as (k, v)) => (f p; ())) l
       in lapp
       end

(*
	31.	function fold
*)

  fun fold f table init = 
       let fun flatten Empty = []
	     | flatten (Internal (left, right)) =
		(flatten left) @ (flatten right)
	     | flatten (Terminal (hash, key, value)) = (key, value) :: []
	     | flatten (Tlist (hash, l)) = l
       in V.List.fold f (flatten table) init
       end

(*
	32.	function makestring
*)

  fun makestring (store, elstring, separator) =
       let fun foldstring (element, "") = elstring element
	     | foldstring (element, prev) =
	        (elstring element) ^ separator ^ prev
       in fold foldstring store ""
       end

 end (* struct *)

(*
	33.	functor Imperative_Store
*)

functor Imperative_Store (structure V: VENDOR): IMPERATIVE_STORE =
 struct
  structure Store = Store (structure V = V)

  type ('key, 'value) T = ('key, 'value) Store.T ref 

  fun new args = ref (Store.new args)
  fun size arg = Store.size (! arg)
  fun empty arg = Store.empty (! arg)

  fun add (store, key, value) = store := Store.add (! store, key, value)

  exception Not_Present_In_Store
  fun find (store, key) =
       (let val (new_store, result) = Store.find (! store, key)
        in store := new_store;
	   result
        end)
	 handle Store.Not_Present_In_Store => raise Not_Present_In_Store 

  fun look (store, key) =
       case Store.look (! store, key) of
          SOME (new_store, result) =>
	   (store := new_store;
	    SOME result)
	| NONE => NONE

  fun remove (store, key) = store := Store.remove (! store, key)

  fun remove_selected (store, f) = store := Store.remove_selected (! store, f)

  fun map f store = ref (Store.map f (! store))

  fun fold f store init = Store.fold f (! store) init

  fun makestring (store, toString, sep) =
       Store.makestring (! store, toString, sep)

 end

(*
	34.	functor Imperative_Monomorphic_Store

	This store adds to the basic Monomorphic_Store both the
	imperative feature (i.e. the store is updated in-place) and
	caching of recently accessed values.
*)

functor Imperative_Monomorphic_Store
           (structure V: VENDOR
	    type key
	    type value
	    val eq: key * key -> bool
	    val hash: key -> word): IMPERATIVE_MONOMORPHIC_STORE =
 struct
  structure Store = Monomorphic_Store (structure V = V
				       type key = key
				       type value = value
				       val eq = eq
				       val hash = hash)

  type key = key
  type value = value

  type T = (Store.T * (key * value) option) ref 

  fun new () = ref (Store.new (), NONE)
  fun size (ref (store, _)) = Store.size store
  fun empty (ref (store, _)) = Store.empty store

  fun add (store_cache as ref (store, _), key, value) =
       store_cache := (Store.add (store, key, value), SOME (key, value))

  local
   fun check_cache (NONE, _) = NONE
     | check_cache (SOME (k, v), key) =
        if eq (k, key) then SOME v else NONE

  in
   exception Not_Present_In_Store
   fun find (store_cache as ref (store, cache), key) =
        case check_cache (cache, key) of
           SOME v => v
	 | NONE =>
            (let val (new_store, result) = Store.find (store, key)
	     in store_cache := (new_store, SOME (key, result));
	        result
	     end)
	      handle Store.Not_Present_In_Store => raise Not_Present_In_Store 

   fun look (store_cache as ref (store, cache), key) =
        case check_cache (cache, key) of
	   NONE =>
	    (case Store.look (store, key) of
	        SOME (new_store, result) =>
	         (store_cache := (new_store, SOME (key, result));
		  SOME result)
	      | NONE => NONE)
         | v => v
  end

  fun remove (store_cache as ref (store, _), key) =
       store_cache := (Store.remove (store, key), NONE)

  fun remove_selected (store_cache as ref (store, _), f) =
       store_cache := (Store.remove_selected (store, f), NONE)

  fun map f (store_cache as ref (store, _)) = ref (Store.map f store, NONE)

  fun app f (ref (store, _)) = Store.app f store

  fun fold f (ref (store, _)) init = Store.fold f store init

  fun makestring (ref (store, _), toString, sep) =
       Store.makestring (store, toString, sep)

 end

(*
	35.	functor Single_Store
*)

functor Single_Store (structure V: VENDOR
		      type key
		      type value
		      val eq: key * key -> bool
		      val hash: key -> word): SINGLE_STORE =
 struct
  structure Store = Imperative_Monomorphic_Store (structure V = V
						  type key = key
						  type value = value
						  val eq = eq
						  val hash = hash)

  type key = key
  type value = value

  val store = Store.new ()

  fun size () = Store.size store
  fun empty () = Store.empty store

  fun add (key, value) = Store.add (store, key, value)

  exception Not_Present_In_Store
  fun find key =
       (Store.find (store, key))
       handle Store.Not_Present_In_Store => raise Not_Present_In_Store 

  fun look key =
       Store.look (store, key)

  fun remove key = Store.remove (store, key)

  fun remove_selected f = Store.remove_selected (store, f)

  fun app f = Store.app f store

  fun fold f init = Store.fold f store init

  fun makestring (toString, sep) = Store.makestring (store, toString, sep)

 end

       
