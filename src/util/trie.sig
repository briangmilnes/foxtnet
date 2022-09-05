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

	A signature for a TRIE, a set representation for hierarchical keys.
  See for example, Aho, Hopcroft and Ullman's Data Structures and Algorithms.
  

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TRIE

		iii.	RCS Log
	
$Log: trie.sig,v $
Revision 1.1  1994/06/28  02:36:39  milnes
Initial revision


		1.	signature TRIE
*)

signature TRIE =
 sig

   type key    (* The key that the Trie indexes each node with upon. *)
   type data   (* The record stored at that node. *)
   datatype T =  Trie of {data : data list, children : (key * T) list} |
                 EmptyTrie

   val equal : T * T -> bool
   val new   : unit -> T  

   (* Add data to the trie node, creating the node if neccesary. *)
   val add    : T * key list * data -> T
   exception Empty_Key_List

   (* Find in T the node matching the key list and return its data,
      or [] if there is none. *)
   val find   : T * key list -> data list

   (* Delete the data from the node in the Trie, deleting the
     node if it has no children and remaining data, return the
     new Trie, or NONE if it was deleted. *)
   val delete : T * key list * data -> T

   (* An inorder walk of the trie, with the reversed path and the 
     data handed to the function. *)
   val walk   : T * (key list * data list -> bool) -> unit
 end 


