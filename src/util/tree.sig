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

	A rooted, labeled, data bearing tree.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ROOTED_LABELED_DATA_TREE

		iii.	RCS Log
	
$Log: tree.sig,v $
Revision 1.1  1994/09/23  19:55:11  milnes
Initial revision


		1.     signature ROOTED_LABELED_DATA_TREE
*)

signature ROOTED_LABELED_DATA_TREE =
 sig
  type label
  type data
  datatype node =  Node of {alabel : label, data : data, zchildren : node list} 
  (* alabel, and zchildren are to force printing order. *)
  type tree (* = node option *)
  type path (* = label list *)
  
  val new : label * data -> tree
  val find : tree * path -> node option 

  exception Add_Not_Rooted of tree * path 
  val add : tree * path * data -> tree

  exception Path_Not_In_Tree of  path * tree 
  val delete : tree * path -> tree
  val walk : tree * (node * 'a list -> 'a option) -> 'a option
  val makestring : tree -> string
 end

