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

	An implementation of labeled, data bearing, rooted trees.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Tree

		iii.	RCS Log
	
$Log: tree.fun,v $
Revision 1.3  1995/06/20  17:37:47  esb
minor fix.

Revision 1.2  1994/11/07  21:38:19  cline
use V.String

Revision 1.1  1994/09/23  19:55:40  milnes
Initial revision


		1.	functor Tree
*)

functor Tree (type label
              val label_eq: label * label -> bool
              val makestring_label: label -> string
              type data
              val makestring_data: data -> string
              val null_data: data
	      structure V: VENDOR): ROOTED_LABELED_DATA_TREE =
 struct
  type label = label
  type data = data 
  type path = label list
  datatype node =  Node of {alabel: label, data: data, zchildren: node list} 

  type tree = node option

  fun new (alabel,data) = SOME (Node{alabel=alabel,data=data, zchildren = []})

  fun member (l,[]) = NONE
    | member (l, (n as Node{alabel,...}) :: rest) = 
      if label_eq(l,alabel) 
      then SOME n 
      else member(l,rest)

  fun delete_child (l,[]) = []
    | delete_child (l, (n as Node{alabel,...}) :: rest) = 
      if label_eq(l,alabel) 
      then rest
      else n :: delete_child(l,rest)
          
  fun find (NONE, path) = NONE
    | find (SOME n, []) = NONE
    | find (SOME (n as (Node{alabel,zchildren,data})), [l]) = 
       if label_eq (l,alabel) then SOME n else NONE
    | find (SOME (Node{alabel,zchildren,data}), l :: child_label :: rest) = 
       if label_eq (l,alabel) 
       then 
        (case member (child_label,zchildren) of
           NONE => NONE
         | SOME child => find(SOME child, child_label :: rest))
       else NONE

  exception Implementation_Error of string

  fun nodepath ([], data) = raise (Implementation_Error "nodepath")
    | nodepath ([l], data) = Node{alabel=l,data=data,zchildren=[]}
    | nodepath (l :: rest, data) = 
       Node{alabel=l,data=null_data,zchildren=[nodepath(rest,data)]}

  exception Add_Not_Rooted of tree * path

  fun add (NONE, path, data) = raise (Add_Not_Rooted (NONE,path))
    | add (t, [], data) = raise (Implementation_Error "add1") 
    | add (t as (SOME (Node{alabel,data,zchildren})): tree, [l], new_data) =
       if label_eq(alabel,l)
       then (SOME (Node{alabel=alabel,data=new_data,zchildren=zchildren}))
       else raise (Add_Not_Rooted (t,[l]))
    | add (t as (SOME (Node{alabel,data,zchildren})),
           l :: child_label :: rest,
           new_data) =
      if label_eq(alabel,l) 
      then
        (case member (child_label,zchildren) of
           NONE =>
            SOME (Node{alabel=alabel,data=data,
                       zchildren = nodepath(child_label :: rest,new_data) :: zchildren})
         | SOME child => 
           (case add(SOME child, child_label :: rest, new_data) of
              NONE => raise (Implementation_Error "add") 
            | SOME new_child =>
               SOME (Node{alabel=alabel,data=data,
                           zchildren =  new_child :: delete_child(child_label, zchildren)})))

      else raise (Add_Not_Rooted (t,l :: rest))

  exception Path_Not_In_Tree of  path * tree 

 fun delete (NONE, path) = raise (Path_Not_In_Tree (path, NONE)) 
   | delete (t, []) = raise (Path_Not_In_Tree ([],t))
   | delete (t as (SOME (Node{alabel,...})), [l]) = 
      if label_eq(l,alabel) then NONE else raise (Path_Not_In_Tree ([l], t))
   | delete (t as (SOME (Node{alabel,data,zchildren})), l1 :: l2 :: []) =
      if label_eq(l1,alabel)
      then 
       (case member(l2,zchildren) of
          NONE => raise (Path_Not_In_Tree (l1 :: l2 :: [], t))
        | SOME child =>
           SOME (Node{alabel=alabel,data=data, 
                      zchildren = delete_child(l2,zchildren)}))
      else raise (Path_Not_In_Tree ([l1], t))
   | delete (t as (SOME (Node{alabel,data,zchildren})),l1 :: l2 :: rest) =
      if label_eq(l1,alabel) 
      then
       (case member(l2,zchildren) of
          NONE => raise (Path_Not_In_Tree (l1 :: l2 :: rest, t))
        | SOME subt => 
           (case delete(SOME subt,l2::rest) of
              NONE => (SOME (Node{alabel=alabel,data=data,
                              zchildren = delete_child(l2,zchildren)}))
            | SOME s => 
              (SOME (Node{alabel=alabel,data=data,
                          zchildren = s :: delete_child(l2,zchildren)}))))
      else raise (Path_Not_In_Tree (l1 :: l2 :: rest, t))

  fun strip_options [] = []
    | strip_options (NONE :: rest) = strip_options rest
    | strip_options ((SOME c) :: rest) = c :: (strip_options rest)

  fun walk (NONE, f) = NONE
   |  walk (SOME (n as (Node{alabel,zchildren,data})), f) =
       f (n, (strip_options (map (fn c => walk (SOME c,f)) zchildren)))

  fun implode [] = ""
    | implode [s] = s
    | implode (hd::tl) = hd ^ implode tl

  fun makestring (t) =
   let
    fun tabit n =
     let 
      fun taber 0 = []
        | taber n = " " :: (taber (n - 1))
     in
      implode (taber n)
     end

    fun ms (NONE,tab) = [(tabit tab)] @ ["NONE"]
      | ms (SOME (Node{alabel,data,zchildren}),tab) =
         ([(tabit tab)] @
          ["SOME Node{alabel=", (makestring_label alabel) , ",\n"] @
          [(tabit (tab + 10))] @ ["data=", (makestring_data data), ",\n"] @
          [(tabit (tab + 10))] @
           [case zchildren of
            [] => "zchildren = [" 
           | _ => "zchildren = [\n"] @
          (V.List.fold op@ (map (fn s => ms((SOME s), tab+22)) zchildren) []) @
          [(tabit (tab + 10))] @ ["]}\n"])
   in
    implode (ms (t,0))
   end
 end
