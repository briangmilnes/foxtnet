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
	1.	

		iii.	RCS Log
	
$Log: trie.fun,v $
Revision 1.2  1994/06/29  19:30:21  milnes
Added an equality check for data.

Revision 1.1  1994/06/28  02:36:39  milnes
Initial revision


		1.	functor TRIE
*)

functor Trie(type key
             val keyequal : key * key -> bool
             type data
             val dataequal : data * data -> bool
             ) : TRIE =
 struct

  type key  = key
  type data = data
  datatype T =  Trie of {data : data list, children : (key * T) list}
              | EmptyTrie

  fun listequal(predicate,[],[]) = true 
    | listequal(predicate,[],a) = false
    | listequal(predicate,a,[]) = false
    | listequal(predicate,a1 :: r1, a2 :: r2) = 
      if predicate(a1,a2) then listequal(predicate,r1,r2) else false

  fun equal (Trie{data=data1,children=children1},Trie{data=data2,children=children2}) =
    if listequal(dataequal,data1,data2)
    then listequal((fn ((k1,t1),(k2,t2)) => keyequal(k1,k2) andalso equal(t1,t2)),
                   children1,children2)
    else false
    | equal (EmptyTrie,EmptyTrie) = true
    | equal _ = false

  fun assoc equal =
   let 
    fun a (key,[]) = NONE
      | a (key,(k,v) :: rest) = if equal(key,k) then SOME v else a (key,rest)
   in
    a
   end
  
  fun remove equal =
   let
    fun d (key,[]) = []
      | d (key, (a,v) :: rest) =
         if equal(key,a) then rest else (a,v) :: (d (key,rest))
   in
    d
   end

  val assoc = assoc keyequal
  val removekey = remove keyequal

  fun removedatum (d, []) = []
    | removedatum (d, d1 :: rest) =
        if dataequal(d,d1) then rest else d1 :: (removedatum (d,rest))

  exception Trie_Implementation_Error of string

  fun new () = EmptyTrie

  fun new_path ([],data) = Trie{data = [data],children = []}
    | new_path (k :: krest, data) = 
        Trie{data=[],children = [(k,new_path(krest,data))]}

  fun member (datum,[]) = false
    | member (datum, d1 :: rest) =
       if dataequal(datum,d1) then true else member(datum,rest)

  exception Empty_Key_List

  fun add (_,[],_) = raise Empty_Key_List
    | add (EmptyTrie,list,datum) = new_path(list,datum)
    | add (Trie{data,children},[k],datum) =
      let 
        val C = assoc (k,children)
      in
       case C of
         NONE =>
          Trie{data = data,
               children = (k,Trie{data= [datum], children = []}) :: children}
       | SOME (t as (Trie{data=old_data,children=old_children})) =>
          if member(datum,old_data) then t
          else Trie{data = data,
               children = (k,Trie{data= datum :: old_data, children = old_children})
                           :: (removekey (k,children))}
       | SOME EmptyTrie => raise (Trie_Implementation_Error "add 1")
      end 
    | add (Trie{data,children}, k :: rest, datum) =
      let
        val C = assoc(k,children)
      in
        case C of
          NONE => Trie{data=data,
                       children = (k,new_path (rest,datum)) :: 
                                   (removekey (k,children))}
        | SOME (J as Trie{data=old_data,children=old_children}) =>
           Trie{data = data,
                children = (k,add(J,rest,datum)) :: (removekey (k,children))}
        | SOME EmptyTrie => raise (Trie_Implementation_Error "add 2")
      end

  fun delete (t,[],data) = t
    | delete (t as Trie{data,children},[k],datum) =
      let
        val C = assoc (k,children)
      in
       case C of
         NONE => t (* No child labeled K, return the original trie. *)
       | SOME (Trie{data = d, children = c}) =>
         let
          val new_data = removedatum (datum,d)
         in
          case (new_data,c) of
            ([],[]) => Trie{data = data, children = removekey(k,children)}
          | _ => Trie{data = data,
                      children = (k,Trie{data=new_data,children=c}) :: removekey (k,children)}
         end 
       | SOME EmptyTrie => raise (Trie_Implementation_Error "delete 1")
      end
    | delete (t as Trie{children,data},k :: rest,datum) =
      let 
        val C = assoc (k,children)
      in
        case C of
          NONE => t
        | SOME J =>
          let
            val d = delete(J,rest,datum)
            val children' = removekey(k,children)
          in
           case d of
             EmptyTrie =>
              (case (data,children') of
                ([],[]) => EmptyTrie (* No data, no children, no node.*)
               | _ => Trie{data = data, children = (k,d) :: children'})
             | F => Trie{data = data, children = (k,F) :: children'}
          end
      end
    | delete (EmptyTrie,_,_) = raise (Trie_Implementation_Error "delete 2")

  fun find (EmptyTrie, keys) = []
    | find (Trie{data,children},[]) = []
    | find (Trie{data,children},[k]) =
       let 
        val C = assoc(k,children)
       in
        case C of
          NONE => []
        | SOME (t as Trie{data,children}) => data
        | SOME EmptyTrie => raise (Trie_Implementation_Error "find 1")
       end
    | find (Trie{data,children}, k1 :: rest) =
      let 
        val C = assoc(k1,children)
      in
       case C of
         NONE => []
       | SOME t => find (t,rest)
      end

  fun walk (t,f) =
   let 
    fun walk_aux(path,EmptyTrie,f) = ()
     |  walk_aux(path,Trie{data,children},f) =
        let 
         fun walker (k,Trie{data,...})  = f (k :: path,data)
           | walker (k,EmptyTrie) = raise (Trie_Implementation_Error "walk 1")
        in 
         f (path,data);
         app walker children
        end
  in
    walk_aux([],t,f)
  end  
end 
