(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

signature STREAM =
sig
  type 'a T

  exception EMPTY

  val empty  : 'a T
  val cons   : 'a * 'a T -> 'a T
  val lcons  : 'a * (unit -> 'a T) -> 'a T

  val delay  : (unit -> 'a T) -> 'a T
	(* useful when even the first element should be delayed *)

  val uncons : 'a T -> 'a * 'a T
  val head   : 'a T -> 'a
  val tail   : 'a T -> 'a T

  val isempty: 'a T -> bool

  val foldr  : ('a * 'b -> 'b) -> 'b -> 'a T -> 'b
  val foldl  : ('a * 'b -> 'a) -> 'a -> 'b T -> 'a

  val to_list : 'a T -> 'a list
end
