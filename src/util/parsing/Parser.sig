(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

signature PARSER =
  (* See                                    *)
  (*   Hutton                               *)
  (*   "Higher-order functions for parsing" *)
  (*   JFP 2(3) (Jul 92) 323-43             *)
  (* or                                     *)
  (*   Paulson                              *)
  (*   ML for the Working Programmer        *)
  (*   p 321-328                            *)
sig

  include PARSERBASE

  (*
  infix  2 -- ##

  infixr 3 &&
  infix  2 wth suchthat return guard
  infixr 1 ||
  *)

  val &&       : ('a,'t,'r) P * ('b,'t,'r) P -> ('a * 'b,'t,'r) P 
  val ||       : ('a,'t,'r) P * ('a,'t,'r) P -> ('a,'t,'r) P 

  val wth      : ('a,'t,'r) P * ('a -> 'b) -> ('b,'t,'r) P 
  val suchthat : ('a,'t,'r) P * ('a -> bool) -> ('a,'t,'r) P
  val return   : ('b,'t,'r) P * 'a -> ('a,'t,'r) P

  val guard    : ('a,'t,'r) P * (Position.T -> 'b) -> ('a,'t,'r) P

  val seq      : ('a,'t,'r) P list -> ('a list,'t,'r) P
  val alt      : ('a,'t,'r) P list -> ('a,'t,'r) P

  val satisfy  : ('t -> bool) -> ('t,'t,'r) P 

  val literal  : ''t -> (''t,''t,'r) P
  val string   : ''t list -> (''t list,''t,'r) P
  val oneof    : ''t list -> (''t,''t,'r) P

  val opt      : ('a,'t,'r) P -> ('a option,'t,'r) P 
  val optional : ('a -> 'b) -> 'b -> ('a,'t,'r) P -> ('b,'t,'r) P

  val repeat   : ('a,'t,'r) P -> ('a list,'t,'r) P 
  val repeat1  : ('a,'t,'r) P -> ('a list,'t,'r) P 

  val first    : ('a,'t,'r) P -> ('b,'t,'r) P -> ('a,'t,'r) P 
  val second   : ('a,'t,'r) P -> ('b,'t,'r) P -> ('b,'t,'r) P 
  val third    : ('a,'t,'r) P -> ('b,'t,'r) P -> ('c,'t,'r) P  -> ('c,'t,'r) P 
  val middle   : ('a,'t,'r) P -> ('b,'t,'r) P -> ('c,'t,'r) P -> ('b,'t,'r) P 


  val separate : ('a,'t,'r) P -> ('b,'t,'r) P -> ('a list,'t,'r) P 
  val separate0: ('a,'t,'r) P -> ('b,'t,'r) P -> ('a list,'t,'r) P 
  val separate': ('a,'t,'r) P -> ('b,'t,'r) P -> ('a list,'t,'r) P 

(*  val use      : (('a,'t,'r) P,'t,'r) P -> ('a,'t,'r) P *)

  (***** Infix utilities *****)

  datatype Associativity = LeftAssoc | RightAssoc | NonAssoc

  val parseinfix : ('a * 'b * 'a -> 'a) -> ('b -> int * Associativity)
	             -> ('a,'t,'r) P -> ('b,'t,'r) P -> ('a,'t,'r) P

  (***** Helpful utilities for manipulating intermediate results *****)

  val flat3 : 'a * ('b * 'c) -> 'a * 'b * 'c
  val flat4 : 'a * ('b * ('c * 'd)) -> 'a * 'b * 'c * 'd
  val flat5 : 'a * ('b * ('c * ('d * 'e))) -> 'a * 'b * 'c * 'd * 'e

end



