(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

signature BASE =
sig

  exception GUARD of string
  exception IMPOSSIBLE of string
  exception UNSUPPORTED of string
    (* GUARD msg is raised when some pre-condition is not satisfied      *)
    (* IMPOSSIBLE msg is raised in a situation that should be impossible *)
    (*     to reach.  obviously, there must be a bug someplace.          *)
    (* UNSUPPORTED msg is raised by a module which does not support a    *)
    (*     function included in its signature.                           *)

(***** INTEGER UTILITIES *****)

  type Positive  sharing type Positive = int
  type Natural   sharing type Natural  = int
    (* used for documentation purposes only *)

  val positive : int -> bool
  val natural  : int -> bool
    (* positive n = (n > 0) *)
    (* natural n = (n >= 0) *)

  val even     : int -> bool
  val odd      : int -> bool
  val half     : int -> int
  val quarter  : int -> int
  val pow2     : Natural -> Positive
    (* even n = (n mod 2 = 0)    *)
    (* odd n = (n mod 2 = 1)     *)
    (* half n = floor (n / 2)    *)
    (* quarter n = floor (n / 4) *)
    (* pow2 n = 2 * ... * 2 * 1 -- n 2's *)


(*  val maxint   : int *)
(*  val minint   : int *)
    (* maxint + 1 = raise Overflow *)
    (* minint - 1 = raise Overflow *)

  val ints     : Natural -> Natural list
  val upto     : int * int -> int list
    (* ints n = [0,..,n-1]              *)
    (* upto (i,j) = [i,..,j]  if j >= i *)
    (*            = []        if j <  i *)

(***** FUNCTION UTILITIES *****)

  val id       : 'a -> 'a
  val const    : 'a -> 'b -> 'a
    (* id x = x      *)
    (* const x y = x *)

  val curry    : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val curry3   : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    (* curry f x y = f (x,y)      *)
    (* curry3 f x y z = f (x,y,z) *)

  val uncurry  : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
    (* uncurry f (x,y) = f x y      *)
    (* uncurry3 f (x,y,z) = f x y z *)

  val flip     : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
    (* flip f x y = f y x *)

  val secl     : 'a -> ('a * 'b -> 'c) -> 'b -> 'c 
  val secr     : ('a * 'b -> 'c) -> 'b -> 'a -> 'c
    (* secl x op+ y = x + y *)
    (* secr op+ y x = x + y *)

  val power    : ('a * 'a -> 'a)       -> 'a -> Positive -> 'a
  val powerl   : ('a * 'a -> 'a) -> 'a -> 'a -> Natural  -> 'a
  val powerr   : ('a * 'a -> 'a) -> 'a -> 'a -> Natural  -> 'a
    (* power op* x n = x * ... * x        -- n x's, * is associative *)
    (* powerl op* c x n = c * x * ... * x -- n x's, * is associative *)
    (* powerr op* c x n = x * ... * x * c -- n x's, * is associative *)


(***** LIST UTILITIES *****)

  type 'a list1  sharing type list1 = list  (* non-empty lists *)
    (* used for documentation purposes only *)

  val cons     : 'a -> 'a list -> 'a list
    (* cons x xs = x :: xs *)

  val foldl    : ('a * 'b -> 'a) -> 'a -> 'b list  -> 'a
  val foldr    : ('a * 'b -> 'b) -> 'b -> 'a list  -> 'b
  val foldl1   : ('a * 'a -> 'a)       -> 'a list1 -> 'a
  val foldr1   : ('a * 'a -> 'a)       -> 'a list1 -> 'a
    (* foldl op+ c [x1,..,xn] = c + x1 + ... + xn -- + is left-associative  *)
    (* foldr op+ c [x1,..,xn] = x1 + ... + xn + c -- + is right-associative *)
    (* foldl1 op+ [x1,..,xn] = x1 + ... + xn -- + is left-associative       *)
    (* foldr1 op+ [x1,..,xn] = x1 + ... + xn -- + is right-associative      *)

  val reducel  : ('a * 'a -> 'a) -> 'a -> 'a list  -> 'a
  val reducer  : ('a * 'a -> 'a) -> 'a -> 'a list  -> 'a
  val reduce1  : ('a * 'a -> 'a)       -> 'a list1 -> 'a
    (* reducel op+ c [x1,..,xn] = c + x1 + ... + xn -- + is associative *)
    (* reducel op+ c [x1,..,xn] = x1 + ... + xn + c -- + is associative *)
    (* reduce1 op+ [x1,..,xn] = x1 + ... + xn -- + is associative       *)
    (*                                                                  *)
    (* reduce combines elements in a balanced fashion, e.g.             *)
    (*   reduce1 op+ [a,b,c,d] = (a + b) + (c + d)                      *)

  val mapaccuml: ('a * 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
  val mapaccumr: ('a * 'b -> 'c * 'b) -> 'b -> 'a list -> 'c list * 'b
    (* mapaccuml combines map and foldl *)
    (* mapaccumr combines map and foldr *)

  val scanl    : ('a * 'b -> 'a) -> 'a -> 'b list  -> 'a list1
  val scanr    : ('a * 'b -> 'b) -> 'b -> 'a list  -> 'b list1
  val scanl1   : ('a * 'a -> 'a)       -> 'a list1 -> 'a list1
  val scanr1   : ('a * 'a -> 'a)       -> 'a list1 -> 'a list1
    (* scan returns a list of the partial results produced by fold, e.g. *)
    (*    scanl op+ a [b,c,d] = [a,a+b,a+b+c,a+b+c+d]                    *)

  val zip      : 'a list * 'b list -> ('a * 'b) list
  val unzip    : ('a * 'b) list -> 'a list * 'b list
    (* zip ([x1,..,xn],[y1,..,yn]) = [(x1,y1),..,(xn,yn)]    *)
    (* unzip [(x1,y1),..,(xn,yn)] = ([x1,..,xn],[y1,..,yn])  *)
    (*                                                       *)
    (* if the two arguments to zip are of different lengths, *)
    (* the longer is simply truncated rather than raising an *)
    (* exception                                             *)

  val take     : Natural * 'a list -> 'a list
  val drop     : Natural * 'a list -> 'a list
  val split    : Natural * 'a list -> 'a list * 'a list
    (* take (n,[x1,..]) = [x1,..,xn]           *)
    (* drop (n,[x1,..]) = [x(n+1),..]          *)
    (* split (n,xs) = ([x1,..,xn],[x(n+1),..]) *)

  val filter   : ('a -> bool) -> 'a list -> 'a list
  val partition: ('a -> bool) -> 'a list -> 'a list * 'a list
    (* filter p [x1,..,xn] = [y1,..,ym]                 *)
    (* partition p [x1,..,xn] = ([y1,..,ym],[z1,..,xk]) *)
    (*   where yi = the ith x such that p x is true     *)
    (*         zi = the ith x such that p x is false    *)

  val exists   : ('a -> bool) -> 'a list -> bool
  val forall   : ('a -> bool) -> 'a list -> bool
  val pairwise : ('a * 'a -> bool) -> 'a list -> bool
    (* exists p [x1,x2,..] = p x1 orelse p x2 orelse ...               *)
    (* forall p [x1,x2,..] = p x1 andalso p x2 andalso ...             *)
    (* pairwise p [x1,x2,..] = p (x1,x2) andalso p (x2,x3) andalso ... *)

  val last     : 'a list1 -> 'a
    (* last [x1,..,xn] = xn *)

  val flatten  : 'a list list -> 'a list
    (* flatten [[x1,..,xn],..,[y1,..,ym]] = [x1,..,xn,..,y1,..,ym] *)

  val revonto  : 'a list * 'a list -> 'a list
    (* revonto ([x1,..,xn],[y1,..,ym]) = [xn,..,x1,y1,..,ym] *)

  val joindups : ('a * 'a -> bool) -> ('a * 'a -> 'a) -> 'a list -> 'a list
    (* joindups eq combine [a,a',b,c,c',d] =      *)
    (*     [combine (a,a'),b,combine (c,c'),d]    *)
    (* where eq is true for (a,a') and (c,c') and *)
    (* false for the other adjacent pairs         *)

(***** ORDERED LIST UTILITIES *****)

  type 'a OrdList  sharing type OrdList  = list  

  val insert   : ('a * 'a -> bool) -> 'a * 'a OrdList -> 'a OrdList
  val merge    : ('a * 'a -> bool) -> 'a OrdList * 'a OrdList -> 'a OrdList
  val sort     : ('a * 'a -> bool) -> 'a list -> 'a OrdList
    (* insert op< (3,[1,2,4,5]) = [1,2,3,4,5]      *)
    (* merge op< ([1,3,5],[2,3,4]) = [1,2,3,3,4,5] *)
    (* sort op< [3,4,2,5,1,3] = [1,2,3,3,4,5]      *)
    (*   where < = Integer.<                       *)

(***** PAIR UTILITIES *****)

  val pair     : 'a -> 'b -> 'a * 'b
  val fst      : 'a * 'b -> 'a
  val snd      : 'a * 'b -> 'b
  val swap     : 'a * 'b -> 'b * 'a
    (* pair x y = (x,y)   *)
    (* fst (x,y) = x      *)
    (* snd (x,y) = y      *)
    (* swap (x,y) = (y,x) *)

  val mapfst   : ('a -> 'b) -> 'a * 'c -> 'b * 'c
  val mapsnd   : ('b -> 'c) -> 'a * 'b -> 'a * 'c
  val mappair  : ('a -> 'b) -> 'a * 'a -> 'b * 'b
    (* mapfst f (x,y) = (f x,y)    *)
    (* mapsnd f (x,y) = (x,f y)    *)
    (* mappair f (x,y) = (f x,f y) *)

  val wrt      :  ('b * 'b -> 'c) * ('a -> 'b) -> 'a * 'a -> 'c
    (* infix wrt                                     *)
    (* (p wrt f) (x,y) = p (f x,f y)                 *)
    (*                                               *)
    (* wrt stands for "with respect to"              *)
    (* most commonly used to define orderings with   *)
    (* respect to some other ordering.  for example, *)
    (*    op< wrt length                             *)
    (* defines an ordering on lists such that        *)
    (*    xs < ys iff length xs < length ys          *)


(***** ITERATION UTILITIES *****)

  val until    : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
  val iterate  : ('a -> 'a) -> Natural -> 'a -> 'a
    (* until p f x = if p x then x else until p f (f x) *)
    (* iterate f n x = f ( ... (f x)...) -- n f's       *)


(***** UNSAFE UTILITIES *****)

  val newRef     : unit -> 'a ref         (* uninitialized ref cell *)
  val polyRef    : 'a -> 'a ref           (* no weak types *)
  val polyCallcc : ('a cont -> 'a) -> 'a  (* no weak types *)

end
