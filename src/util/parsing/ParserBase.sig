(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

signature PARSERBASE =
  (* See                                    *)
  (*   Hutton                               *)
  (*   "Higher-order functions for parsing" *)
  (*   JFP 2(3) (Jul 92) 323-43             *)
  (* or                                     *)
  (*   Paulson                              *)
  (*   ML for the Working Programmer        *)
  (*   p 321-328                            *)
sig

  structure Position : POSITION

  type ('a,'t,'r) P
    (*  'a = result of local parser        *)
    (*  't = type of tokens                *)
    (*  'r = final result of overall parse *)

  (*
  infix  2 -- ##
  *)

  val succeed  : 'a -> ('a,'t,'r) P 
  val fail     : ('a,'t,'r) P 

  val done     : 'a -> ('a,'t,'r) P 
  val any      : ('t,'t,'r) P 

  val --       : ('a,'t,'r) P * ('a -> ('b,'t,'r) P) -> ('b,'t,'r) P
  val ##       : ('a,'t,'r) P * (Position.T -> ('a,'t,'r) P) -> ('a,'t,'r) P

  val !!       : ('a,'t,'r) P -> ('a * Position.T,'t,'r) P

  val lookahead: ('a,'t,'r) P -> ('a -> ('b,'t,'r) P) -> ('b,'t,'r) P

  val justone  : ('a,'t,'r) P -> ('a,'t,'r) P 

  val parse    : ('r,'t,'r option) P ->
	             ('t * Position.T) Stream.T -> 'r option
  val parses   : ('r,'t,'r Stream.T) P ->
	             ('t * Position.T) Stream.T -> 'r Stream.T
  val parsewith: ('a -> 'b) -> (Position.T -> 'b) -> ('a,'t,'b) P ->
	             ('t * Position.T) Stream.T -> 'b

  val transform: ('r,'t,'r Stream.T) P ->
	             ('t * Position.T) Stream.T -> 'r Stream.T

  (* Recursive Stuff *)

  val $  : (unit -> ('a,'t,'r) P) -> ('a,'t,'r) P

end
