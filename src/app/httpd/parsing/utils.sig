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

	Some useful parsing utilities.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PARSING_UTILS
	2.	Predicate Combinators
	3.	Type translations
	4.	Patterns

		iii.	RCS Log
	
$Log: utils.sig,v $
Revision 1.1  1996/05/19  18:17:19  esb
Initial revision

Revision 1.1  1995/02/13  23:01:17  esb
Initial revision



		1.	signature PARSING_UTILS
*)

signature PARSING_UTILS = 
 sig
  val is_alphabetic: char -> bool
  val is_digit: char -> bool

(*
		2.	Predicate Combinators
*)

  val cor: ('a -> bool) * ('a -> bool) -> ('a -> bool)
  val cand: ('a -> bool) * ('a -> bool) -> ('a -> bool)
  val cnot: ('a -> bool) -> ('a -> bool)
(*
  infixr 4 cor 
  infixr 4 cand 
*)
  
(*
		3.	Type translations
*)

  val list1: 'a -> 'a list
  val list2: 'a * 'a list -> 'a list
  val list3: 'a * ('a * 'a) -> 'a list
  val list4: 'a * ('a * ('a * 'a)) -> 'a list
  val list5: 'a * ('a * ('a * ('a * 'a))) -> 'a list
  val list6: 'a * ('a * ('a * ('a * ('a * 'a)))) -> 'a list
  val list7: 'a * ('a * ('a * ('a * ('a * ('a * 'a))))) -> 'a list
  val list8: 'a * ('a * ('a * ('a * ('a * ('a * ('a * 'a)))))) -> 'a list
  val list9:
       'a * ('a * ('a * ('a * ('a * ('a * ('a * ('a * 'a))))))) -> 'a list

  val digit_to_int: char -> int
  val pint_to_int: char list -> int

  val sint_to_int: char list -> int

(*
		4.	Patterns
*)

  type ('a,'t,'r) P			(* shared with Parser.P *)

  val bt: (char, char, 'a) P 
  val btn: (char, char, 'a) P 
  val btspace: (char list, char, 'a) P
  val btnspace: (char list, char, 'a) P
  val whitespace: (char list, char, 'a) P

  val -& : ('a, char, 'b) P -> ('a, char, 'b) P
  val &-& : ('a, char, 'b) P * ('c, char, 'b) P -> ('a * 'c, char, 'b) P

  val digit: (char, char, 'a) P
  val literals: ''a list -> (''a list, ''a, 'b) P
  val string: (char list, char, 'a) P
  val id: (char list, char, 'a) P
  val pint: (char list, char, 'a) P
  val sint: (char list, char, 'a) P
  val uptoeol: (char list, char, 'a) P
  val semi_comment : (char * char list, char, 'a) P
  val sharp_comment: (char * char list, char, 'a) P
  val word: (char list, char, 'a) P

(*
		4.	Semantic Checks

	An infix semantic test-and-translate idiom.
*)

   type T				(* shared with Parser.Position.T *)

(*
 infix 2 sem
*)
   val sem: ('a, 'b, 'c) P * ('a * T -> 'd option) -> ('d, 'b, 'c) P

 end (* sig *)
