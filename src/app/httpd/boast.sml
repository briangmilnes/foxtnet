(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Peter Lee (Peter.Lee@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

 * Bignum messages
 * Written by Peter Lee
 * February 1995
 *
 * This file defines the functor Bignum_Message, which provides a
 * the function
 *      message :  capitalization -> ints -> string
 * where the first argument is either Capitalize or NoCapitalize, to
 * indicate whether the message should be capitalized, and ints is a
 * type containing (potentially big) integers.
 *
 * The integer is translated into English text which is reminiscent of
 * the messages used by MacDonald's to report the number of hamburgers
 * that have been served.  This, of course, is a market-proven way to
 * convey big numbers, especially to the type of person who eats Big Macs
 * and reads the National Enquirer.
 *

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

		iii.	RCS Log
	
$Log: boast.sml,v $
# Revision 1.1  1995/03/08  20:18:36  cstone
# Initial revision
#
*)

(* A standard signature for (potentially) big integers
 *)
signature INTS = sig
  eqtype ints

  val int : ints -> int
  val ints : int -> ints
  val makestring : ints -> string

  val + : ints * ints -> ints
  val - : ints * ints -> ints
  val * : ints * ints -> ints
  val div : ints * ints -> ints
  val mod : ints * ints -> ints
  val < : ints * ints -> bool
  val <= : ints * ints -> bool
  val > : ints * ints -> bool
  val >= : ints * ints -> bool
end

(* The signature for Bignum_Message structures.
 *)
signature BIGNUM_MESSAGE = sig
  structure I : INTS

  datatype capitalization = Capitalize | NoCapitalize

  val message : capitalization -> I.ints -> string
end

(* The Bignum_Message functor.
 *)
functor Bignum_Message (structure Ints : INTS) : BIGNUM_MESSAGE =
struct
  structure I = Ints

  datatype capitalization = Capitalize | NoCapitalize

  (* The main computation involves a translation of integers into
   * "fudged_int"s.  A fudged integer has very little of the original
   * integer in it: only a "magnitude" and a few digits of the original
   * integer.
   *
   * If the translation is carried out by factors of f, then a
   * magnitude of m indicates the the original integer i is on the
   * order of f^m.  For example, if the factor f=1000 and m=2, then
   * i is somewhere around a million or so.
   *
   * There are three kinds of fudged integers:  Almost(m,n),
   * Over(m,n), and Half(m) (where m is a magnitude and n an int).
   * For example, if the factor f=1000, m=2, and n=25, then
   *
   *     Almost(m,n) => "Almost 25 million"
   *     Over(m,n)   => "Over 25 million"
   *     Half(m)     => "Half a million"
   *)

  type magnitude = int

  datatype fudged_int =
      Almost of magnitude * int
    | Over   of magnitude * int
    | Half   of magnitude

  (* The "almost" constant determines the cut-off for "Almost" values.
   * For example, with factor of 1000 and "almost" constant of 9, an
   * integer value greater than 900 is translated into "Almost 1 thousand".
   *)
  val almost = 8

  (* The function "approximate" translates integers into fudged integers.
   *
   *        approximate f n : fudged_int
   *
   * where f:int >=1, n:ints >= 0.
   *
   * Argument f gives the factor, in terms of the number of decimal digits.
   * For example, f=3 means that the factor is 1000; f=1 means factor of 10.
   *)
  fun approximate f n =
          (* standard exponentiation function *)
      let fun exp (b, 0) = 1
	    | exp (b, n) = b * exp (b, n-1)

	  (* various factors *)
	  val factor = I.ints (exp (10, f))
	  val almost_factor = I.ints (almost * exp (10, f-1))
	  val half_factor = I.ints (5 * exp (10, f-1))

	  (* the translation of n *)
	  fun approx (n, mag) =
	      let val num = I.div (n, factor)
	      in
		  if num = I.ints 0 then
		      if I.>= (n, almost_factor) then
			  Almost (mag+1,
				  I.int (I.div (I.+(n,factor),factor)))
		      else if I.>= (n, half_factor) then
			  Half (mag+1)
		      else
			  Over (mag, I.int n)
		  else if I.< (num, half_factor) andalso
		          I.>= (I.mod (n, factor), almost_factor) then
			  Almost (mag+1,
				  I.int (I.div (n, factor)) + 1)
		       else
			  approx (num, mag+1)
	      end
      in
	  approx (n, 0)
      end

  (* magnitude m : string,  m >= 0
   * This function translates magnitude m (with factor=1000) into the
   * equivalent English word.
   *)
  local val approx_text =
      ["", "thousand", "million", "billion", "trillion", "gazillion"]
  in
      fun magnitude m =
	  let fun mag (m, [text]) = text
		| mag (0, text::_) = text
		| mag (m, _::rest) = mag (m-1,rest)
	  in
	      mag (m, approx_text)
	  end
  end

  (* Finally, the "message" function.
   *)
  fun message c n =
      let val negative = I.< (n, I.ints 0)
	  val n = if negative then I.- (I.ints 0, n) else n
	  val neg = if negative then " minus " else " "

	  fun cap s = case c of
	      Capitalize => chr (ord s - (ord "a" - ord "A"))
	    | _ => s
      in
	  case approximate 3 n of
	      Almost (n,m) =>
		  (cap "a")^"lmost"^neg^(FoxMakestring.int m)^" "^(magnitude n)
	    | Over (0,m) =>
		  (cap "o")^"nly"^neg^(FoxMakestring.int m)
	    | Over (n,m) =>
		  if negative then
		      (cap "l")^"ess than"^neg^(FoxMakestring.int m)^" "^(magnitude n)
		  else
		      (cap "o")^"ver "^(FoxMakestring.int m)^" "^(magnitude n)
	    | Half (n) =>
		  if n=1 then
		      if negative then
			  (cap "a")^"bout minus 1 "^(magnitude n)^" or so"
		      else
			  (cap "l")^"ess than 1 "^(magnitude n)
		  else
		      if negative then
			  (cap "a")^"bout"^neg^"1 "^(magnitude n)^" or so"
		      else
			  (cap "o")^"ver half a "^(magnitude n)
      end
end

structure Integers : INTS = struct
  type ints = Integer.int

  fun int n = n
  fun ints n = n
  val makestring = FoxMakestring.int

  val op + = Integer.+
  val op - = Integer.-
  val op *  = Integer.*
  val op div = Integer.div
  val op mod = Integer.mod
  val op < = Integer.<
  val op <= = Integer.<=
  val op > = Integer.>
  val op >= = Integer.>=
end

structure Word64 : INTS = struct
  type ints = FoxWord64.word

  fun int n = FoxWord64.wordToInt n
  fun ints n = FoxWord64.intToWord n
  val makestring = FoxMakestring.word64

  val op + = FoxWord64.+
  val op - = FoxWord64.-
  val op *  = FoxWord64.*
  val op div = FoxWord64.div
  val op mod = FoxWord64.mod
  val op < = FoxWord64.<
  val op <= = FoxWord64.<=
  val op > = FoxWord64.>
  val op >= = FoxWord64.>=
end
