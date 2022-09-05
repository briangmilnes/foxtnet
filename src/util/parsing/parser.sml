(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

functor Parser (structure Base: BASE
		structure ParserBase: PARSERBASE): PARSER =
struct

  open ParserBase
  exception ParserInternalError

  fun flat3 (a, (b, c)) = (a, b, c)
  fun flat4 (a, (b, (c, d))) = (a, b, c, d)
  fun flat5 (a, (b, (c, (d, e)))) = (a, b, c, d, e)

  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard
  infixr 1 ||

  fun p && q = p -- (fn x => q -- (fn y => succeed (x, y)))
  fun p || q = p ## (fn _ => q)

  fun p wth f      = p -- succeed o f
  fun p suchthat g = p -- (fn x => if g x then succeed x else fail)
  fun p return x   = p -- (fn _ => succeed x)

  fun p guard g    = p ## (fn errpos => (g errpos; fail))

  val seq = Base.foldr (fn (ph, pt) => ph && pt wth op::) (succeed [])
  val alt = Base.foldr op|| fail

  fun satisfy g = any suchthat g

  fun literal t = satisfy (fn t' => t = t')
  fun string ts = seq (map literal ts)
  fun oneof ts  = alt (map literal ts)

  fun opt p = p wth SOME || succeed NONE
  fun optional f x p = p wth f || succeed x

  fun repeat p = let fun rep () =  p && $rep wth op:: || succeed []
		 in $rep end

  fun repeat1 p = p && repeat p wth op::

  fun first p q    = p -- (fn x => q return x)
  fun second p q   = p -- (fn _ => q)
  fun third p q r  = p -- (fn _ => q -- (fn _ => r))
  fun middle p q r = p -- (fn _ => q -- (fn x => r return x))

  fun separate  p q = p && repeat (second q p) wth op::
  fun separate0 p q = separate p q || succeed []
  fun separate' p q = first (separate p q) (opt q)

  fun use p = p -- (fn q => q)

  (***** infix parsing *****)

  datatype Associativity = LeftAssoc | RightAssoc | NonAssoc

  fun resolvefixity combine prec x xs ys =
      let fun build ([], [], c) = succeed c
	    | build (x::xs, y::ys, c) = build (xs, ys, combine (x, y, c))
	    | build _ = raise ParserInternalError

	  fun resolve (xs, ys, c, [], []) = build (xs, ys, c)
	    | resolve ([], [], c, y'::ys', x'::xs') = resolve ([c], [y'], x', ys', xs')
	    | resolve (x::xs, y::ys, c, y'::ys', x'::xs') =
	        let val (p, a)   : int * Associativity = prec y
                    val (p', a') : int * Associativity = prec y'
		    fun goleft () =
			  resolve (xs, ys, combine (x, y, c), y'::ys', x'::xs')
		    fun goright () =
			  resolve (c::x::xs, y'::y::ys, x', ys', xs')
                in
		    if p > p' then goleft ()
                    else if p' > p then goright ()
                    else case (a, a') of
			   (LeftAssoc, LeftAssoc) => goleft ()
			 | (RightAssoc, RightAssoc) => goright ()
			 | (_, _) => fail
                end
	    | resolve _ = raise ParserInternalError

      in resolve ([], [], x, ys, xs) end

  fun parseinfix combine prec p q =
      p && repeat (q && p) -- (fn (x, yxs) =>
			         let val (ys, xs) = Base.unzip yxs
				 in resolvefixity combine prec x xs ys end)

end
