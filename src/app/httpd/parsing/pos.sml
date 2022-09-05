(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

functor Position (): POSITION =
  (* positions within a file (for reporting errors) *)
struct

  type Coord = {char: int, line: int}
  datatype T = At of {char: int, line: int}
             | Between of ({char: int, line: int} * {char: int, line: int})

  val initpos = At {char = 1, line = 1}

  fun nextchar (At {char, line}) = At {char = char + 1, line = line}
    | nextchar (Between (_,{char, line})) = At {char = char + 1, line = line}

  fun nextline (At {char, line}) = At {char = 1, line = line + 1}
    | nextline (Between (_,{char, line})) = At {char = 1, line = line + 1}

  fun markstream s =
        let fun mark (s, char, line) =
	          let val (c, s) = Stream.uncons s
                  in
		      Stream.lcons ((c, At {char = char, line = line}),
				    fn () =>
				      if c = #"\n" then
					  mark (s,1, line + 1)
				      else
					  mark (s, char + 1, line))
                  end
                  handle Stream.EMPTY => Stream.empty
        in mark (s,1,1) end

  fun right (At coord) = coord
    | right (Between (_, coord)) = coord

  fun left (At coord) = coord
    | left (Between (coord,_)) = coord

  fun leftmost (a as {char = c1, line = l1},
		b as {char = c2, line = l2}): Coord =
        if      l1 < l2 then a
        else if l2 < l1 then b
        else if c1 < c2 then a
        else (* c2 <= c1 *)  b

  fun rightmost (a as {char = c1, line = l1},
		 b as {char = c2, line = l2}): Coord =
        if      l1 > l2 then a
        else if l2 > l1 then b
        else if c1 > c2 then a
        else (* c2 >= c1 *)  b

  fun union (p1, p2) =
        Between (leftmost  (left p1, left p2), rightmost (right p1, right p2))
  fun max (p1, p2) =
        Between (rightmost (left p1, left p2), rightmost (right p1, right p2))
  fun min (p1, p2) =
        Between (leftmost  (left p1, left p2), leftmost  (right p1, right p2))

  fun coord2string {char, line} =
        (makestring (line: int)) ^ ":" ^ (makestring (char: int))

  fun makestring (At coord) = coord2string coord
    | makestring (Between (coord1, coord2)) =
        if coord1 = coord2 then coord2string coord1
        else coord2string coord1 ^ "-" ^ coord2string coord2

end
