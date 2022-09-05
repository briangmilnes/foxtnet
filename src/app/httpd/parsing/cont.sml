(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

functor ContParser(structure Position : POSITION) : PARSERBASE =
  (* backtracking parser using success and failure continuations *)
struct

  structure Position = Position

  type pos = Position.T
  type 't stream = ('t * pos) Stream.T

  type 'r  fcont = pos -> 'r
  type ('t,'r) cont = 'r fcont * pos * 't stream -> 'r
  type ('a,'t,'r) scont = 'a * pos -> ('t,'r) cont
  type ('a,'t,'r) P = ('a,'t,'r) scont -> ('t,'r) cont

  infix  2 -- ##

  (* Primitive Parsers *)

  fun succeed x s (f,pos,ts) = s (x,pos) (f,pos,ts)
  fun fail s (f,pos,ts) = f pos

  fun done x s (f,pos,ts) =
        if Stream.isempty ts then s (x,pos) (f,pos,ts)
	                     else f pos

fun snd (x,y) = y

  fun any s (f,pos,ts) =
        if Stream.isempty ts then f pos
	else let val (h,ts) = Stream.uncons ts
                 val pos = Position.nextchar (snd h)
             in s h (f,pos,ts) end

  fun (p -- q) s = p (fn (x,pos1) => q x (fn (y,pos2) =>
       	             s (y,Position.union (pos1,pos2))))

  fun (p ## q) s (f,pos,ts) =
        p s (fn err1 => q err1 s (fn err2 => f (Position.max (err1,err2)),
	 	                  pos,ts),
	     pos,ts)

  fun !! p s = p (fn (x,pos) => s ((x,pos),pos))

  fun lookahead p q s (f,pos,ts) =
        p (fn (x,_) => fn _ => q x s (f,pos,ts)) (f,pos,ts)

  fun justone p s (f,pos,ts) =
        p (fn a => fn (_,pos,ts) => s a (f,pos,ts)) (f,pos,ts)

  fun $ p s = p () s

  fun parsewith s f p ts = p (fn (x,_) => fn _ => s x) (f,Position.initpos,ts)
  fun parse p = parsewith SOME (fn _ => NONE) p

  fun parses p ts =
        Stream.delay (fn () =>
	  p (fn (x,_) => fn (f,pos,_) => Stream.lcons (x,fn () => f pos))
	    (fn _ => Stream.empty,Position.initpos,ts))

  fun transform p ts =
        let fun trans (pos,ts) =
		  if Stream.isempty ts then Stream.empty
		  else
		    p (fn (x,_) => fn (_,pos,ts) =>
		         Stream.lcons (x,fn () => trans (pos,ts)))
		      (fn _ => Stream.empty,pos,ts)
	in
	    Stream.delay (fn () => trans (Position.initpos,ts))
	end

end
