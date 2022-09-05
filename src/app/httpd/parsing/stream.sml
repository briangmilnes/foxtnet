(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

structure Stream : STREAM =
struct
  structure Susp = System.Unsafe.Susp

  datatype 'a T = Empty
                | Cons of 'a * 'a T
		| Delay of 'a T Susp.susp

  exception EMPTY

  val empty = Empty
  val cons  = Cons
  val delay = Delay o Susp.delay
  fun lcons (x,xs) = Cons (x,delay xs)

  fun uncons Empty = raise EMPTY
    | uncons (Cons xxs) = xxs
    | uncons (Delay xs) = uncons (Susp.force xs)

  fun head Empty = raise EMPTY
    | head (Cons (x,xs)) = x
    | head (Delay xs) = head (Susp.force xs)

  fun tail Empty = raise EMPTY
    | tail (Cons (x,xs)) = xs
    | tail (Delay xs) = tail (Susp.force xs)

  fun isempty Empty = true
    | isempty (Cons (x,xs)) = false
    | isempty (Delay xs) = isempty (Susp.force xs)

  fun foldr f c =
      let fun fold Empty = c
	    | fold (Cons (x,xs)) = f (x,fold xs)
	    | fold (Delay xs) = fold (Susp.force xs)
      in fold end

  fun foldl f c xs =
      let fun fold (c,Empty) = c
	    | fold (c,Cons (x,xs)) = fold (f (c,x),xs)
	    | fold (c,Delay xs) = fold (c,Susp.force xs)
      in fold (c,xs) end

  fun to_list Empty = []
    | to_list (Cons (a,b)) = a :: (to_list b)
    | to_list (Delay xs) = to_list (Susp.force xs)

end

