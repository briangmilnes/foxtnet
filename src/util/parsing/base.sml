(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

functor Base (structure V: VENDOR): BASE =
 struct

  exception GUARD of string
  fun guard s p x = if p x then x else raise GUARD s
    
  exception IMPOSSIBLE of string
  exception UNSUPPORTED of string

(***** INTEGER UTILITIES *****)

  type Positive = int
  type Natural  = int

  fun positive n = n > 0
  fun natural n  = n >= 0

  fun even n = n mod 2 = 0
  fun odd n = not (even n)
  fun half n = n div 2
  fun quarter n = n div 4
  fun pow2 n = Word32.toInt
                (Word32.<< ((*0w1*)(Word32.fromInt 1), Word31.fromInt (guard "Base.pow2" natural n)))

(*
  val maxint =
        let fun calcmax n = (n + 1; calcmax (n+n+1)) handle Overflow => n
	in calcmax 1 end
  val minint = ~maxint - 1
*)

  fun upto (i,n) =
        let fun u i = if i = n then [n] else i :: u (i+1)
	in if i > n then [] else u i end
  fun ints n = upto (0,n-1)


(***** FUNCTION UTILITIES *****)

  fun id x = x
  fun const x y = x

  fun curry f x y = f (x,y)
  fun curry3 f x y z = f (x,y,z)

  fun uncurry f (x,y) = f x y
  fun uncurry3 f (x,y,z) = f x y z

  fun flip f x y = f y x

  fun secl x f y = f (x,y)
  fun secr f y x = f (x,y)

  fun power f x n =
        let fun pow (x,n) =
	          if even n then pow (f (x,x),half n)
		  else if n = 1 then x
                  else f (x,pow (f (x,x),half n))
	in pow (x,guard "Base.power" positive n) end
  fun powerl f c x 0 = c
    | powerl f c x n = f (c,power f x n)
  fun powerr f c x 0 = c
    | powerr f c x n = f (power f x n,c)


(***** LIST UTILITIES *****)

  type 'a list1 = 'a list

  fun cons x xs = x :: xs

  fun foldl f c xs =
        let fun fold (c,[]) = c
	      | fold (c,x::xs) = fold (f (c,x),xs)
	in fold (c,xs) end
  fun foldr f c =
        let fun fold [] = c
	      | fold (x::xs) = f (x,fold xs)
	in fold end
  fun foldl1 f [] = raise GUARD "Base.foldl1"
    | foldl1 f (x::xs) = foldl f x xs
  fun foldr1 f = 
        let fun fold [] = raise GUARD "Base.foldr1"
	      | fold [x] = x
	      | fold (x::xs) = f (x,fold xs)
	in fold end
	 
  fun reduce1 f xs =
        (* combine xs in a balanced fashion.  for example, *)
        (*   reduce1 op+ [a,b,c,d] = (a+b) + (c+d)         *)
        let fun red (i,rs,[]) = finish rs
	      | red (i,rs,[x]) = finish (x::rs)
	      | red (i,rs,x::x'::xs) =
		  if even i then red (i+1,combine (half i,f (x,x'),rs),xs)
		  else red (i+1,f (x,x') :: rs,xs)
	    and combine (i,x',x::xs) =
		  if even i then combine (half i,f (x,x'),xs)
		  else f (x,x') :: xs
	      | combine (_,_,[]) = raise IMPOSSIBLE "Base.reduce1"
	    and finish [] = raise GUARD "Base.reduce1"
	      | finish (x::xs) =
		  let fun fin (x',[]) = x'
			| fin (x',x::xs) = fin (f (x,x'),xs)
		  in fin (x,xs) end
	in red (1,[],xs) end
  fun reducel f c xs = reduce1 f (c::xs)
  fun reducer f c xs = reduce1 f (xs @ [c])

  fun mapaccuml f c xs =
        let fun m (c,[]) = (c,[])
	      | m (c,x::xs) = let val (c,y) = f (c,x)
				  val (c,ys) = m (c,xs)
			      in (c,y::ys) end
	in m (c,xs) end
  fun mapaccumr f c =
        let fun m [] = ([],c)
	      | m (x::xs) = let val (ys,c) = m xs
				val (y,c) = f (x,c)
			    in (y::ys,c) end
	in m end

  fun scanl f c xs =
        let fun scan (c,[]) = [c]
	      | scan (c,x::xs) = c :: scan (f (c,x),xs)
	in scan (c,xs) end
  fun scanr f c xs =
        let fun scan [] = (c,[c])
	      | scan (x::xs) = let val (c,cs) = scan xs
				   val c = f (x,c)
			       in (c,c::cs) end
            val (_,cs) = scan xs
	in cs end
 fun scanl1 f [] = raise GUARD "Base.scanl1"
   | scanl1 f (x::xs) = scanl f x xs
 fun scanr1 f xs =
       let fun scan [] = raise GUARD "Base.scanr1"
	     | scan [x] = (x,[x])
	     | scan (x::xs) = let val (c,cs) = scan xs
				  val c = f (x,c)
			      in (c,c::cs) end
           val (_,cs) = scan xs
       in cs end

  fun exists p =
        let fun e [] = false
	      | e (x::xs) = p x orelse e xs
	in e end
  fun forall p =
        let fun a [] = true
	      | a (x::xs) = p x andalso a xs
	in a end
  fun pairwise p [] = true
    | pairwise p (x::xs) =
        let fun w (oldx,[]) = true
	      | w (oldx,x::xs) = p (oldx,x) andalso w (x,xs)
	in w (x,xs) end

  fun zip (x::xs,y::ys) = (x,y) :: zip (xs,ys)
    | zip _ = []
  val unzip = foldr (fn ((x,y),(xs,ys)) => (x::xs,y::ys)) ([],[])

  fun take (n,xs) =
        let fun t (0,xs) = []
	      | t (n,[]) = []
	      | t (n,x::xs) = x :: t (n-1,xs)
	in t (guard "take" natural n,xs) end
  fun drop (n,xs) =
        let fun d (0,xs) = xs
	      | d (n,[]) = []
	      | d (n,x::xs) = d (n-1,xs)
	in d (guard "drop" natural n,xs) end
  fun split (n,xs) =
        let fun s (0,xs) = ([],xs)
	      | s (n,[]) = ([],[])
	      | s (n,x::xs) = let val (xs,ys) = s (n-1,xs)
                              in (x::xs,ys) end
	in s (guard "split" natural n,xs) end	    

  fun filter p =
        let fun filt [] = []
	      | filt (x::xs) = if p x then x :: filt xs else filt xs
	in filt end
  fun partition p xs =
        let fun part ([],rts,rfs) = (rev rts,rev rfs)
	      | part (x::xs,rts,rfs) =
		  if p x then part (xs,x::rts,rfs)
		         else part (xs,rts,x::rfs)
	in part (xs,[],[]) end

  fun last [] = raise GUARD "Base.last"
    | last (x::xs) =
        let fun lst (x,[]) = x
	      | lst (_,x::xs) = lst (x,xs)
	in lst (x,xs) end

  val flatten = foldr op@ []

  fun revonto ([],ys) = ys
    | revonto (x::xs,ys) = revonto (xs,x::ys)

  fun joindups eq join =
        let fun jdups (x1 :: (xs as (x2 :: xs'))) =
		  if eq (x1,x2) then dups (x1,join(x1,x2),xs')
                                else x1 :: jdups xs
	      | jdups xs = xs

	    and dups (x1,c,[]) = [c]
	      | dups (x1,c,xs as (x2::xs')) =
		  if eq (x1,x2) then dups (x1,join(c,x2),xs')
                                else c :: jdups xs
        in jdups end

(***** ORDERED LIST UTILITIES *****)

  type 'a OrdList = 'a list

  fun insert op< (x,xs) =
        let fun ins [] = [x]
	      | ins (xs as (x'::xs')) = if x < x' then x :: xs
					          else x' :: ins xs'
	in ins xs end

  fun merge op< (xs,ys) =
        let fun ml (x,xs,[]) = x::xs
	      | ml (x,xs,y::ys) = if y < x then y :: ml (x,xs,ys)
				           else x :: mr (xs,y,ys)
	    and mr ([],y,ys) = y::ys
	      | mr (x::xs,y,ys) = if y < x then y :: ml (x,xs,ys)
				           else x :: mr (xs,y,ys)
	in
	    case xs of
		[] => ys
	      | x::xs => ml (x,xs,ys)
	end

  fun sort op< =
        let fun uprun (x,down,[]) = [rev down]
	      | uprun (x,down,ys as y::rest) =
		  if y < x then rev down :: runs ys
		           else uprun (y,y::down,rest)
	    and downrun (x,up,[]) = [up]
	      | downrun (x,up,ys as y::rest) =
		  if y < x then downrun (y,y::up,rest)
		           else up :: runs ys
	    and runs [] = []
	      | runs [x] = [[x]]
	      | runs (x::y::rest) =
		  if y < x then downrun (y,[y,x],rest)
			   else uprun (y,[y,x],rest)
	in reducel (merge op<) [] o runs end

(***** PAIR UTILITIES *****)

  fun pair x y = (x,y)
  fun fst (x,y) = x
  fun snd (x,y) = y
  fun swap (x,y) = (y,x)
  fun mapfst f (x,y) = (f x,y)
  fun mapsnd f (x,y) = (x,f y)
  fun mappair f (x,y) = (f x,f y)
  fun wrt (f,g) (x,y) = f (g x,g y)


(***** ITERATION UTILITIES *****)

  fun until p f =
        let fun u x = if p x then x else u (f x)
	in u end

  fun iterate f n x =
        let fun iter (0,x) = x
	      | iter (i,x) = iter (i-1,f x)
	in iter (guard "iterate" natural n,x) end


(***** UNSAFE UTILITIES *****)

  val newRef = System.Unsafe.cast ref : unit -> 'a ref
  val polyRef = System.Unsafe.cast ref : 'a -> 'a ref
  val polyCallcc = System.Unsafe.cast V.Control.callcc : ('a cont -> 'a) -> 'a

end
