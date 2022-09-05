(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

functor Input (structure V: VENDOR
	       structure Base: BASE): INPUT =
 struct

  fun getchars file close =
        let fun get () =
	          if end_of_stream file then
		      (close (); Stream.empty)
		  else
		      doline (V.String.explode (input_line file))

            and doline cs = Base.foldr Stream.cons (Stream.delay get) cs

        in Stream.delay get end

  fun readfile filename =
        let val file = open_in filename
        in
	    getchars file (fn () => close_in file)
        end

  fun readkeybd () = getchars std_in (fn () => ())

  fun readstring string =
    Base.foldr Stream.cons Stream.empty (V.String.explode string)

 end
