(* TIMEBASE *)
signature TIMEBASE = 
   sig
      
      type time
         
      val - : time * time -> time
      val + : time * time -> time
      val * : time * int -> time
      val div : time * int -> time

      val < : time * time -> bool
      val > : time * time -> bool

      val toString : time -> string
      val toReal : time -> real

      val zero : time

   end
