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

	A little tool for accessing the environment variables.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Unix_Environment

		iii.	RCS Log
	
$Log: environ.fun,v $
Revision 1.4  1995/02/04  21:47:11  robby
updated to 107

Revision 1.3  1994/11/07  21:38:06  cline
use V.String

Revision 1.2  1994/10/19  22:57:51  esb
moved the system dependency to Vendor.

Revision 1.1  1994/10/14  03:10:38  milnes
Initial revision

		1.	functor Unix_Environment
*)

functor Unix_Environment (structure V: VENDOR): UNIX_ENVIRONMENT =
 struct
  fun split_at_equal s = 
   let
    fun split_at_equal_aux(var_name_chars, []) = 
           (V.String.implode (rev var_name_chars), "")
      | split_at_equal_aux(var_name_chars, #"=" :: rest) = 
           (V.String.implode (rev var_name_chars), V.String.implode rest)
      | split_at_equal_aux(var_name_chars, c :: rest) = 
          split_at_equal_aux(c::var_name_chars, rest)
   in
     split_at_equal_aux([],V.String.explode s)
   end 

  fun find name =
   let
    fun find_aux [] = NONE
      | find_aux (s :: rest) = 
         let val (var_name,value) = split_at_equal s
         in
          if name = var_name then SOME value else find_aux rest
         end 
   in find_aux (V.Misc.environment ())
   end
 end



