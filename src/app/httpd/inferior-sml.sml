(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (Chris.Stone@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: inferior-sml.sml,v $
# Revision 1.1  1995/03/08  20:27:04  cstone
# Initial revision
#


		1.	...
*)





val sml_filename = "/usr/cs/bin/csh"
val sml_args     = ["csh", "/usr0/cstone/exec/runML.csh"]

val being_used = ref 0
val sttt = ref ""

fun launch_sml s =
   let val _  = being_used := 1;
       val (infd,outfd) = System.Unsafe.CInterface.exec(sml_filename,
                            sml_args, [])
       val _ = System.Unsafe.SysIO.write 
                (outfd, Fox_Basis.Access.from_string s, 
                  Fox_Basis.V.String.length s)
       val _ = System.Unsafe.SysIO.closef outfd

       val bufsize = 1024

       fun loop () =
         let val (x,_,_) = System.Unsafe.SysIO.select
                 ([infd],[],[],SOME (System.Timer.TIME {sec=0, usec=0}))
         in
	     if (x = []) then
                (Fox_Basis.Scheduler.sleep 4000;
                 loop ())
             else
                let val a = ByteArray.array(bufsize, 0)
                    val n = System.Unsafe.SysIO.read(infd, a, bufsize)
                    val s' = Fox_Basis.Access.range_to_string(a, 
                                Fox_Basis.Access.Length {first=0, length=n})
                in
                    if (s' = "") then
                       (System.Unsafe.SysIO.closef infd; "")
                    else
                       s' ^ (loop ())
                end
         end

       val chop_first_line =
          let fun loop' [] = []
                | loop' ("\n"::xs) = xs
                | loop' (x::xs) = loop' xs
          in
              implode o loop' o explode
          end

       val s' = (chop_first_line o chop_first_line o chop_first_line o loop) ()

   in
       (being_used := 0;
        "<HEAD><TITLE>Result of SML Execution</TITLE></HEAD>\n" ^ 
        "<BODY><H1>Result of SML Execution</H1><P>\n" ^
        "<HR><PRE>\n" ^ 
        s ^
        "</PRE><P><HR><P>\n<PRE>" ^ 
        s' ^
        "\n</PRE><HR></BODY>\n") 
   end

fun refuse () =
   "<HEAD><TITLE>Sorry</TITLE></HEAD>\n" ^
   "<BODY><H1>Sorry</H1><P>\n" ^
   "Someone else is currently executing code.  Please try" ^
   " again in a few minutes.</BODY>"


fun runML _ [("code",s)] = 
   (if (!being_used = 0) then
      (print "===RUNNING====\n";
       print s;
       sttt := s;
       print "\n==============\n";
       launch_sml s)
    else
      refuse ())

