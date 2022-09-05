(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This marshalls and unmarshalls DES keys to and from
	bytearrays.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DES_KEY_LINEARIZE

		iii.	RCS Log
	
$Log: des_linearize.fun,v $
Revision 1.1  1994/08/25  12:15:18  robby
Initial revision

Revision 1.1  1994/07/14  20:30:43  robby
Initial revision

Revision 1.1  94/07/13  18:44:14  robby
Initial revision


	1.	functor Des_Key_Linearize
*)
functor Des_Key_Linearize(structure Des:DES'
			  structure Ubyte1:UBYTE1_LINEARIZE
			  val reverse:bool):DES_KEY_LINEARIZE=
struct
  structure Des=Des
  type T=Des.key
  datatype extern=Extern of ByteArray.bytearray * int
  type incoming=extern
  type outgoing=extern

  exception Does_Not_Match

  structure Mesh=Pair
    (structure P1=Ubyte1
     structure P2=Pair
       (structure P1=Ubyte1
	structure P2=Pair
	  (structure P1=Ubyte1
	   structure P2=Pair
	     (structure P1=Ubyte1
	      structure P2=Pair
		(structure P1=Ubyte1
		 structure P2=Pair
		   (structure P1=Ubyte1
		    structure P2=Pair
		      (structure P1=Ubyte1
		       structure P2=Ubyte1)))))))

  fun size _=8

  fun marshall (k,Extern(array,pos))=
    let val (one,two,three,four,five,six,seven,eight)=
      Des.key_to_ubyte1_tuple k
      val (Mesh.Extern(array,pos))=
	Mesh.marshall
	((one,(two,(three,(four,(five,(six,(seven,eight))))))),
	 Mesh.Extern(array,pos))
	handle Mesh.Does_Not_Match => raise Does_Not_Match
    in
      Extern(array,pos)
    end
	  
  fun unmarshall (Extern(array,pos))=
    let val ((one,(two,(three,(four,(five,(six,(seven,eight))))))),
	     Mesh.Extern (array,pos))=
      Mesh.unmarshall(Mesh.Extern(array,pos))
      handle Mesh.Does_Not_Match => raise Does_Not_Match
    in
      (Des.ubyte1_tuple_to_key (one,two,three,four,five,six,seven,eight),
       Extern (array,pos))
    end

end
