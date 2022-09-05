(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
        George Necula (George.Necula@@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@@cs.cmu.edu)
        Nick Haines (Nick.Haines@@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

DES encryption support

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DES_KEY

		iii.	RCS Log
	
$Log: key.sig,v $
Revision 1.1  1994/01/13  18:50:37  necula
Initial revision

		1.	signature DES_KEY
*)

			      (* DES_KEY provides an external view of the DES 
                               * keys. Look at the definition of the functor 
                               * to see what actual EXT types are implemented
			      *)
signature DES_KEY =
  sig

    type T
    type ext
    val internalize : ext -> T
    val externalize : T -> ext
      
  end;

