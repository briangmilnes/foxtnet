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

	Access to addressing information for the machine executing the
	program.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ADDRESSING_TYPES
	2.	signature ADDRESSING

		iii.	RCS Log
	
$Log: addressing.sig,v $
Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/03/21  17:52:29  cstone
Made addressing_path a type abbreviation in ADDRESSING.

Revision 1.3  1995/03/07  23:46:44  esb
made AT a substructure.

Revision 1.2  1994/10/21  20:45:10  esb
minor changes.

Revision 1.1  94/10/19  23:18:06  milnes
Initial revision


	1.	signature ADDRESSING_TYPES

	A Host specifies the addressing information for a FoxNet.
	Each host may have one name, but many interfaces. Each
	interface has a single ip address, but may have multiple
	gateways. Each gateway has a name and an ip address.

*)

signature ADDRESSING_TYPES =
 sig
  datatype gateway = Gateway of {name: string, ip: Word32.word}

  datatype interface = 
      Interface of {name: string, ip: Word32.word, 
		    mask: Word32.word option,
		    gateways: gateway list}

  datatype host = Host of {name: string, interfaces: interface list}

  datatype alias = Alias of {name: string option, host: host}

  exception Bad_Initialization of string
 end  (* sig *)

(*
	2.	signature ADDRESSING

	Each operating system typically provides a set of files on
	disk that specify the host's name, its ip address, and a
	default gateway.  Many allow for multiple ip addresses on
	different ip interfaces. This is a signature for an addressing
	structure that specifies how FoxNet should search for
	addressing information from any combination and ordering of
	
*)

signature ADDRESSING =
 sig
  structure AT: ADDRESSING_TYPES

   (* Env_Var specifies that the file name is stored in the shell's
      environment variable of the specified name.

      File_Name directly specifies the file name.

      Default specifies that the operating system's default
      algorithm should be used.  On most variants of Unix,
      this looks up various files in /etc.

      Host_Value directly specifies the desired value.

      Alternate is a host specification used if fetch would
      otherwise return NONE.

    *)

  datatype source =
      EnvVar of string              
    | File of string                
    | Default            
    | Host_Value of AT.host
    | Alternate of AT.alias

  type addressing_path = source list

  val default: addressing_path ref
  (* Typically, 
     [EnvVar "FOXNET", 
      File "/etc/foxnet",
      File "/afs/cs/project/fox/foxnet/etc/foxnet",
      Default
     ]

   This causes the foxnet to first check the unix environment for
   FOXNET.  If it points to the file, the file is read and the
   addressing it specifies is used.

   Otherwise, "/etc/foxnet" is searched and any addressing it
   specifies is used.  Otherwise,
   "/afs/cs/project/fox/foxnet/lib/foxnet" is read and used.
   Otherwise, the operating system's default addressing techinque is
   used.  If a file does not contain a "host" line then the operating
   system's idea of the host name is used.

  *)

  (* The function that reads FoxNet defaults from the specified path. *)
  val initialize: addressing_path -> unit

  (* Query the system as to its current addressing. *)
  val fetch: unit -> AT.host option
 end


