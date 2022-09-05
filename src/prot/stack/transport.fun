(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	Functors useful for building TCP and UDP.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	Signature TRANSPORT_STRUCTURES
	2.	Functor Transport_Structures

		iii.	RCS Log
	
$Log: transport.fun,v $
Revision 1.6  1996/04/18  21:32:20  cline
converted hash from int to word

Revision 1.5  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/10/13  15:53:43  cstone
Added Host_Id structure

Revision 1.2  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.1  1995/07/05  17:51:19  cline
Initial revision



		1.	Signature TRANSPORT_STRUCTURES
*)

signature TRANSPORT_IDS =
sig
  type host_id
  type port
  structure Host_Id : TRANSPORT_HOST_ID
  structure Key : TRANSPORT_CONNECTION_KEY
  structure Address : TRANSPORT_ADDRESS
  structure Pattern : TRANSPORT_PATTERN
    sharing type Host_Id.T = host_id
end

(*
		2.	Functor Transport_Structures
*)

functor Transport_Ids (structure Lower: NETWORK_PROTOCOL): 
							TRANSPORT_IDS =
struct
  type host_id = Lower.Host_Id.T
  type port = Word16.word
  val w16to31 = Word.fromLargeWord o Word16.toLargeWord
  structure Host_Id = Lower.Host_Id
  structure Key =
   struct
     type host_id = host_id
     type port = port
     datatype key = Key of {peer: host_id, local_port: port, remote_port: port}
     type T = key
     fun makestring (Key {peer, local_port, remote_port}) = 
          "peer = " ^ Lower.Host_Id.makestring peer ^
           ", local_port = " ^ (Integer.toString o Word16.toInt) local_port ^
          ", remote_port = " ^ (Integer.toString o Word16.toInt) remote_port
     fun equal (Key {peer = p1, local_port = l1, remote_port = r1},
                Key {peer = p2, local_port = l2, remote_port = r2}) = 
          Lower.Host_Id.equal (p1, p2) andalso l1 = l2 andalso r1 = r2
     fun hash (Key {peer, local_port, remote_port}) = 
          Lower.Host_Id.hash peer + w16to31 local_port + w16to31 remote_port
   end

  structure Address =
   struct
     type host_id = host_id
     type port = port
     datatype address = 
         Complete of {peer: host_id, local_port: port, remote_port: port}
       | Remote_Specified of {peer: host_id, remote_port: port}
     type T = address
     fun makestring (Complete {peer, local_port, remote_port}) = 
          "peer = " ^ Lower.Host_Id.makestring peer ^
          ", local_port = " ^ (Integer.toString o Word16.toInt) local_port ^
	  ", remote_port = " ^ (Integer.toString o Word16.toInt) remote_port
       | makestring (Remote_Specified {peer, remote_port}) = 
          "peer = " ^ Lower.Host_Id.makestring peer ^
	  ", remote_port = " ^ (Integer.toString o Word16.toInt) remote_port
     fun equal (Complete a1, Complete a2) = Key.equal (Key.Key a1, Key.Key a2)
       | equal (Remote_Specified {peer = p1, remote_port = r1},
		Remote_Specified {peer = p2, remote_port = r2}) = 
          Lower.Host_Id.equal (p1, p2) andalso r1 = r2
       | equal _ = false
     fun hash (Complete {peer, local_port, remote_port}) = 
	  Lower.Host_Id.hash peer + w16to31 local_port + w16to31 remote_port
       | hash (Remote_Specified {peer, remote_port}) = 
	  Lower.Host_Id.hash peer + w16to31 remote_port
   end

  structure Pattern =
   struct
     type host_id = host_id
     type port = port
     datatype pattern = 
         Complete of {peer: host_id, local_port: port, remote_port: port}
       | Remote_Specified of {peer: host_id, remote_port: port}
       | Local_Specified of {local_port: port}
       | Unspecified
     type T = pattern
     fun makestring (Complete {peer, local_port, remote_port}) = 
          "peer = " ^ Lower.Host_Id.makestring peer ^
           ", local_port = " ^ (Integer.toString o Word16.toInt) local_port ^
	  ", remote_port = " ^ (Integer.toString o Word16.toInt) remote_port
       | makestring (Remote_Specified {peer, remote_port}) = 
          "peer = " ^ Lower.Host_Id.makestring peer ^
	  ", remote_port = " ^ (Integer.toString o Word16.toInt) remote_port
       | makestring (Local_Specified {local_port}) = 
	  "local_port = " ^ (Integer.toString o Word16.toInt) local_port
       | makestring (Unspecified) = "Unspecified"
     fun equal (Complete a1, Complete a2) = Key.equal (Key.Key a1, Key.Key a2)
       | equal (Remote_Specified {peer = p1, remote_port = r1},
		Remote_Specified {peer = p2, remote_port = r2}) = 
          Lower.Host_Id.equal (p1, p2) andalso r1 = r2
       | equal (Local_Specified {local_port = l1},
		Local_Specified {local_port = l2}) = l1 = l2
       | equal (Unspecified, Unspecified) = true
       | equal _ = false
     fun hash (Complete {peer, local_port, remote_port}) = 
	  Lower.Host_Id.hash peer + w16to31 local_port + w16to31 remote_port
       | hash (Remote_Specified {peer, remote_port}) = 
	  Lower.Host_Id.hash peer + w16to31 remote_port
       | hash (Local_Specified {local_port}) = w16to31 local_port
       | hash (Unspecified) = 0w0
   end

end
