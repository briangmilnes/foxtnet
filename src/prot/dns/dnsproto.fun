(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Low level DNS Protocol implementation


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Dns_Protocol

		iii.	RCS Log
	
$Log: dnsproto.fun,v $
Revision 1.6  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.5  96/04/18  21:30:59  cline
converted hash from int to word

Revision 1.4  1996/03/04  21:30:17  esb
changed Dns_Address to be Lower.Host_Id.

Revision 1.3  1996/02/07  19:17:38  cline
renumbered next_id

Revision 1.2  1996/01/19  23:05:23  esb
adapted to the new wordarray signature.

Revision 1.1  1996/01/16  22:00:09  cline
Initial revision

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision



	1.	functor Dns_Protocol
*)

functor Dns_Protocol (structure B: FOX_BASIS
		      structure Lower: UDP_PROTOCOL
			where type host_id = Word32.word
		      val dns_port: Lower.Transport_Address.port
		      val debug_level: int ref option): DNS_PROTOCOL = 
 struct
   structure Message = Dns_Message (structure In = Lower.Incoming
				    structure Out = Lower.Outgoing
				    structure B = B
				    val debug_level = debug_level)

   structure Dns_External =
     struct
       exception Dns_Proto_Operation_Not_Supported
       fun die _ = raise Dns_Proto_Operation_Not_Supported
       type T = Message.message
       val makestring = Message.makestring
       val new = die
       val uninitialized = die
       val size = die
       val sub = die
       val update = die
       val join = die
       val split = die
       val fold = die
       fun makestring_max (m, max) =
	    let val s = makestring m
	        val int_max = Word.toInt max
	    in if B.V.String.length s <= int_max then s
	       else B.V.String.substring (s, 0, int_max)
	    end
     end

   structure Incoming = Dns_External
   structure Outgoing = Dns_External

   structure Dns_Address = Lower.Host_Id

   structure Dns_Pattern =
     struct
       datatype T = Pattern
       fun makestring Pattern = "Any"
       fun equal (Pattern, Pattern) = true
       fun hash Pattern = 0w0
     end

   structure Dns_Connection_Key =
     struct
       datatype T = Key of {host: Lower.Host_Id.T, id: Word16.word}

       fun makestring (Key {host, id}) =
	    Lower.Host_Id.makestring host ^ " id = 0x" ^ Word16.toString id

       fun equal (Key {host=host0, id=id0},
		  Key {host=host1, id=id1}) =
	    id0 = id1 andalso Lower.Host_Id.equal (host0, host1)

       fun hash (Key {host, id}) =
	    Lower.Host_Id.hash host +
	    Word.fromLargeWord (Word16.toLargeWord id)
     end

   fun resolve (_, host) =
     SOME (Lower.Transport_Address.Remote_Specified
	   {peer=host, remote_port=dns_port})

   val next_id = ref (Word16.fromInt 0)

   fun new_id (id_host, {conns, listens}) =
     let
       fun host_ids [] = []
	 | host_ids (Dns_Connection_Key.Key {host, id}::rest) =
	 if Lower.Host_Id.equal (id_host, host)
	   then id::host_ids rest
	 else host_ids rest
       val ids = host_ids (conns ())
       fun member (i, []) = false
	 | member (i, id::rest) = i=id orelse member (i,rest)
       fun increment x = x := Word16.+ (!x, Word16.fromInt 1)
     in
       while member (!next_id, ids) do increment next_id;
	 !next_id before increment next_id
     end

   fun make_key (_, host, _, info) =
     Dns_Connection_Key.Key {host=host, id=new_id (host, info)}

   fun map_pattern _ = SOME ((), Lower.Transport_Pattern.Local_Specified
			     {local_port=dns_port})

   fun match _ = true

   fun init_connection _ = ((),())

   fun fin_connection _ = ()

   local
     val id_cursor = 0w0
     structure W16 =
       Protocol_Extern16_Big (structure In = Lower.Incoming
			      structure Out = Lower.Outgoing
			      structure B = B)
   in
     fun send (Dns_Connection_Key.Key {id, ...}, state) outgoing =
       let val packet = Message.marshal outgoing
       in
	 W16.marshal (packet, id) id_cursor;
	 [packet]
       end

     fun identify (Lower.Transport_Key.Key {peer, ...}, _) packet =
       let
	 val (id, _) = W16.unmarshal (packet,  id_cursor)
       in
	 [Dns_Connection_Key.Key {host = peer, id = id}]
       end
   end

   fun receive _ packet = SOME (Message.unmarshal packet)

   fun undelivered _ _ = ()

   fun lower_status _ _ = ()

   structure Conn =
     Connection(structure Lower = Lower
		structure Setup = Lower.Setup
		structure Address = Dns_Address
		structure Pattern = Dns_Pattern
		structure Connection_Key = Dns_Connection_Key
		structure Incoming = Incoming
		structure Outgoing = Outgoing
		structure Status = Lower.Status
		structure Count = Lower.Count
		structure X = Lower.X
		type connection_extension = unit
		type listen_extension = unit
		type session_extension = unit
		type connection_state = unit
		type protocol_state = unit
		val lower_setup = (fn setup => setup)
		val init_proto = (fn _ => ((),()))
		val fin_proto = (fn _ => ())
		val resolve = resolve
		val make_key = make_key
		val map_pattern = map_pattern
		val match = match
		val init_connection = init_connection
		val fin_connection = fin_connection
		val send = send
		val identify = identify
		val receive = receive
		val undelivered = undelivered
		val lower_status = lower_status
		structure B = B
		val module_name = "dnsproto.fun"
		val debug_level = debug_level)

   open Conn

 end (* struct *)
