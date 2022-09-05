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


	IP packet fragmentation and reassembly support. The reassembly
	algorithm is taken from RFC 815.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ip_Fragment
	2.	internal function keep_coped
	3.	internal function round_down
	4.	internal function split_loop
	5.	function fragment
	6.	type hole
	7.	function split_hole
	8.	function update_holes
	9.	type fragment_list
	10.	function restrict
	11.	function insert_fragment
	12.	function merge_fragments
	13.	reassembly context (state, type T)
	14.	function reassemble
	15.	function new
	16.	function gc

		iii.	RCS Log
	
$Log: ipfrag.fun,v $
Revision 1.40  1997/01/24  14:47:01  cline
replaced max with Int.max

Revision 1.39  1996/04/18  21:19:46  cline
converted hash from into to word

Revision 1.38  1996/02/23  21:13:06  esb
updated a comment.

Revision 1.37  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.36  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.35  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.34  1995/08/08  18:22:07  esb
changed to support ICMP.

Revision 1.33  1995/06/26  17:30:13  esb
added some debugging statements.

Revision 1.32  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.31  1995/06/20  17:04:14  esb
intermediate state, does not work yet.

Revision 1.30  1995/03/24  16:00:28  esb
replaced signExtend with wordToInt.

Revision 1.29  1995/03/12  17:50:04  esb
adapted to new trace.sig.

Revision 1.28  1995/03/07  20:35:14  esb
updated tracing.

Revision 1.27  1995/02/04  20:40:05  robby
updated to 107

Revision 1.26  1995/01/18  21:01:31  esb
adapted to new coro.sig

Revision 1.25  1994/11/11  18:10:29  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.24  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.23  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.22  1994/09/14  15:30:41  milnes
Corrected garbage collection of fragments.

Revision 1.21  1994/08/24  22:09:09  esb
made reassembly consider the IP length instead of the packet length

Revision 1.20  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.19  1994/07/04  21:32:56  esb
adapted to Copy/Create split.

Revision 1.18  1994/07/01  02:27:48  danwang
Moved control structures into Fox_Basis.

Revision 1.17  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.16  1994/06/05  18:43:00  milnes
Add icmp timeout on reassembly message.

Revision 1.15  1994/05/10  07:50:16  esb
adapted to new store.sig and receive_packet.sig.

Revision 1.14  94/05/04  01:41:20  esb
brought up to date with the new coroutine and event signatures.

Revision 1.13  94/05/03  20:55:11  milnes
Added ip option handling.

Revision 1.12  1994/04/16  00:39:20  esb
many small fixes and optimizations.

Revision 1.11  94/04/06  23:13:16  esb
substantially rewrote the reassembly code.

Revision 1.10  94/03/09  23:22:20  esb
only copy headers once now on fragmentation.

Revision 1.9  94/03/09  03:30:02  esb
major optimization, minor restructuring. Added header_copy.

Revision 1.8  94/03/04  02:33:35  milnes
Added some tracing to the gc_thread.

Revision 1.7  1994/02/21  00:00:14  esb
modified send to adapt to new send_packet interface.

Revision 1.6  94/02/09  18:02:45  esb
added a g.c. thread.

Revision 1.5  94/02/08  15:06:33  esb
changed to adapt to new interface.

Revision 1.4  1994/01/30  02:52:04  esb
bug fix.

Revision 1.3  1994/01/29  04:47:38  esb
many fixes, including a major bug fix (added hide_trailer).

Revision 1.2  1994/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.1  94/01/13  15:00:43  cline
Initial revision



		1.	functor Ip_Fragment

	Ip_Fragment provides two main functions: fragment and
	reassembly. See the IP standard (RFC 791) for details of the
	specification. Basically, packets need to be fragmented on the
	way out if they are larger than the maximum transmission unit
	(MTU), and reassembled when they are received. Once all the
	fragments for a packet are received, the packet is returned to
	the caller of reassemble.

 *)

functor Ip_Fragment (structure B: FOX_BASIS
		     structure Header: IP_HEADER_EXTERN
		     structure Incoming: EXTERNAL
		     structure Outgoing: EXTERNAL
		     val debug_level: int ref option): IP_FRAGMENT =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipfrag.fun"
			   val makestring = fn _ => NONE)

  type ip_header = Header.header
  type incoming = Incoming.T
  type outgoing = Outgoing.T

(*
		2.	internal function keep_coped
*)

  local

   val zero16 = Word16.fromInt 0

   fun keep_copied [] = []
     | keep_copied (first :: rest) =
        if Header.Option.send_in_fragments first then first :: keep_copied rest
	else keep_copied rest

(*
		3.	internal function round_down
*)

   fun round_down (value, step: Word.word) = value - value mod step

(*
		4.	internal function split_loop
*)

   fun split_loop (size, min_size, packet, offset,
		   header as
		   (Header.V4 {tos, data_length, identification, flags, ttl,
			       protocol, source, destination, options})) =
        let val packet_size = Outgoing.size packet
	    val header_size = Header.size header
	    val total_size = packet_size + header_size
        in if total_size <= size then
	    [(packet, Header.V4 {tos = tos,
				 data_length = Word16.fromInt
				                  (Word.toInt packet_size),
				 identification = identification,
				 flags = Header.Flag.Last_Fragment offset,
				 ttl = ttl, protocol = protocol,
				 source = source, destination = destination,
				 options = options},
	      header_size)]
	   else
	    let val first_estimate = round_down (size - header_size, 0w8)
	        val first_size =
		     if total_size - first_estimate >= min_size then
		      first_estimate
		     else
		      round_down (packet_size div 0w2 + 0w8, 0w8)
	        val (first, rest) = Outgoing.split (packet, first_size)
	        val new_options = if offset <> zero16 then options
				  else keep_copied options
		val first_size = Outgoing.size first
	    in (first,
	        Header.V4 {tos = tos,
			   data_length = Word16.fromInt
			                    (Word.toInt first_size),
			   identification = identification,
			   flags = Header.Flag.Fragment offset,
			   ttl = ttl, protocol = protocol,
			   source = source, destination = destination,
			   options = options},
		header_size) ::
	       split_loop (size, min_size, rest,
			   Word16.+ (offset,
					Word16.fromInt
					(Word.toInt (first_size div 0w8))),
			   Header.V4 {tos = tos,
				      data_length = data_length, (* ignored *)
				      identification = identification,
				      flags = Header.Flag.None, ttl = ttl,
				      protocol = protocol, source = source,
				      destination = destination,
				      options = new_options})
	    end
	end

(*
		5.	function fragment
*)

  in (* local *)
   fun fragment ({packet, header, max_size, min_size}) =
        split_loop (max_size, min_size, packet, zero16, header)
  end (* local *)

(*
		6.	type hole
*)

  datatype hole = Hole of {first: Word.word, (* number of first byte in hole.*)
			   last: Word.word} (* number of last byte in hole. *)
                | Last_Hole of {first: Word.word} (* logically last hole. *)

  (* first_hole is what is used to correctly create a list given the
     first fragment of a key. *)
  val first_hole = [Last_Hole {first = 0w0}]

  fun makestring_hole (Last_Hole {first}) =
       Word.toString first ^ "..oo"
    | makestring_hole (Hole {first, last}) =
       Word.toString first ^ ".." ^ Word.toString last

  fun fold_hole (hole, "") = makestring_hole hole
    | fold_hole (hole, string) = makestring_hole hole ^ ", " ^ string

  fun makestring_holes list = B.V.List.fold fold_hole list ""

(*
		7.	function split_hole
*)

   fun split_hole (hole_first, hole_last, frag_first, frag_last) =
	if frag_first > hole_first then
	 Hole {first = hole_first, last = frag_first - 0w1} ::
	 split_hole (frag_first, hole_last, frag_first, frag_last)
	else if frag_last < hole_last then
	 [Hole {first = frag_last + 0w1, last = hole_last}]
	else []
        
(*
		8.	function update_holes

	If the fragment is complete, the returned list is empty.
	The common case is: the list is [Last_Hole {first}],
	frag_first = first (so frag_last >= first), more_frags = true.
*)

       (* common case *)
   fun update_holes ([Last_Hole {first}], frag_first, frag_last, more_frags) =
        let val res = if more_frags then [Last_Hole {first = frag_last + 0w1}]
		      else []
	in if frag_first = first then res
	   else if frag_first > first then
	    Hole {first = first, last = frag_first - 0w1} :: res
	   else if frag_last >= first then res
	   else [Last_Hole {first = first}]
	end
     | update_holes ([], _, _, _) = []
     | update_holes ((head as (Hole {first, last})) :: rest,
		     frag_first, frag_last, more_frags) =
        if frag_first > last orelse frag_last < first then
	 head :: update_holes (rest, frag_first, frag_last, more_frags)
	else
	 split_hole (first, last, frag_first, frag_last) @
	 update_holes (rest, frag_first, frag_last, more_frags)
     | update_holes ((head as (Last_Hole {first})) :: rest,
		     frag_first, frag_last, more_frags) =
        let val res = if more_frags then [Last_Hole {first = frag_last + 0w1}]
		      else []
	    val tail = update_holes (rest, frag_first, frag_last, more_frags)
	in if frag_first = first then res @ tail
	   else if frag_first > first then
	    Hole {first = first, last = frag_first - 0w1} :: (res @ tail)
	   else if frag_last >= first then res @ tail
	   else Last_Hole {first = first} :: tail
	end

(*
		9.	type fragment_list

	For each fragment list, we keep a time-to-live (ttl).
	Following RFC 1122, p. 57, we set the reassembly timeout to
	reassembly_timeout when we receive the first fragment of a
	packet.
*)

   datatype key = Reassembly_Key of {remote_ip: Word32.word,
				     local_ip: Word32.word,
				     protocol_id: Word8.word,
				     packet_id: Word16.word}

   datatype fragment_list =
       FL of {holes: hole list,
	      ttl: int,
	      key: key,
	      headers: (Header.header * Incoming.T) option,
	      fragments: {fragment: Incoming.T, first: Word.word,
			  last: Word.word} list}

   fun hash_key (Reassembly_Key {packet_id, ...}) =
        Word.fromLargeWord (Word16.toLargeWord packet_id)
   fun eq_key (a, b: key) = a = b

   fun makestring_fragment {fragment, first, last} =
        Word.toString first ^ "..." ^ Word.toString last

   fun fold_fragment (fragment, "") = makestring_fragment fragment
     | fold_fragment (fragment, string) =
        makestring_fragment fragment ^ ", " ^ string

   fun makestring_fragments fraglist = B.V.List.fold fold_fragment fraglist ""

(*
	10.	function restrict

	Reduce the size of a fragment as specified by new_first.
*)

   fun restrict (old, new_first) =
        let val {fragment, first = old_first, last} = old
	    val delta = new_first - old_first
	    val (_, restricted) = Incoming.split (fragment, delta)
	in {fragment = restricted, first = new_first, last = last}
	end

(*
		11.	function insert_fragment

	Insert the fragment in the proper place (descending fragment
	offset) in the fragment list, hiding any overlapping headers
	or trailers.
*)

   fun insert_fragment (new, []) = [new]
     | insert_fragment (new as {fragment = new_frag, first = new_first,
				last = new_last},
			(head as {fragment, first, last}) :: rest) =
        if new_first > last then	(* perfect match, common case. *)
	 new :: head :: rest
					(* else: new_first <= last *)
	else if first > new_last then	(* no overlap, insert further down *)
	 head :: insert_fragment (new, rest)
	else				(* some overlap: *)
	 if first <= new_first andalso last >= new_last then
	  (* new is fully contained in old, discard new *)
	  head :: rest
(* The part that follows has never been tested and may never be tested,
   since it is only invoked when we receive overlapping fragments. *)
	 else if first >= new_first andalso last <= new_last then
	  insert_fragment (new, rest)	(* discard old, check for overlap *)
	 (* else: neither fragment is fully contained in the other, so either
	    new_last > last >= new_first > first, or 
	    last > new_last >= first > new_first *)
	 else if first > new_first then
	  restrict (head, new_last + 0w1) :: insert_fragment (new, rest)
	 else
	  restrict (new, last + 0w1) :: head :: rest

(*
		12.	function merge_fragments

	Merge fragments after updating the total length field of
	the header of the first packet, which is at the end of the list.
*)

   val one16 = Word16.fromInt 1

   type ip_packet = incoming * ip_header * incoming (* raw packet *)
   fun merge_fragments (fragments, NONE) = NONE
     | merge_fragments ([], _) = NONE
     | merge_fragments (frags as ({fragment, first, last} :: rest),
			SOME headers) =
        let val data_size = Word16.+ (Word16.fromInt (Word.toInt last), one16)
	    fun get_fragment {fragment, first, last} = fragment
	    val new_frags = map get_fragment (rev frags)
	    val (Header.V4 {tos, data_length, identification, flags,
			    ttl, protocol, source, destination,
			    options}, raw_packet) = headers
	    val new_header = Header.V4 {tos = tos, data_length = data_size,
					identification = identification,
					flags = Header.Flag.None,
					ttl = ttl, protocol = protocol,
					source = source,
					destination = destination,
					options = options}
	    fun append [] = Incoming.uninitialized 0w0
	      | append [last] = last
	      | append (first :: rest) =
		 Incoming.join (first, append rest)
	in SOME (append new_frags, new_header, raw_packet)
	end

(*
		13.	reassembly context (state, type T)
*)

   type ip_packet = incoming * ip_header * incoming (* raw header *)
   type T = ((key, fragment_list) B.Store.T *
	     ({header: ip_header, fragment: incoming,
	       raw_packet: incoming} -> unit))

(*
		14.	function reassemble
*)

  fun reassemble ((store, expired), (packet, header, raw_packet)) =
       let val Header.V4 {source, destination, protocol, identification,
			  ttl = ttl8, flags, data_length, ...} = header
	   val ttl = Word8.toInt ttl8
	   val key = Reassembly_Key {remote_ip = source,
				     local_ip = destination,
				     protocol_id = protocol,
				     packet_id = identification}
	   val (first_offset, more) =
	         case flags of
		    Header.Flag.None => (0w0, false)
		  | Header.Flag.Dont_Fragment => (0w0, false)
		  | Header.Flag.Fragment offset =>
		     (Word.fromInt (Word16.toInt offset) * 0w8, true)
		  | Header.Flag.Last_Fragment offset =>
		     (Word.fromInt (Word16.toInt offset) * 0w8, false)
	   val last_offset = first_offset
	                   + Word.fromInt (Word16.toInt data_length) - 0w1
           val (store1, holes, ttl, fragments, old_headers) =
	         case B.Store.look (store, key) of
		    NONE => (store, first_hole, ttl, [], NONE)
		  | SOME (new_store,
			  FL {holes, ttl = old_ttl, key,
			      headers, fragments}) =>
		     (new_store, holes, Int.max (old_ttl, ttl), fragments,
		      headers)
	   val new_headers =
	        if first_offset = 0w0 then SOME (header, raw_packet)
		else old_headers
	   val new_holes = update_holes (holes, first_offset,
					 last_offset, more)
	   val frag_desc = {fragment = packet, first = first_offset,
			    last = last_offset}
	   val new_fragments = insert_fragment (frag_desc, fragments)
       in if new_holes = [] then	(* successful reassembly. *)
	   (merge_fragments (new_fragments, new_headers),
	    (B.Store.remove (store1, key), expired))
	  else				(* not (yet) done. *)
	   (Trace.debug_print (fn _ =>
			       "new holes = " ^ makestring_holes new_holes ^
			       ", new fragments = " ^
			       makestring_fragments new_fragments);
	    (NONE,
	     (B.Store.add (store1, key,
			   FL {holes = new_holes, ttl = ttl, key = key,
			       headers = new_headers,
			       fragments = new_fragments}),
	      expired)))
       end

(*
		15.	function new
*)

  fun new expired = (B.Store.new (hash_key, eq_key), expired)

(*
		16.	function gc

	This function is designed to execute once per second.  On each
	execution, it removes dead fragment lists (which have a ttl of
	zero) and then decrements the ttl of all other fragment lists.
	When we remove an initial fragment, we call expired, so the
	ICMP above IP (if any) can generate a Time_Exceeded message.
	(RFC 792 p. 6, RFC 1122 p. 41)
*)

  local
   fun dead expired (_, FL {ttl, fragments, headers, ...}) =
	case fragments of
	   [] =>
	    (Trace.local_print "found empty fragment, expiring it";
	     true)		(* it's an illegal fragment list, remove it. *)
	 | (head :: rest) =>
	    if ttl <= 0 then 
	     (Trace.debug_constant_string "found dead fragment, expiring it";
	      case headers of
	         NONE => ()
	       | SOME (header, raw_packet) =>
		  let val {first, fragment, last} = head
		  in expired {header = header, fragment = fragment,
			      raw_packet = raw_packet}
		  end;
	      (* This is a dead fragment list. *)
	      true)
	    else false (* Not a dead fragment list. *)

   fun decrement (k, FL {ttl, holes, key, headers, fragments}) =
        (Trace.debug_constant_string "decrementing fragment ttl";
	 FL {ttl = Int.max (ttl, 1) - 1, holes = holes, key = key,
	     headers = headers, fragments = fragments})
  in

   fun gc (store, expired) =
        (B.Store.map decrement (B.Store.remove_selected (store, dead expired)),
	 expired)

  end

 end
