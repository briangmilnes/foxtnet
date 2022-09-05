(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	This functor marshals and unmarshals IP headers.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_FLAG
	2.	signature IP_FLAG_EXTERN
	3.	functor Ip_Flag_Extern
	4.	signature IP_HEADER_EXTERN
	5.	functor Ip_Header
	6.	sub-structure Trace
	7.	word sub-structures
	8.	flag and option sub-structures
	9.	exported types, exceptions
	10.	function size
	11.	function marshal
	12.	function route_dest
	13.	function unmarshal
	14.	function identify
	15.	function identify_pointer
	16.	function lengths

	iii.	RCS Log
	
$Log: ipheader.fun,v $
Revision 1.17  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.16  97/06/04  11:39:49  esb
added a check to use slow_identify if the packet is not aligned correctly.

Revision 1.15  97/04/07  14:07:58  esb
optimized for common case.

Revision 1.14  1996/02/23  21:14:27  esb
changed identify to return whether a packet is a fragment.

Revision 1.13  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.12  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.11  1995/10/04  21:32:15  esb
added a fast, checksum-less access to IP header/data lengths.

Revision 1.10  1995/10/03  18:54:16  esb
identify now returns destination, for validity checking.

Revision 1.9  1995/09/26  16:24:18  esb
change needed to adapt to new ipoption functor.

Revision 1.8  1995/09/19  17:38:58  cline
fixed shift

Revision 1.7  1995/09/13  15:29:18  esb
added inserting the destination into a received route.

Revision 1.6  1995/08/24  00:50:45  esb
added some abstraction.

Revision 1.5  1995/08/08  18:22:30  esb
changed to support ICMP.

Revision 1.4  1995/06/28  20:06:02  cline
Fixed the type of identify from using extern_out to extern_in

Revision 1.3  1995/06/27  19:01:20  cline
adapted to new extern.sig

Revision 1.2  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.1  1995/06/20  17:05:46  esb
Initial revision


	1.	signature IP_FLAG
*)

signature IP_FLAG =
 sig
  datatype flag =
      None
    | Dont_Fragment
    | Fragment of Word16.word (* offset, in 8-octet units *)
    | Last_Fragment of Word16.word (* offset, in 8-octet units *)
  val makestring: flag -> string
 end

(*
	2.	signature IP_FLAG_EXTERN
*)

signature IP_FLAG_EXTERN =
 sig
  include EXTERN
  include IP_FLAG
  sharing type T = flag

  val unmarshal_word: Word32.word -> T
 end

(*
	3.	functor Ip_Flag_Extern
*)

functor Ip_Flag_Extern (structure Marshal_Word16: EXTERN
			 where type T = Word16.word): IP_FLAG_EXTERN =
 struct
  local
   structure S:
    sig
     type extern_in
     type extern_out
     type cursor
     exception Extern
    end
      = Marshal_Word16
  in
   open S
  end

  datatype flag = None
                | Dont_Fragment
	        | Fragment of Word16.word (* offset, in 8-octet units *)
	        | Last_Fragment of Word16.word (* offset,
                                                     in 8-octet units *)

  fun makestring None = "none"
    | makestring Dont_Fragment = "don't fragment"
    | makestring (Fragment offset) =
       "fragment (" ^ (Integer.toString o Word16.toInt) offset ^ ")"
    | makestring (Last_Fragment offset) =
       "last fragment (" ^ (Integer.toString o Word16.toInt) offset ^ ")"

  type T = flag

  val zero_flag = Word16.fromInt 0
  val dont_fragment = Word16.fromInt 0x4000
  val more_fragments = Word16.fromInt 0x2000
  val offset_mask = Word16.- (more_fragments, Word16.fromInt 1)

  fun size _ = 0w2

  fun marshal (array, None) = Marshal_Word16.marshal (array, zero_flag)
    | marshal (array, Dont_Fragment) =
       Marshal_Word16.marshal (array, dont_fragment)
    | marshal (array, Fragment offset) =
       Marshal_Word16.marshal (array,
		       Word16.orb (Word16.andb (offset, offset_mask),
				      more_fragments))
    | marshal (array, Last_Fragment offset) =
       Marshal_Word16.marshal (array, Word16.andb (offset, offset_mask))

  local
   fun unmarshal_word16 word =
        if word = zero_flag then None
	else if Word16.andb (word, dont_fragment) = dont_fragment then
	 Dont_Fragment
	else if Word16.andb (word, more_fragments) = more_fragments then
	 Fragment (Word16.andb (word, offset_mask))
	else
	 Last_Fragment (Word16.andb (word, offset_mask))

  in
   fun unmarshal argument =
       let val (data, cursor) = Marshal_Word16.unmarshal argument
       in (unmarshal_word16 data, cursor)
       end

   fun unmarshal_word word = unmarshal_word16 (Word16.fromLargeWord word)

 end (* local *)

 end (* struct *)

(*
	4.	signature IP_HEADER_EXTERN
*)

signature IP_HEADER_EXTERN =
 sig
  include EXTERN
  type ip_number = Word32.word
  type ip_protocol = Word8.word

  structure Option: IP_OPTION_EXTERN
  structure Flag: IP_FLAG_EXTERN

  datatype header = V4 of {tos: Word8.word,
			   data_length: Word16.word,
			   identification: Word16.word,
			   flags: Flag.flag,
			   ttl: Word8.word,
			   protocol: ip_protocol,
			   source: ip_number,
			   destination: ip_number,
			   options: Option.ip_option list}
  val makestring: header -> string
  val equal: header * header -> bool

  sharing type T = header

  val identify: extern_in * cursor
              -> {source: ip_number, dest: ip_number,
		  protocol: ip_protocol, fragment: bool} option

  datatype pointer_position = Header | Option of Option.ip_option option | Data
  val identify_pointer: header * Word.word -> pointer_position

  val lengths: extern_in * cursor
             -> {header: Word.word, data: Word.word} option
 end

(*
	5.	functor Ip_Header
*)

functor Ip_Header (structure In: EXTERNAL
		   structure Out: EXTERNAL
		   val debug_level: int ref option
		   structure B: FOX_BASIS): IP_HEADER_EXTERN =
 struct

(*
	6.	sub-structure Trace
*)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipheader.fun"
			   val makestring = fn _ => NONE)

(*
	7.	word sub-structures
*)

  structure Marshal_Word8 = 
      Protocol_Extern8 (structure In = In
			structure Out = Out
			structure B = B)

  structure Marshal_Word16 =
      Protocol_Extern16_Big (structure In = In
			     structure Out = Out
			     structure B = B)

  structure Marshal_Word32 =
      Protocol_Extern32_Big (structure In = In
			     structure Out = Out
			     structure B = B)

(*
	8.	flag and option sub-structures
*)

  structure Flag = Ip_Flag_Extern (structure Marshal_Word16 = Marshal_Word16)

  structure Option =
      Ip_Option_Extern (structure Marshal_Word8 = Marshal_Word8
			structure Marshal_Word16 = Marshal_Word16
			structure Marshal_Word32 = Marshal_Word32
			val debug_level = debug_level
			structure B = B)

(*
	9.	exported types, exceptions
*)

  type ip_number = Word32.word
  type ip_protocol = Word8.word

  datatype header = V4 of {tos: Word8.word,
			   data_length: Word16.word,
			   identification: Word16.word,
			   flags: Flag.flag,
			   ttl: Word8.word,
			   protocol: Word8.word,
			   source: ip_number,
			   destination: ip_number,
			   options: Option.ip_option list}

  fun makestring (V4 {tos, data_length, identification, flags, ttl,
		      protocol, source, destination, options}) =
       "version = 4, tos = " ^ Word8.fmt StringCvt.DEC tos ^
       ", data length = " ^ (Integer.toString o Word16.toInt) data_length ^
       ", id = " ^ (Integer.toString o Word16.toInt) identification ^
       ", flags(offset) = " ^ Flag.makestring flags ^
       ", ttl = " ^ Word8.fmt StringCvt.DEC ttl ^
       ", protocol = " ^ Word8.fmt StringCvt.DEC protocol ^
       ", source = " ^ Option.makestring_ip_number source ^
       ", destination = " ^ Option.makestring_ip_number destination ^
       (if options = [] then ""
	else ", options = " ^ Option.makestrings options)

  fun equal (h1, h2) =
       case (h1, h2) of
	  (V4 {options = [], ...}, V4 {options = [], ...}) => h1 = h2
	| _ =>
	   let fun clear_options (V4 {tos, data_length, identification,
				      flags, ttl, protocol, source,
				      destination, options}) =
	            (V4 {tos = tos, data_length = data_length,
			 identification = identification,
			 flags = flags, ttl = ttl, protocol = protocol,
			 source = source, destination = destination,
			 options = []}, options)
	       val (new_h1, o1) = clear_options h1
	       val (new_h2, o2) = clear_options h2
	   in new_h1 = new_h2 andalso
	      Option.equal (o1, o2)
	   end

  type T = header
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = Word.word
  exception Extern

(*
	10.	function size
*)

  val min_size = 0w20
  val min_size_minus_one = min_size - 0w1
  val min_size8 = Word8.fromInt (Word.toInt min_size)
  val min_size32 = Word32.fromInt (Word.toInt min_size)

  val version4 = 0w4 : Word8.word
  val marshal_version4 = Word8.<< (version4, 0w4)
  val version6 = 0w6 : Word8.word
  val marshal_version6 = Word8.<< (version6, 0w4)

  val initial_checksum = Word16.fromInt 0

  val hlen_mask = 0wxf : Word8.word

  fun size (V4 {options = [], ...}) = min_size
    | size (V4 {options, ...}) = min_size + Option.size options

(*
	11.	function marshal

  "*std_*" functions are optimizations for marshaling IP headers that
  have no options and no flags.
*)

  local

   val ihlen = Word.toInt min_size
   val w16ihlen = Word16.fromInt ihlen
   val w32ihlen = Word32.fromInt ihlen
   val w32verhlen_const = Word32.<< (Word32.fromInt 0x45, 0w24)
   val w32x0 = Word32.fromInt 0
   val w32xff = Word32.fromInt 0xff
   val w32xffff = Word32.fromInt 0xffff
   val coded_std_verhlen = Word8.fromInt 0x45

   val std_header = Word_Array.from8
                     (Word_Array.W8.U_Big.F.create_uninitialized min_size)
   val (std_array, std_first, std_last) = Word_Array.expose std_header

   val _ = if std_first <> 0w0 orelse std_last <> min_size_minus_one then
            (print "error: unexpected values in first/last\n";
	     raise Subscript)
	   else ()

   fun std_marshal (base_array, cursor, tos, data_length, id,
		    ttl, protocol, source, destination) =
         let val length = Word32.+ (w32ihlen, Word16.toLargeWord data_length)
	     val w1 = Word32.orb (w32verhlen_const, length)
	     val w2 = Word32.<< (Word16.toLargeWord id, 0w16)
	     val w3x = Word32.orb (Word32.<< (Word8.toLargeWord ttl, 0w8),
				   Word8.toLargeWord protocol)
	     fun add (new, acc) =
                  Word32.+ (Word32.+ (Word32.>> (new, 0w16),
				      Word32.andb (new, w32xffff)),
			    acc)
	     val check = add (destination,
			      add (source, add (w2, add (w1, w3x))))
 (* since we are checksumming 9 half-words, we can have at most an
    overflow of 8, which is added it back in to give a 16-bit checksum. *)
	     val check16 = Word32.+ (Word32.andb (check, w32xffff),
				     Word32.>> (check, 0w16))
	     val w3 = Word32.orb (Word32.<< (w3x, 0w16),
				  Word32.xorb (check16, w32xffff))
	 in Pack32Big.update (std_array, 0, w1);
	    Pack32Big.update (std_array, 1, w2);
	    Pack32Big.update (std_array, 2, w3);
	    Pack32Big.update (std_array, 3, source);
	    Pack32Big.update (std_array, 4, destination);
	    Out.update (base_array, cursor, std_header);
	    cursor + min_size
         end

   val max_ip_header = 0w60		(* RFC 791, p. 13 *)

  in
   fun marshal (array, V4 {tos, data_length, identification, flags = Flag.None,
			   ttl, protocol, source, destination, options = []}) =
    (* optimized common case: flags = none, options = [] *)
        (fn cursor => std_marshal (array, cursor, tos, data_length,
				   identification, ttl,
				   protocol, source, destination))
     | marshal (array, V4 {tos, data_length, identification, flags, ttl,
			   protocol, source, destination, options}) =
        let val (option_size, option_marshal) =
	          case options of [] =>
		     (0w0, fn cursor => cursor)
		   | _ =>
		     let val size = Option.size options
		     in (size, Option.marshal ((array, size), options))
		     end
	    val hlen = min_size + option_size
	    val _ = if hlen > max_ip_header then
	             Trace.print_raise (Extern,
					SOME
					"marshal, IP header size too large")
		    else ()
	    val length = Word16.+ (Word16.fromInt (Word.toInt hlen),
				   data_length)
	    val verhlen = Word8.orb (marshal_version4,
				     Word8.>> (Word8.fromInt (Word.toInt hlen),
					       0w2))
	     (* add_checksum is called after all other marshaling functions,
	        so the cursor is at the end of the header. *)
	    val checksum_offset = 0w10
	    fun add_checksum header_end =
	         let val header_start = header_end - hlen
		     val (_, short_front) =
		           if header_start = 0w0 then (array, array)
			   else Out.split (array, header_start)
		     val (short_back, _) =
		           if hlen = Out.size short_front then
			    (short_front, short_front)
			   else Out.split (short_front, hlen)
		     val checksum_cursor = header_start + checksum_offset
		     val partial_check =
		           Out.fold (short_back, B.Checksum.check_partial,
				     B.Checksum.initial_state)
		     val raw = B.Checksum.complete_partial partial_check
		     val check = B.Checksum.one_s_complement raw
	         in Marshal_Word16.marshal (array, check) checksum_cursor;
		    header_end
	         end
	in add_checksum o
	   option_marshal o
	   Marshal_Word32.marshal (array, destination) o
	   Marshal_Word32.marshal (array, source) o
	   Marshal_Word16.marshal (array, initial_checksum) o
	   Marshal_Word8.marshal (array, protocol) o
	   Marshal_Word8.marshal (array, ttl) o
	   Flag.marshal (array, flags) o
	   Marshal_Word16.marshal (array, identification) o
	   Marshal_Word16.marshal (array, length) o
	   Marshal_Word8.marshal (array, tos) o
	   Marshal_Word8.marshal (array, verhlen)
        end

  end (* local *)

(*
	12.	function route_dest
*)

  fun insert_dest (input as {previous, available = []}, _) = input
    | insert_dest ({previous, available = first :: rest}, new) =
       {previous = previous @ [new], available = rest}

  fun route_dest ([], _) = []
    | route_dest (head :: tail, dest) =
       let val new_head =
                 case head of
	            Option.Loose_Route (Option.UA old) =>
		     Option.Loose_Route (Option.UA (insert_dest (old, dest)))
	          | Option.Strict_Route (Option.UA old) =>
		     Option.Strict_Route (Option.UA (insert_dest (old, dest)))
	          | Option.Record_Route (Option.UA old) =>
		     Option.Record_Route (Option.UA (insert_dest (old, dest)))
		  | _ => head
       in new_head :: route_dest (tail, dest)
       end

(*
	13.	function unmarshal
*)

  local
   val zero16 = Word16.fromInt 0
   val ones16 = Word16.fromInt 0xffff

   val zero32 = Word32.fromInt 0
   val xff32 = Word32.fromInt 0xff
   val xffff32 = Word32.fromInt 0xffff

   val std_ver_hlen = Word32.fromInt 0x45

   fun fast_unmarshal (w1, w2, w3, w4, w5) = (* version, hlength checked *)
        V4 {tos = Word8.fromLargeWord (Word32.andb (Word32.>> (w1, 0w16),
						    xff32)),
	    data_length = Word16.fromLargeWord (Word32.-
						(Word32.andb (w1, xffff32),
						 min_size32)),
	    identification = Word16.fromLargeWord (Word32.>> (w2, 0w16)),
	    flags = Flag.unmarshal_word (Word32.andb (w2, xffff32)),
	    ttl = Word8.fromLargeWord (Word32.>> (w3, 0w24)),
	    protocol = Word8.fromLargeWord (Word32.andb (Word32.>> (w3, 0w16),
							 xff32)),
	    source = w4,
	    destination = w5,
	    options = []}

   fun add_c (new, acc) =
        Word32.+ (Word32.+ (Word32.>> (new, 0w16),
			    Word32.andb (new, xffff32)), acc)

   fun fast_check (w1, w2, w3, w4, w5) =
        let val w1_sum = Word32.+ (Word32.>> (w1, 0w16),
				   Word32.andb (w1, xffff32))
	    val check = add_c (w5, add_c (w4, add_c (w3, add_c (w2, w1_sum))))
 (* since we are checksumming 10 half-words, we can have at most an
    overflow of 9, which is added it back in to give a 16-bit checksum. *)
	    val checksum = Word32.+ (Word32.andb (check, xffff32),
				     Word32.>> (check, 0w16))
	in checksum = xffff32 orelse checksum = zero32
	end

   fun slow_unmarshal (array, cursor) =
        let val (verhlen, c1) = Marshal_Word8.unmarshal (array, cursor)
	    val version = Word8.>> (verhlen, 0w4) 
	    val _ = if version <> version4 then
	             Trace.print_raise (Extern,
				        SOME ("unmarshal version " ^
					      Word8.fmt StringCvt.DEC version))
		    else ()
	    val hlen = Word.fromInt (Word8.toInt
				     (Word8.<< (Word8.andb
						(verhlen, hlen_mask),
					        0w2)))
	   val packet_size = In.size array
	   val _ = if hlen < min_size orelse hlen > packet_size then
	            Trace.print_raise (Extern,
				       SOME ("IP header length " ^
					     Word.toString hlen ^
					     ", minimum is 20, " ^
					     "packet size is " ^
					     Word.toString packet_size))
		   else ()
	   val (_, check1) = if cursor = 0w0 then (array, array)
			     else In.split (array, cursor)
	   val (check_array, _) = In.split (check1, hlen)
	   val checksum = B.Checksum.complete_partial
	                   (In.fold (check_array,
				     B.Checksum.check_partial,
				     B.Checksum.initial_state))
	   val _ = if checksum <> ones16 andalso checksum <> zero16 then
	            Trace.print_raise (Extern,
				       SOME ("IP header, checksum " ^
					     (Integer.toString o Word16.toInt)
					     checksum ^
					     ", header " ^
					     In.makestring check_array))
		   else
		    ()
	   val (tos, c2) = Marshal_Word8.unmarshal (array, c1)
	   val (length, c3) = Marshal_Word16.unmarshal (array, c2)
	   val (identification, c4) = Marshal_Word16.unmarshal (array, c3)
	   val (flags, c5) = Flag.unmarshal (array, c4)
	   val (ttl, c6) = Marshal_Word8.unmarshal (array, c5)
	   val (protocol, c7) = Marshal_Word8.unmarshal (array, c6)
	   val (checksum_field, c8) = Marshal_Word16.unmarshal (array, c7)
	   val (source, c9) = Marshal_Word32.unmarshal (array, c8)
	   val (destination, c10) = Marshal_Word32.unmarshal (array, c9)
	   val (options, c11) =
	         if hlen > min_size then
		  let val args = ((array, hlen - min_size), c10)
		      val (opts, cursor) = Option.unmarshal args
		  in (route_dest (opts, destination), cursor)
		  end
		 else ([], c10)
	   val data_length = Word16.- (length,
				       Word16.fromInt (Word.toInt hlen))
       in (V4 {tos = tos,
	       data_length = data_length,
	       identification = identification,
	       flags = flags,
	       ttl = ttl,
	       protocol = protocol,
	       source = source,
	       destination = destination,
	       options = options},
	   c11)
       end

   val fragment_offset = 0w6
   val protocol_offset = 0w9
   val source_offset = 0w12
   val dont_fragment = Word16.fromInt 0x4000
   val dont_fragment32 = Word32.fromInt 0x4000

   fun slow_identify (array, cursor) =
        (let val (proto, _) = Marshal_Word8.unmarshal
	                         (array, cursor + protocol_offset)
	     val (source, dest_offset) =
	            Marshal_Word32.unmarshal (array, cursor + source_offset)
	     val (dest, _) = Marshal_Word32.unmarshal (array, dest_offset)
	     val (frag, _) = 
	            Marshal_Word16.unmarshal (array, cursor + fragment_offset)
         in SOME {source = source, dest = dest, protocol = proto,
		  fragment = frag <> zero16 andalso frag <> dont_fragment}
         end)
	  handle x => NONE

  in
   fun unmarshal (array, cursor) =
	case In.fold (array, fn (wa, l) => wa :: l, []) of
	   [singleton] =>
	    let val (byte_array, first, last) = Word_Array.expose singleton
	        val offset = cursor + first
	        val index = Word.toInt offset div 4
	    in if offset mod 0w4 = 0w0 andalso (last - offset) >= min_size then
	        let val w1 = Pack32Big.subArr (byte_array, index)
	            val w2 = Pack32Big.subArr (byte_array, index + 1)
	            val w3 = Pack32Big.subArr (byte_array, index + 2)
	            val w4 = Pack32Big.subArr (byte_array, index + 3)
	            val w5 = Pack32Big.subArr (byte_array, index + 4)
		    val arg = (w1, w2, w3, w4, w5)
	        in if Word32.>> (w1, 0w24) = std_ver_hlen andalso
		      fast_check arg then
	            (fast_unmarshal (w1, w2, w3, w4, w5), cursor + min_size)
	           else
		    slow_unmarshal (array, cursor)
	        end
	       else
	        slow_unmarshal (array, cursor)
	    end
	 | _ => slow_unmarshal (array, cursor)

(*
	14.	function identify

	This is needed by IP as a quick way to identify the connection
	to which a packet should be directed.
*)

   fun identify (array, cursor) =
	(case In.fold (array, fn (wa, l) => (Word_Array.expose wa) :: l, []) of
	    [(byte_array, first, last)] =>
	     let val actual_index = Word.toInt (cursor + first)
	     in if actual_index mod 4 = 0 then
	         let val index = actual_index div 4
	             val w2 = Pack32Big.subArr (byte_array, index + 1)
		     val w3 = Pack32Big.subArr (byte_array, index + 2)
		     val w4 = Pack32Big.subArr (byte_array, index + 3)
		     val w5 = Pack32Big.subArr (byte_array, index + 4)
		     val p = Word8.fromLargeWord
		               (Word32.andb (Word32.>> (w3, 0w16), xff32))
		     val frag = Word32.andb (w3, xffff32)
		     val f = frag <> zero32 andalso frag <> dont_fragment32
	         in SOME {source = w4, dest = w5, protocol = p, fragment = f}
		 end
		else
		 slow_identify (array, cursor)
	     end
	  | _ => slow_identify (array, cursor))
	handle _ => slow_identify (array, cursor)
  end

(*
	15.	function identify_pointer

	This is needed by ICMP as a quick way to identify the connection
	to which a packet should be directed.
*)

  datatype pointer_position = Header | Option of Option.ip_option option | Data

  fun identify_pointer (header as (V4 {options, ...}), pointer) =
       if pointer < min_size then Header
       else if options <> [] andalso pointer < size header then
	Option (Option.option_position (options, pointer - min_size))
       else Data

(*
	16.	function lengths

	This is needed by IP as a way to determine header and data
	lengths even for packets which don't checksum (e.g. headers
	returned with ICMP messages).
*)

  fun lengths (array, cursor) =
       (let val (verhlen, skip) = Marshal_Word8.unmarshal (array, cursor)
	    val (_, dlen_cursor) = Marshal_Word8.unmarshal (array, skip)
	    val (data_length, _) = Marshal_Word16.unmarshal (array,
							     dlen_cursor)
	    val hlen = Word.fromInt
	                 (Word8.toInt (Word8.<< (Word8.andb (verhlen,
							     hlen_mask), 0w2)))
        in SOME {header = hlen,
		 data = Word.fromInt (Word16.toInt data_length) - hlen}
        end)
	 handle x => NONE

 end (* struct *)
