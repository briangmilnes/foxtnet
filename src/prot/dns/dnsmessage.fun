(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Message definition and conversion for DNS

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DNS_EXTERN
	2.	functor Dns_Message
	3.	exported types and exceptions
	4.	some constants
	5.	extern related declarations
	6.	structure Domain_Name
	7.	datatype rr_type_data - rr type and corresponding rdata
	8.	datatype rr_qtype
	9.	datatype rr_class
	10.	datatype rr (resource record)
	11.	datatype question
	12.	datatype header
	13.	datatype message
	14.	fun marshal_string_length
	15.	fun marshal_domain_name
	16.	fun marshal_header
	17.	fun marshal_questions
	18.	fun marshal_rrs
	19.	fun marshal
	20.	fun unmarshal_domain_name
	21.	fun unmarshal_header
	22.	fun unmarshal_questions
	23.	fun unmarshal_rrs
	24.	fun unmarshal
	25.	makestring utilities
	26.	fun makestring_rrs
	27.	fun makestring_questions
	28.	fun makestring_header
	29.	fun makestring

		iii.	RCS Log
	
$Log: dnsmessage.fun,v $
Revision 1.9  1997/04/22  11:30:40  esb
added a size function to the Domain_Name structure.

Revision 1.8  97/03/10  18:52:05  esb
adapted to new vendor.sig

Revision 1.7  96/04/18  21:30:41  cline
converted hash from int to word

Revision 1.6  1996/03/04  21:48:08  esb
minor fix.

Revision 1.5  1996/03/04  21:28:32  esb
added the "invert" function to Domain_Name, for inverse queries.

Revision 1.4  1996/02/16  16:35:15  cline
added ttl to makestring.

Revision 1.3  1996/02/06  23:40:56  esb
minor change.

Revision 1.2  1996/01/19  23:05:23  esb
adapted to the new wordarray signature.

Revision 1.1  1996/01/16  22:30:28  cline
Initial revision

		1.	signature DNS_EXTERN
*)

signature DNS_EXTERN =
 sig
   include DNS_MESSAGE

   type extern_in
   type extern_out

   exception Extern

   val marshal : message -> extern_out
   val unmarshal : extern_in -> message
   val makestring : message -> string
   val makestring_rr : rr -> string
   val equal_rr : rr * rr -> bool
 end

(*
		2.	functor Dns_Message
*)

functor Dns_Message(structure In: EXTERNAL
		    structure Out: EXTERNAL
		    structure B: FOX_BASIS
		    val debug_level: int ref option): DNS_EXTERN =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "dnsmessage.fun"
			   val makestring = fn _ => NONE)

(*
	3.	exported types and exceptions
*)

  type internet_address = Word32.word
  type time_value = Word32.word

  exception Extern

(*
	4.	some constants
*)
  val query_mask = 0x8000
  val opcode_shift = 0w11
  val aa_mask = 0x0400
  val tc_mask = 0x0200
  val rd_mask = 0x0100
  val ra_mask = 0x0080

(*
	5.	extern related declarations
*)

  type extern_in = In.T
  type extern_out = Out.T

  structure W8 =
    Protocol_Extern8 (structure In = In
		      structure Out = Out
		      structure B = B)

  structure W16 =
    Protocol_Extern16_Big (structure In = In
			   structure Out = Out
			   structure B = B)

  structure W32 =
    Protocol_Extern32_Big (structure In = In
			   structure Out = Out
			   structure B = B)
  structure Str =
    Extern_String_Length (structure In = In
			  structure Out = Out
			  structure V = B.V)

  fun w16_to_out w =
       let val out = Out.uninitialized 0w2 in W16.marshal (out, w) 0w0; out end

  fun w32_to_out w =
       let val out = Out.uninitialized 0w4 in W32.marshal (out, w) 0w0; out end

(*
	6.	structure Domain_Name
*)

  structure Domain_Name =
    struct
      type T = string list

      fun makestring [] = "."
	| makestring [label] = label
	| makestring (label::rest) = label ^ "." ^ makestring rest

      fun hash_aux ([], h) = h
	| hash_aux (c::rest, h) =
	let
	  val highbit = Word32.<< (0wx8000, 0w16)
	  fun op++ arg = case Word32.+ arg of
	    x => if Word32.< (x,highbit) then x else
	      Word32.+ (Word32.- (x, highbit), Word32.fromInt 1)
	  infix ++
	  val h32 = Word.toLargeWord h
	  val c32 = Word32.fromInt (B.V.Char.ord (B.V.Char.to_lower c))
	in
	  Word.fromLargeWord (h32 ++ h32 ++ c32)
	end

      fun hash [] = 0w0
	| hash (label::rest) = hash_aux (B.V.String.explode label, hash rest)

      fun equal ([], []) = true
	| equal (h0::t0, h1::t1) = (B.V.String.caseless_equal (h0, h1)
				    andalso equal (t0, t1))
	| equal _ = false

      fun parse "" = []
	| parse "." = []
	| parse s =
	case B.V.String.index (#".", s, 0) of
	  SOME i => (B.V.String.substring (s, 0, i)) ::
	    (parse
	     (B.V.String.substring (s, i+1, B.V.String.length s-i-1)))
	| NONE => [s]

      fun invert list = (rev list) @ ["IN-ADDR", "ARPA"]

      fun size list = foldr Int.+ 0 (map String.size list)
    end

(*
	7.	datatype rr_type_data - rr type and corresponding rdata
*)

  datatype rr_type_data =
      A               (* a host address *)
        of internet_address
    | NS              (* an authoritative name server *)
        of Domain_Name.T
    | MD              (* a mail destination (Obsolete - use MX) *)
        of Domain_Name.T
    | MF              (* a mail forwarder (Obsolete - use MX) *)
        of Domain_Name.T
    | CNAME           (* the canonical name for an alias *)
        of Domain_Name.T
    | SOA             (* marks the start of a zone of authority *)
      (* This is currently not implemented, but should contain the
	 following information:
        of {mname: Domain_Name.T,
  	  rname: Domain_Name.T,
  	  serial: Word32.word,
  	  refresh: time_value,
  	  retry: time_value,
  	  expire: time_value,
  	  minimum: Word32.word}
       *)
    | MB              (* a mailbox domain name (EXPERIMENTAL) *)
        of Domain_Name.T
    | MG              (* a mail group member (EXPERIMENTAL) *)
        of Domain_Name.T
    | MR              (* a mail rename domain name (EXPERIMENTAL) *)
        of Domain_Name.T
    | NULL            (* a null RR (EXPERIMENTAL) *)
    | WKS             (* a well known service description *)
    (* This is currently not implemented, but should contain the
       following information:
        of  {address: internet_address,
	     protocol: Word8.word,
	     services: int list}
    *)
    | PTR             (* a domain name pointer *)
        of Domain_Name.T
    | HINFO           (* host information *)
        of {cpu:string, os:string}
    | MINFO           (* mailbox or mail list information *)
    (* This is currently not implemented, but should contain the
       following information:
        of {rmailbx:Domain_Name.T, emailbx:Domain_Name.T}
    *)
    | MX              (* mail exchange *)
        of {preference:Word16.word, exchange:Domain_Name.T}
    | TXT             (* text strings *)
        of string list

  fun rr_type_equal (A addr0, A addr1) = addr0 = addr1
    | rr_type_equal (NS n0, NS n1) = Domain_Name.equal(n0, n1)
    | rr_type_equal (MD n0, MD n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (MF n0, MF n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (CNAME n0, CNAME n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (SOA, SOA) = true
    | rr_type_equal (MB n0, MB n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (MG n0, MG n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (MR n0, MR n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (NULL, NULL) = true
    | rr_type_equal (WKS, WKS) = true
    | rr_type_equal (PTR n0, PTR n1) = Domain_Name.equal (n0, n1)
    | rr_type_equal (HINFO i0, HINFO i1) = i0=i1
    | rr_type_equal (MINFO, MINFO) = true
    | rr_type_equal (MX {preference=p0, exchange=x0},
		     MX {preference=p1, exchange=x1}) =
                      (p0=p1 andalso Domain_Name.equal (x0, x1))
    | rr_type_equal (TXT l0, TXT l1) = l0=l1
    | rr_type_equal _ = false
(*
	8.	datatype rr_qtype
*)
  datatype rr_qtype =
      A_Q             (* a host address *)
    | NS_Q            (* an authoritative name server *)
    | MD_Q            (* a mail destination (Obsolete - use MX) *)
    | MF_Q            (* a mail forwarder (Obsolete - use MX) *)
    | CNAME_Q         (* the canonical name for an alias *)
    | SOA_Q           (* marks the start of a zone of authority *)
    | MB_Q            (* a mailbox domain name (EXPERIMENTAL) *)
    | MG_Q            (* a mail group member (EXPERIMENTAL) *)
    | MR_Q            (* a mail rename domain name (EXPERIMENTAL) *)
    | NULL_Q          (* a null RR (EXPERIMENTAL) *)
    | WKS_Q           (* a well known service description *)
    | PTR_Q           (* a domain name pointer *)
    | HINFO_Q         (* host information *)
    | MINFO_Q         (* mailbox or mail list information *)
    | MX_Q            (* mail exchange *)
    | TXT_Q           (* text strings *)
    | AXFR_Q          (* request for a transfer of an entire zone *)
    | MAILB_Q         (* request for mailbox-related records (MB, MG or MR) *)
    | MAILA_Q         (* request for mail agent RRs (Obsolete - see MX) *)
    | All_Records_Q   (* request for all records *)

  fun qtype_int A_Q = 1
    | qtype_int NS_Q = 2
    | qtype_int MD_Q = 3
    | qtype_int MF_Q = 4
    | qtype_int CNAME_Q = 5
    | qtype_int SOA_Q = 6
    | qtype_int MB_Q = 7
    | qtype_int MG_Q = 8
    | qtype_int MR_Q = 9
    | qtype_int NULL_Q = 10
    | qtype_int WKS_Q = 11
    | qtype_int PTR_Q = 12
    | qtype_int HINFO_Q = 13
    | qtype_int MINFO_Q = 14
    | qtype_int MX_Q = 15
    | qtype_int TXT_Q = 16
    | qtype_int AXFR_Q = 252
    | qtype_int MAILB_Q = 253
    | qtype_int MAILA_Q = 254
    | qtype_int All_Records_Q = 255

  fun int_qtype 1 = A_Q
    | int_qtype 2 = NS_Q
    | int_qtype 3 = MD_Q
    | int_qtype 4 = MF_Q
    | int_qtype 5 = CNAME_Q
    | int_qtype 6 = SOA_Q
    | int_qtype 7 = MB_Q
    | int_qtype 8 = MG_Q
    | int_qtype 9 = MR_Q
    | int_qtype 10 = NULL_Q
    | int_qtype 11 = WKS_Q
    | int_qtype 12 = PTR_Q
    | int_qtype 13 = HINFO_Q
    | int_qtype 14 = MINFO_Q
    | int_qtype 15 = MX_Q
    | int_qtype 16 = TXT_Q
    | int_qtype 252 = AXFR_Q
    | int_qtype 253 = MAILB_Q
    | int_qtype 254 = MAILA_Q
    | int_qtype 255 = All_Records_Q
    | int_qtype _ = raise Extern

(*
	9.	datatype rr_class
*)
  datatype rr_class =
      IN              (* the Internet *)
    | CS              (* the CSNET class (Obsolete - used only for examples in
                         some obsolete RFCs) *)
    | CH              (* the CHAOS class *)
    | HS              (* Hesiod [Dyer 87] *)
    | Any_Class       (* Query for any class *)

  fun class_int IN = 1
    | class_int CS = 2
    | class_int CH = 3
    | class_int HS = 4
    | class_int Any_Class = 255

  fun int_class 1 = IN
    | int_class 2 = CS
    | int_class 3 = CH
    | int_class 4 = HS
    | int_class 255 = Any_Class
    | int_class _ = raise Extern

(*
	10.	datatype rr (resource record)
*)
  datatype rr = RR of {name: Domain_Name.T,
		       rr_type: rr_type_data,
		       rr_class: rr_class,
		       ttl: Word32.word}

  fun equal_rr (RR {name=name0, rr_type=rr_type0, rr_class = rr_class0, ...},
		RR {name=name1, rr_type=rr_type1, rr_class = rr_class1, ...}) =
        (Domain_Name.equal (name0, name1)
	 andalso rr_class0 = rr_class1
	 andalso rr_type_equal (rr_type0, rr_type1))

(*
	11.	datatype question
*)

  datatype question = Question of {name: Domain_Name.T,
				   rr_qtype: rr_qtype,
				   rr_class: rr_class}

(*
	12.	datatype header
*)

  datatype opcode = Query | Inverse_Query | Status

  fun opcode_int Query = 0
    | opcode_int Inverse_Query = 1
    | opcode_int Status = 2

  fun int_opcode 0 = Query
    | int_opcode 1 = Inverse_Query
    | int_opcode 2 = Status
    | int_opcode _ = raise Extern

  datatype rcode = No_Error | Format_Error | Server_Failure | Name_Error
    | Not_Implemented | Refused

  fun rcode_int No_Error = 0
    | rcode_int Format_Error = 1
    | rcode_int Server_Failure = 2
    | rcode_int Name_Error = 3
    | rcode_int Not_Implemented = 4
    | rcode_int Refused = 5

  fun int_rcode 0 = No_Error
    | int_rcode 1 = Format_Error
    | int_rcode 2 = Server_Failure
    | int_rcode 3 = Name_Error
    | int_rcode 4 = Not_Implemented
    | int_rcode 5 = Refused
    | int_rcode _ = raise Extern

  datatype header = Header of {query: bool,
			       opcode: opcode,
			       aa: bool,	(* authoritative answer *)
			       tc: bool,	(* truncated *)
			       rd: bool,	(* recursion desired *)
			       ra: bool,	(* recursion available *)
			       rcode: rcode}

(*
	13.	datatype message
*)

  datatype message = Message of {header: header,
				 question: question list,
				 answer: rr list,
				 authority: rr list,
				 additional: rr list}
                   | Parse_Error of string

(*
	14.	fun marshal_string_length
*)

  fun marshal_string_length s =
       let val size_s = Word.fromInt (String.size s)
           val out = Out.uninitialized (size_s + 0w1)
       in (Str.marshal (out, s)
	   (W8.marshal (out, Word8.fromInt (Word.toInt size_s)) 0w0));
	  out
       end

  fun marshal_string_lengths [] = Out.uninitialized 0w0
    | marshal_string_lengths (hd::tl) =
        Out.join (marshal_string_length hd, marshal_string_lengths tl)

(*
	15.	fun marshal_domain_name

*)

  local
    fun check_length s =
      if Out.size s <= 0w255 then s	(* RFC 1035 p. 10 (sec. 2.3.4) *)
      else Trace.print_raise (Extern, SOME "marshal_domain_name(check_length)")
    fun abbreviate _ = NONE
  in
    fun marshal_domain_name ([], _) = marshal_string_length ""
      | marshal_domain_name (name as (hd::tl), x) =
      case abbreviate (name, x) of
	SOME w => w16_to_out w
      | NONE => 
	  if String.size hd <= 63 then	(* RFC 1035 p. 10 (sec. 2.3.4) *)
	    check_length (Out.join (marshal_string_length hd,
				    marshal_domain_name (tl, x)))
	  else Trace.print_raise (Extern, SOME "marshal_domain_name")
  end (* local *)

(*
	16.	fun marshal_header
*)
  fun marshal_header 
        (Message {header = Header {query, opcode, aa, tc, rd, ra, rcode},
		  question, answer, authority, additional}) =
    let
      val id = Word16.fromInt 0  (* let dns protocol fill this in *)
      val flags = Word16.fromInt
	            ((if query then 0 else query_mask) +
		     (Bits.<< (opcode_int opcode, opcode_shift)) +
		     (if aa then aa_mask else 0) +
		     (if tc then tc_mask else 0) +
		     (if rd then rd_mask else 0) +
		     (if ra then ra_mask else 0) +
		     (rcode_int rcode))
      val qdcount = Word16.fromInt (length question)
      val ancount = Word16.fromInt (length answer)
      val nscount = Word16.fromInt (length authority)
      val arcount = Word16.fromInt (length additional)

      val header = Out.uninitialized 0w12
      val cursor = 0w0 (* initial cursor *)
      val cursor = W16.marshal (header, id) cursor
      val cursor = W16.marshal (header, flags) cursor
      val cursor = W16.marshal (header, qdcount) cursor
      val cursor = W16.marshal (header, ancount) cursor
      val cursor = W16.marshal (header, nscount) cursor
      val cursor = W16.marshal (header, arcount) cursor
    in
      header
    end
    | marshal_header (Parse_Error _) =
        Trace.print_raise (Extern, SOME "marshal_header")

(*
	17.	fun marshal_questions
*)
  fun marshal_question (Question {name, rr_qtype, rr_class}) msg =
    let
      val upto_name = Out.join (msg, marshal_domain_name (name, msg))
      val type_class = Out.uninitialized 0w4
    in
      (W16.marshal (type_class, Word16.fromInt (class_int rr_class))
       (W16.marshal (type_class, Word16.fromInt (qtype_int rr_qtype))
	0w0));
      Out.join (upto_name, type_class)
    end

  fun marshal_questions [] = (fn msg => msg)
    | marshal_questions (q::rest) =
        (marshal_questions rest) o (marshal_question q)

(*
	18.	fun marshal_rrs
*)
  fun marshal_type (A addr, _) = (1, w32_to_out addr)
    | marshal_type (NS name, msg) = (2, marshal_domain_name (name, msg))
    | marshal_type (MD name, msg) = (3, marshal_domain_name (name, msg))
    | marshal_type (MF name, msg) = (4, marshal_domain_name (name, msg))
    | marshal_type (CNAME name, msg) = (5, marshal_domain_name (name, msg))
    | marshal_type (SOA, _) = Trace.print_raise (Extern, SOME "SOA")
    | marshal_type (MB name, msg) = (7, marshal_domain_name (name, msg))
    | marshal_type (MG name, msg) = (8, marshal_domain_name (name, msg))
    | marshal_type (MR name, msg) = (9, marshal_domain_name (name, msg))
    | marshal_type (NULL, _) = Trace.print_raise (Extern, SOME "NULL")
    | marshal_type (WKS, _) = Trace.print_raise (Extern, SOME "WKS")
    | marshal_type (PTR name, msg) = (12, marshal_domain_name (name, msg))
    | marshal_type (HINFO {cpu, os}, msg) =
        (13, Out.join (marshal_string_length cpu, marshal_string_length os))
    | marshal_type (MINFO, _) = Trace.print_raise (Extern, SOME "MINFO")
    | marshal_type (MX {preference, exchange}, msg) =
	(15, Out.join (w16_to_out preference,
		       marshal_domain_name (exchange, msg)))
    | marshal_type (TXT strings, _) = (16, marshal_string_lengths strings)

  fun marshal_rr (RR {name, rr_type, rr_class, ttl}) msg =
    let
      val upto_name = Out.join (msg, marshal_domain_name (name, msg))
      val type_cursor = Out.size upto_name
      val upto_rdlength = Out.join (upto_name, Out.uninitialized 0w10)
      val (rr_type_int, rdata) = marshal_type (rr_type, upto_rdlength)
      val rdlength_cursor =
	    (W16.marshal (upto_rdlength,
			  Word16.fromInt (Word.toInt (Out.size rdata)))
	     (W32.marshal (upto_rdlength, ttl)
	      (W16.marshal (upto_rdlength,
			    Word16.fromInt (class_int rr_class))
	       (W16.marshal (upto_rdlength, Word16.fromInt (rr_type_int))
		0w0))))
    in Out.join (upto_rdlength, rdata)
    end

  fun marshal_rrs [] = (fn x => x)
    | marshal_rrs (rr::rest) = (marshal_rrs rest) o (marshal_rr rr)

(*
	19.	fun marshal
*)
  fun marshal (message
	       as Message {question, answer, authority, additional, ...}) =
    let
      val msg = (marshal_rrs (answer @ authority @ additional)
		 (marshal_questions question
		  (marshal_header message)))
    in
      msg
    end
    | marshal (Parse_Error _) = Trace.print_raise (Extern, SOME "marshal")


(*
	20.	fun unmarshal_domain_name
*)

  fun unmarshal_domain_name (msg, cursor) =
       let val (len, string_cursor) = W8.unmarshal (msg, cursor)
       in if len = (0w0:Word8.word) then (* null label *)
	   ([], string_cursor)
          else if Word8.>= (len, 0wxC0) then (* pointer *)
	   let val (ptr, final_cursor) = W16.unmarshal (msg, cursor)
	       val (result, _) = 
	              unmarshal_domain_name (msg, Word.fromInt
					     (Word16.toInt ptr - 0xC000))
	   in (result, final_cursor)
	   end
	  else if Word8.> (len, 0wx40) then raise Extern (* error *)
	  else (* normal label *)
	   let val (label, next_cursor) =
	             Str.unmarshal ((msg, Word.fromInt (Word8.toInt len)), 
				    string_cursor)
	       val (rest, final_cursor) = unmarshal_domain_name (msg,
								 next_cursor)
	   in (label::rest, final_cursor)
	   end
       end

(*
	21.	fun unmarshal_header
*)

  fun unmarshal_header (msg, cursor) =
    let
      val size = In.size msg
      fun maybe_unmarshal cursor =
	if size - cursor >= 0w2 then W16.unmarshal (msg, cursor)
	else (Word16.fromInt 0, cursor)
      val (_(*id*), cursor) = W16.unmarshal (msg, cursor)
      val (flags,   cursor) = W16.unmarshal (msg, cursor)
      val (qdcount, cursor) = maybe_unmarshal cursor
      val (ancount, cursor) = maybe_unmarshal cursor
      val (nscount, cursor) = maybe_unmarshal cursor
      val (arcount, cursor) = maybe_unmarshal cursor

      val int_flags = Word16.toInt flags
      fun flag mask = Bits.andb(int_flags, mask) = mask
      val opcode = int_opcode
	             (Bits.andb
		       (Bits.>> (int_flags, opcode_shift), 15))
      val rcode = int_rcode (Bits.andb (int_flags, 15))
      val header =
	  Header {query = not (flag query_mask),
		  opcode = opcode,
		  aa = flag aa_mask,
		  tc = flag tc_mask,
		  rd = flag rd_mask,
		  ra = flag ra_mask,
		  rcode = rcode}
    in
      {header = header,
       qdcount = Word.fromInt (Word16.toInt qdcount),
       ancount = Word.fromInt (Word16.toInt ancount),
       nscount = Word.fromInt (Word16.toInt nscount),
       arcount = Word.fromInt (Word16.toInt arcount),
       qdcursor = cursor}
    end

(*
	22.	fun unmarshal_questions
*)
  fun unmarshal_question (msg, cursor) =
    let
      val (name, qtype_cursor) = unmarshal_domain_name (msg, cursor)
      val (rr_qtype, class_cursor) = W16.unmarshal (msg, qtype_cursor)
      val (rr_class, final_cursor) = W16.unmarshal (msg, class_cursor)
    in
      (Question {name = name,
		 rr_qtype = int_qtype (Word16.toInt rr_qtype),
		 rr_class = int_class (Word16.toInt rr_class)},
       final_cursor)
    end

  fun unmarshal_questions (_, cursor, 0w0) = ([], cursor)
    | unmarshal_questions (msg, cursor, n) =
       let val (qfirst, cursor) = unmarshal_question (msg, cursor)
           val (qrest, cursor) = unmarshal_questions (msg, cursor, n - 0w1)
       in (qfirst :: qrest, cursor)
       end

(*
	23.	fun unmarshal_rrs
*)
  fun unmarshal_rr (msg, cursor) =
    let
      val (name, cursor) = unmarshal_domain_name (msg, cursor)
      val (rr_type_word, cursor) = W16.unmarshal (msg, cursor)
      val (rr_class, cursor) = W16.unmarshal (msg, cursor)
      val (ttl, cursor) = W32.unmarshal (msg, cursor)
      val (rdlength, rdata_cursor) = W16.unmarshal (msg, cursor)
      val rdlen = Word.fromInt (Word16.toInt rdlength)

      fun assert v f = if f v then v else raise Extern

      fun check_read_domain_name () =
	let
	  val (name, cursor) = unmarshal_domain_name (msg, rdata_cursor)
	in
	  assert name (fn _ => cursor = (rdata_cursor + rdlen))
	  before Trace.debug_print (fn () => "  check_read_domain_name!")
	end

      fun read_w32 cursor = #1 (W32.unmarshal (msg, cursor))

      fun read_hinfo_rdata () =
	let
	  val (cpu_size, cpu_cursor) = W8.unmarshal (msg, rdata_cursor)
	  val (cpu, os_size_cursor) =
	        Str.unmarshal ((msg, Word.fromInt (Word8.toInt cpu_size)),
			       cpu_cursor)
	  val (os_size, os_cursor) = W8.unmarshal (msg, os_size_cursor)
	  val (os, final_cursor) =
	        Str.unmarshal ((msg, Word.fromInt (Word8.toInt os_size)),
			       os_cursor)
	in
	  assert {cpu=cpu, os=os}
	         (fn _ => final_cursor = (rdata_cursor + rdlen))
	end

      fun read_mx_rdata () =
	let
	  val (preference, exchange_cursor) = W16.unmarshal (msg, rdata_cursor)
	  val (exchange, final_cursor) = unmarshal_domain_name
					   (msg, exchange_cursor)
	in
	  assert {preference=preference, exchange=exchange}
	         (fn _ => cursor = (rdata_cursor + rdlen))
	end

      fun read_txt_rdata cursor =
	if cursor < rdata_cursor + rdlen then
	  let
	    val (len, string_cursor) = W8.unmarshal (msg, cursor)
	    val (s, next_cursor) =
		  Str.unmarshal ((msg, Word.fromInt (Word8.toInt len)),
				 string_cursor)
	  in
	    s :: read_txt_rdata next_cursor
	  end
	else
	  assert [] (fn _ => cursor = rdata_cursor + rdlen)

      val rr_type =
	case Word16.toInt rr_type_word of
	  1 => A (read_w32 rdata_cursor)
	| 2 => NS (check_read_domain_name ())
	| 3 => MD (check_read_domain_name ())
	| 4 => MF (check_read_domain_name ())
	| 5 => CNAME (check_read_domain_name ())
	| 6 => SOA
	| 7 => MB (check_read_domain_name ())
	| 8 => MG (check_read_domain_name ())
	| 9 => MR (check_read_domain_name ())
	| 10 => NULL (* ignore data *)
	| 11 => WKS
	| 12 => PTR (check_read_domain_name ())
	| 13 => HINFO (read_hinfo_rdata ())
	| 14 => MINFO
	| 15 => MX (read_mx_rdata ())
	| 16 => TXT (read_txt_rdata rdata_cursor)
	| _ => raise Extern
    in
      (RR {name = name,
	   rr_type = rr_type,
	   rr_class = int_class (Word16.toInt rr_class),
	   ttl = ttl},
       rdata_cursor + rdlen)
    end

  fun unmarshal_rrs (_, cursor, 0w0) = ([], cursor)
    | unmarshal_rrs (msg, cursor, n) =
    let
      val (rrfirst, cursor) = unmarshal_rr (msg, cursor)
      val (rrrest, cursor) = unmarshal_rrs (msg, cursor, n - 0w1)
    in
      (rrfirst::rrrest, cursor)
    end

(*
	24.	fun unmarshal
*)
  fun unmarshal msg =
    let
      val hdcursor = 0w0
      val {header, qdcount, ancount, nscount, arcount, qdcursor}
			        = unmarshal_header (msg, hdcursor)
      val (question,  ancursor) = unmarshal_questions (msg, qdcursor, qdcount)
      val (answer,    nscursor) = unmarshal_rrs (msg, ancursor, ancount)
      val (authority, arcursor) = unmarshal_rrs (msg, nscursor, nscount)
      val (additional,       _) = unmarshal_rrs (msg, arcursor, arcount)
    in
      Message {header = header,
	       question = question,
	       answer = answer,
	       authority = authority,
	       additional = additional}
    end
  handle x => (case x of Extern => ()
                       | _ => Trace.print_handled (x, SOME "unmarshal");
	       Parse_Error (In.makestring msg))

(*
	25.	makestring utilities
*)
  fun makestring_ip ip =
    let
      open Word32
      fun s pos = Integer.toString
	            (Word32.toInt (andb (>> (ip, pos), fromInt 0xFF)))
    in
      (s 0w24) ^ "." ^ (s 0w16) ^ "." ^ (s 0w8) ^ "." ^ (s 0w0)
    end

  fun makestring_domain_name [] = "."
    | makestring_domain_name [name] = name
    | makestring_domain_name (first::rest) =
        first ^ "." ^ makestring_domain_name rest

  fun makestring_class IN = "Internet"
    | makestring_class CS = "CSNET (obsolete)"
    | makestring_class CH = "CHAOS"
    | makestring_class HS = "Hesiod"
    | makestring_class Any_Class = "*"
(*
	26.	fun makestring_rrs
*)
  fun makestring_type (A addr) = ("Host Address: " ^ makestring_ip addr)
    | makestring_type (NS name) = ("Name Server: " ^
				   makestring_domain_name name)
    | makestring_type (MD name) = ("MD: " ^ makestring_domain_name name)
    | makestring_type (MF name) = ("MF: " ^ makestring_domain_name name)
    | makestring_type (CNAME name) = ("Canonical Name is " ^
				      makestring_domain_name name)
    | makestring_type (SOA) = "Start Zone of Authority ..."
    | makestring_type (MB name) = ("MB: " ^ makestring_domain_name name)
    | makestring_type (MG name) = ("MG: " ^ makestring_domain_name name)
    | makestring_type (MR name) = ("MR: " ^ makestring_domain_name name)
    | makestring_type (NULL) = "NULL"
    | makestring_type (WKS) = "Well Known Services ..."
    | makestring_type (PTR name) = ("Pointer: " ^ makestring_domain_name name)
    | makestring_type (HINFO {cpu, os}) = "Host Info: " ^ cpu ^ "/" ^ os
    | makestring_type (MINFO) = "MINFO ..."
    | makestring_type (MX {preference, exchange}) =
        "MX: " ^ makestring_domain_name exchange ^
	" (preference " ^ Integer.toString (Word16.toInt preference) ^ ")"
    | makestring_type (TXT _) = "TXT ..."

  fun makestring_rr (RR {name, rr_type, rr_class, ttl}) =
        " RR \"" ^ makestring_domain_name name ^
	"\" (class=" ^ makestring_class rr_class ^
	") (ttl = " ^ Integer.toString (Word32.toInt ttl) ^
	") " ^ makestring_type rr_type

  fun makestring_rrs prefix [] = ""
    | makestring_rrs prefix (rr::rest) =
        concat [prefix, makestring_rr rr, makestring_rrs prefix rest]
(*
	27.	fun makestring_questions
*)
  fun makestring_qtype A_Q = "A"
    | makestring_qtype NS_Q = "NS"
    | makestring_qtype MD_Q = "MD"
    | makestring_qtype MF_Q = "MF"
    | makestring_qtype CNAME_Q = "CNAME"
    | makestring_qtype SOA_Q = "SOA"
    | makestring_qtype MB_Q = "MB"
    | makestring_qtype MG_Q = "MG"
    | makestring_qtype MR_Q = "MR"
    | makestring_qtype NULL_Q = "NULL"
    | makestring_qtype WKS_Q = "WKS"
    | makestring_qtype PTR_Q = "PTR"
    | makestring_qtype HINFO_Q = "HINFO"
    | makestring_qtype MINFO_Q = "MINFO"
    | makestring_qtype MX_Q = "MX"
    | makestring_qtype TXT_Q = "TXT"
    | makestring_qtype AXFR_Q = "AXFR"
    | makestring_qtype MAILB_Q = "MAILB"
    | makestring_qtype MAILA_Q = "MAILA"
    | makestring_qtype All_Records_Q =  "All_Records"

  fun makestring_question (Question {name, rr_qtype, rr_class}) =
    "Question: \"" ^ makestring_domain_name name ^
    "\" type=" ^ makestring_qtype rr_qtype ^
    " class=" ^ makestring_class rr_class

  fun makestring_questions [] = ""
    | makestring_questions (q::rest) =
        concat ["\n   ", makestring_question q, makestring_questions rest]


(*
	28.	fun makestring_header
*)
  fun makestring_header (Header {query, opcode, aa, tc, rd, ra, rcode}) =
    concat
    ["DNS ", (if query then "query "
	      else (if aa then "authoritative " else "") ^ "response "),
     "[opcode: ", (case opcode of
		     Query => "Query"
		   | Inverse_Query => "Inverse Query"
		   | Status => "Status") ^ "] ",
     "[flags: ", if tc then "TC " else "",
                 if rd then "RD " else "",
                 if ra then "RA " else "", "]",
     case rcode of
       No_Error        => ""
     | Format_Error    => " Format Error"
     | Server_Failure  => " Server Failure"
     | Name_Error      => " Name Error"
     | Not_Implemented => " Not Implemented"
     | Refused         => " Refused"]

(*
	29.	fun makestring
*)
  fun makestring (Message {header = header,
			   question = question,
			   answer = answer,
			   authority = authority,
			   additional = additional}) =
        makestring_header header ^
	makestring_questions question ^
	makestring_rrs "\n   Answer" answer ^
	makestring_rrs "\n   Authority" authority ^
	makestring_rrs "\n   Additional" additional
    | makestring (Parse_Error s) =
	"Error parsing DNS message.  Raw data follows:\n" ^ s

 end

