(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139 - 3891

		i.	Abstract

	The IP protocol allows clients to specify options with the
	set_option control operation, and to receive options with the
	option_handler control operation. The options are lists of the
	ip_option data structure below. The input side may raise an
	exception if an input option is not meaningful.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ip_Options
	2.	functions makestring_option and makestring_options
	3.	parse_options
	4.	receive_time_stamp_option
	5.	function size
	6.	emit_options
	7.	functions check_option and check


		iii.	RCS Log
	
$Log: ipoptions.fun,v $
Revision 1.12  1995/03/24  16:00:28  esb
replaced signExtend with wordToInt.

Revision 1.11  1995/03/24  01:43:18  esb
adapted to new ipoptions.sig

Revision 1.10  1995/03/10  03:46:34  esb
adapted to new vendor.sig.

Revision 1.9  1995/03/07  20:35:44  esb
updated tracing.

Revision 1.8  1995/02/04  20:40:05  robby
updated to 107

Revision 1.7  1994/11/07  21:32:41  cline
use V.Print

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.4  1994/06/23  14:34:45  danwang
Changed Byte#.ubytes to ubyte#.

Revision 1.3  1994/05/23  14:18:38  milnes
Installed Edo's naming changes in the old revision, plus changed
options to take the byte array of the message header, instead of
the packet.

Revision 1.1  1994/05/03  21:09:19  esb
Initial revision


	1.	functor Ip_Options
*)

functor Ip_Option (structure B: FOX_BASIS
		   type ip_number
		   val ip_size: int
		   val ip_equal: ip_number * ip_number -> bool
		   val makestring_ip: ip_number -> string
		   val bytes_to_ip: ByteArray.bytearray -> ip_number
		   val ip_to_bytes:
		        ip_number -> ByteArray.bytearray): IP_OPTION_INTERNAL =
 struct

  fun local_print s = B.V.Print.print ("ipoption.fun: " ^ s ^ "\n")

  type ip_number = ip_number

  datatype time_stamp = Stamp of FoxWord32.word list
                      | Record_Stamp of {ip: ip_number,
					 time: FoxWord32.word} list
		      | Given_Stamp of {ip: ip_number,
					time: FoxWord32.word option} list

  datatype T = 
     Loose_Route of {length: FoxWord8.word, position: FoxWord8.word,
		     route: ip_number list}
   | Strict_Route of {length: FoxWord8.word, position: FoxWord8.word,
		      route: ip_number list}
   | Record_Route of {length: FoxWord8.word, position: FoxWord8.word,
		      route: ip_number list}
   | Time_Stamp of {length: FoxWord8.word, position: FoxWord8.word,
		    overflow: FoxWord8.word, stamps: time_stamp}

  exception Illegal_Option of string

(*
	2.	functions makestring_option and makestring_options
*)

  local
   fun makestring_space make value = make value ^ " "

   val ip_space = makestring_space makestring_ip

   val b4_space = makestring_space FoxMakestring.word32

   fun makestring_ip_time {ip, time} =
        makestring_ip ip ^ "@" ^ FoxMakestring.word32 time

   fun makestring_ip_maybe {ip, time = NONE} = makestring_ip ip
     | makestring_ip_maybe {ip, time = SOME value} =
        makestring_ip ip ^ "@" ^ FoxMakestring.word32 value

   val ip_time_space = makestring_space makestring_ip_time
   val ip_maybe_space = makestring_space makestring_ip_maybe


   fun makestring_ip_list route = B.V.String.concat (map ip_space route)

   fun makestring_time_stamp (Stamp stamps) =
        B.V.String.concat (map b4_space stamps)
     | makestring_time_stamp (Record_Stamp stamps) =
        "Record " ^ B.V.String.concat (map ip_time_space stamps)
     | makestring_time_stamp (Given_Stamp stamps) =
        "Given " ^ B.V.String.concat (map ip_maybe_space stamps)

  in
   fun makestring_option (Loose_Route {length, position, route}) =
        "Loose_Route (" ^ FoxMakestring.word8 length ^ ", " ^
	FoxMakestring.word8 position ^ ", " ^ makestring_ip_list route ^ ")"
     | makestring_option (Strict_Route {length, position, route}) =
        "Strict_Route (" ^ FoxMakestring.word8 length ^ ", " ^
	FoxMakestring.word8 position ^ ", " ^ makestring_ip_list route ^ ")"
     | makestring_option (Record_Route {length, position, route}) =
        "Record_Route (" ^ FoxMakestring.word8 length ^ ", " ^
	FoxMakestring.word8 position ^ ", " ^ makestring_ip_list route ^ ")"
     | makestring_option (Time_Stamp {length, position, overflow, stamps}) =
        "Time_Stamp (" ^ FoxMakestring.word8 length ^ ", " ^
	FoxMakestring.word8 position ^ ", " ^ FoxMakestring.word8 overflow ^ ", " ^
	makestring_time_stamp stamps ^ ")"

   fun make_options_string_list [] = ["]"]
     | make_options_string_list (opt :: r) =
        makestring_option opt :: (" " :: (make_options_string_list r))

   fun makestring_options opt =
     B.V.String.concat ("[" :: (make_options_string_list opt))

  end (* local *)

(*
	3.	parse_options
*)

  local
   fun byte1to4 (b0, b1, b2, b3) =
        let val b0 = FoxWord32.intToWord (FoxWord8.wordToInt b0)
	    val b1 = FoxWord32.intToWord (FoxWord8.wordToInt b1)
	    val b2 = FoxWord32.intToWord (FoxWord8.wordToInt b2)
	    val b3 = FoxWord32.intToWord (FoxWord8.wordToInt b3)
	in B.Order.B4.from_big
	       (FoxWord32.orb
		(FoxWord32.lshift (b3, 24),
		 FoxWord32.orb
		 (FoxWord32.lshift (b2, 16),
		  FoxWord32.orb (FoxWord32.lshift (b1, 8), b0))))
	end

   exception Incorrect_Options_Encountered

   fun parse_route (array, start, finish) =
	if start + ip_size >= finish then []
	else
	 bytes_to_ip (B.Create.copy_create (array, start, ip_size)) ::
	 parse_route (array, start + ip_size, finish)

   fun parse_stamps (array, start, finish) =
	if start + 4 >= finish then []
	else
	 FoxWord32.sub (array, start) ::
	 parse_stamps (array, start + 4, finish)

   fun parse_ip_time (array, start, finish) =
	if start + 4 + ip_size >= finish then []
	else
	 {ip = bytes_to_ip (B.Create.copy_create (array, start, ip_size)),
	  time = FoxWord32.sub (array, start + ip_size)} ::
	 parse_ip_time (array, start + 4 + ip_size, finish)

   fun parse_ip_maybe (array, start, finish) =
	if start + 4 + ip_size >= finish then []
	else
	 {ip = bytes_to_ip (B.Create.copy_create (array, start, ip_size)),
	  time = SOME (FoxWord32.sub (array, start + ip_size))} ::
	 parse_ip_maybe (array, start + 4 + ip_size, finish)

   fun parse_regular (constructor, array, start, finish) =
        let val length = FoxWord8.sub (array, start + 1)
	    val position = FoxWord8.sub (array, start + 2)
	    val op < = FoxWord8.<
	    val op > = FoxWord8.>
	in if length = (FoxWord8.intToWord 0) orelse
	      length > FoxWord8.intToWord (finish  -  start) orelse
	      position < (FoxWord8.intToWord 4) then
	    raise Incorrect_Options_Encountered
	   else
	    let val route =
	             if position > length then
		      parse_route (array, start + 3,
				   start + (FoxWord8.wordToInt length) - 1)
		     else
		      parse_route (array, start + 3,
				   start + (FoxWord8.wordToInt position) - 1)
	    in (constructor {length = length, position = position,
			     route = route},
		start + FoxWord8.wordToInt length)
	    end
	end

   fun parse_time_stamp (array, start, finish) =
        let val length = FoxWord8.sub (array, start + 1)
            val position = FoxWord8.sub (array, start + 2)
            val flags = FoxWord8.sub (array, start + 3)
	    val overflow = FoxWord8.rshiftl (flags, 4)
	    val opcode = FoxWord8.andb (flags, (FoxWord8.intToWord 0xf))
	in if length = (FoxWord8.intToWord 0) orelse
	      FoxWord8.wordToInt length > (finish  -  start) orelse
	      FoxWord8.wordToInt position < 4 then
	    raise Incorrect_Options_Encountered
	   else
	    (Time_Stamp {length = length, position = position,
			 overflow = overflow,
			 stamps = 
			   case FoxWord8.wordToInt opcode of
			      0 =>   (* stamps only *)
			       Stamp (parse_stamps (array, start + 4, finish))
			    | 1 =>	(* record stamp and host *)
			       Record_Stamp (parse_ip_time (array, start + 4,
							    finish))
			    | 3 =>	(* record stamp with prespecified ip *)
			       Given_Stamp (parse_ip_maybe (array, start + 4,
							    finish))
			    | _ =>	(* undefined. *)
			       raise Incorrect_Options_Encountered},
	     start + (FoxWord8.wordToInt length))
	end

   fun parse_list (array, start, finish) =
        if start > finish then []
	else
	 case FoxWord8.wordToInt (FoxWord8.sub (array, start)) of
	    0 (* end_of_options_list_byte *) =>
	     parse_list (array, start + 1, finish)
	  | 1 (* no_operation_byte *) =>
	     parse_list (array, start + 1, finish)
	  | 7 (* record_route_byte *) =>
	     let val (option, position) = parse_regular (Record_Route, array,
							 start, finish)
		 val rest = parse_list (array, position, finish)
	     in option :: rest
	     end
	  | 68  (* time_stamp_byte *) =>
	     let val (option, position) = parse_time_stamp (array, start,
							    finish)
	         val rest = parse_list (array, position, finish)
	     in option :: rest
	     end
	  | 130 (* obsolete security *) =>
	     parse_list (array, start + 11, finish)
	  | 131 (* loose_source_routing_byte *) =>
	     let val (option, position) = parse_regular (Loose_Route, array,
							 start, finish)
	         val rest = parse_list (array, position, finish)
	     in option :: rest
	     end
	  | 136 (* Obsolete stream SATNET *) =>
	     parse_list (array, start + 4, finish)
	  | 137 (* strict_source_routing_byte *) =>
	     let val (option, position) = parse_regular (Strict_Route, array,
							 start, finish)
	         val rest = parse_list (array, position, finish)
	     in option :: rest
	     end
	  | _ (* Unknown option. *) =>
	     parse_list (array, start + 1, finish)

  in (* local *)

   fun parse {data, start, length} =
        ((parse_list (data, start, start + length - 1))
	 handle Incorrect_Options_Encountered => []
	      | Subscript => [])

  end (* local *)

(*
	4.	receive_time_stamp_option

	Do the processing needed on receipt of a timestamp option.
*)

   fun receive_time_stamp ([], _) = []
     | receive_time_stamp ((opt as
			    Time_Stamp {length, position,
					overflow, stamps}) :: rest, ip) =
        (let fun return_overflow () =
	          if FoxWord8.<(overflow, (FoxWord8.intToWord 0xF)) then
		   Time_Stamp {length = length, position = position,
			       overflow = FoxWord8.+ (overflow,
						      FoxWord8.intToWord 1),
			       stamps = stamps}
		  else opt
	     val (ut_in_seconds, micro_seconds) = B.V.Time.get_universal_time ()
	     val seconds = FoxWord32.intToWord ut_in_seconds
	     val ip_time_size = FoxWord8.intToWord (ip_size + 4)
         in if FoxWord8.> (position, length) then
					(* No space left for any additions. *)
	     return_overflow ()
	    else
	     case stamps of
	        Stamp list =>
		 Time_Stamp {length = length,
			     position = FoxWord8.+ (position,
						    FoxWord8.intToWord 4),
			     overflow = overflow,
			     stamps = Stamp (list @ [seconds])}
	      | Record_Stamp list =>
		 if FoxWord8.<=(FoxWord8.+(position,ip_time_size),length) then
		  Time_Stamp {length = length,
			      position = FoxWord8.+(position, ip_time_size),
			      overflow = overflow,
			      stamps =
			        Record_Stamp (list @
					      [{ip = ip, time = seconds}])}
		 else return_overflow ()
	      | Given_Stamp list =>
		 (case rev list of
		     [] => opt
		   | ({ip = last_ip, time = last_time} :: rest) =>
		      if ip_equal (last_ip, ip) then
		       Time_Stamp {length = length,
				   position = FoxWord8.+ (position,
							  ip_time_size),
				   overflow = overflow,
				   stamps =
				    Given_Stamp (rev ({ip = last_ip,
						       time = SOME seconds} ::
						      rest))}
		      else opt)
	 end) :: (receive_time_stamp (rest, ip))
     | receive_time_stamp (opt :: rest, destination) =
        opt :: (receive_time_stamp (rest, destination))

(*
	5.	function size

	Extract the byte length from the IP options.
*)

  local 
   fun size_option (Strict_Route {length, ...}) = FoxWord8.wordToInt length
     | size_option (Loose_Route {length, ...}) = FoxWord8.wordToInt length
     | size_option (Record_Route {length, ...}) = FoxWord8.wordToInt length
     | size_option (Time_Stamp {length, ...}) = FoxWord8.wordToInt length

   fun size_options_list [] = 0
     | size_options_list (head :: rest) =
       (size_option head) + (size_options_list rest)

  in

   (* size_options must always return a size that is a multiple of four. *)

   fun size_options options =
        let val unpadded_size = size_options_list options
	    val unpadded_size_mod4 = unpadded_size mod 4
        in if unpadded_size_mod4 = 0 then unpadded_size
	   else unpadded_size + (4 - unpadded_size_mod4)
        end

  end (* local *)

(*
	6.	emit_options

 The options must have passed check_options.

*)

  val end_of_options_list_byte = (FoxWord8.intToWord 0)
  val no_operation_byte = (FoxWord8.intToWord 1)
  val strict_source_routing_byte = (FoxWord8.intToWord 137)
  val loose_source_routing_byte = (FoxWord8.intToWord 131)
  val record_route_byte = (FoxWord8.intToWord 7)
  val time_stamp_byte = (FoxWord8.intToWord 68)
  val stamps_byte = (FoxWord8.intToWord 0)
  val record_stamps_byte = (FoxWord8.intToWord 1)
  val given_stamps_byte = (FoxWord8.intToWord 3)

  fun emit_rest (array, start, finish) =
       if start > finish then ()
       else
	(FoxWord8.update (array, start, (FoxWord8.intToWord 0));
	 emit_rest (array, start + 1, finish))

  fun emit_route (array, start, finish, []) = emit_rest (array, start, finish)
    | emit_route (array, start, finish, ip :: rest) =
       if (start + ip_size > finish + 1) then
        (local_print "malformed emit_route while processing option";
	 raise Illegal_Option "malformed emit_route while processing option")
       else
        (B.Copy.copy (ip_to_bytes ip, 0, ip_size, array, start);
         emit_route (array, start + ip_size, finish, rest))

  fun emit_time (array, start, finish, []) = emit_rest (array, start, finish)
    | emit_time (array, start, finish, time :: rest) =
       if (start + 4 > finish + 1) then
        (local_print "malformed emit_time while processing option";
	 raise Illegal_Option "malformed emit_time while processing option")
       else
	(FoxWord32.update (array, start, B.Order.B4.to_big time);
	 emit_time (array, start + 4, finish, rest))

  fun emit_ip_time (array, start, finish, []) =
       emit_rest (array, start, finish)
    | emit_ip_time (array, start, finish, {ip, time} :: rest) =
       if (start + ip_size + 4 > finish + 1) then
        (local_print "malformed emit_ip_time while processing option";
	 raise Illegal_Option "malformed emit_ip_time while processing option")
       else
	(B.Copy.copy (ip_to_bytes ip, 0, ip_size, array, start);
	 FoxWord32.update (array, start + ip_size, B.Order.B4.to_big time);
	 emit_ip_time (array, start + ip_size + 4, finish, rest))

  fun emit_ip_maybe (array, start, finish, []) =
       emit_rest (array, start, finish)
    | emit_ip_maybe (array, start, finish, {ip, time} :: rest) =
       if (start + ip_size + 4 > finish + 1) then
        (local_print "malformed emit_ip_maybe while processing option";
	 raise Illegal_Option "malformed emit_ip_maybe processing option")
       else
	(B.Copy.copy (ip_to_bytes ip, 0, ip_size, array, start);
	 FoxWord32.update (array, start + ip_size,
		       case time of
			  NONE => (FoxWord32.intToWord 0)
			| SOME x => B.Order.B4.to_big x);
	 emit_ip_maybe (array, start + ip_size + 4, finish, rest))

  fun emit_list (array, start, []) =
       (* Fill out the remaining options with padding. *)
       if ByteArray.length array = start then ()
       else emit_rest (array, start, ByteArray.length array - 1)
    | emit_list (array, start,
		 Strict_Route {length, position, route} :: rest) =
       (FoxWord8.update (array, start, strict_source_routing_byte);
        FoxWord8.update (array, start + 1, length);
        FoxWord8.update (array, start + 2, position);
	emit_route (array, start + 3, start + FoxWord8.wordToInt length - 1, route);
        emit_list (array, start + FoxWord8.wordToInt length, rest))
    | emit_list (array, start, Loose_Route {length, position, route} :: rest) =
       (FoxWord8.update (array, start, loose_source_routing_byte);
        FoxWord8.update (array, start + 1, length);
        FoxWord8.update (array, start + 2, position);
	emit_route (array, start + 3, start + FoxWord8.wordToInt length - 1, route);
        emit_list (array, start + FoxWord8.wordToInt length, rest))
    | emit_list (array, start,
		 Record_Route {length, position, route} :: rest) =
       (FoxWord8.update (array, start, record_route_byte);
        FoxWord8.update (array, start + 1, length);
        FoxWord8.update (array, start + 2, position);
	emit_route (array, start + 3, start + FoxWord8.wordToInt length - 1, route);
        emit_list (array, start + FoxWord8.wordToInt length, rest))
    | emit_list (array, start,
		 Time_Stamp {length, position, overflow, stamps} :: rest) =
       (FoxWord8.update (array, start, time_stamp_byte);
	FoxWord8.update (array, start + 1, length);
	FoxWord8.update (array, start + 2, position);
	case stamps of
	   Stamp list =>
	    (FoxWord8.update (array, start + 3,
			   FoxWord8.orb (FoxWord8.lshift (overflow, 4), stamps_byte));
	     emit_time (array, start + 4, start + FoxWord8.wordToInt length - 1,
			list))
	 | Record_Stamp list =>
	    (FoxWord8.update (array, start + 3,
			   FoxWord8.orb (FoxWord8.lshift (overflow, 4),
				     record_stamps_byte));
	     emit_ip_time (array, start + 4, start + FoxWord8.wordToInt length - 1,
			   list))
	 | Given_Stamp list =>
	    (FoxWord8.update (array, start + 3,
			   FoxWord8.orb (FoxWord8.lshift (overflow, 4),
				     given_stamps_byte));
	     emit_ip_maybe (array, start + 4,
			    start + FoxWord8.wordToInt length - 1, list));
	emit_list (array, start + (FoxWord8.wordToInt length), rest))

  fun emit options =
       let val size = size_options options
       in if size > 60 then		(* That's all that fits in 4 bits. *)
           (local_print "options too large, max of 60 bytes";
	    raise Illegal_Option "options too large, max of 60 bytes")
	  else
	   let val options_array = ByteArray.array (size, 0)
	   in emit_list (options_array, 0, options);
	      (options_array, size)
	   end
       end

(*
	7.	functions check_option and check

*)

  fun check_option (Strict_Route {length, position, route}) =
            (* RFC 791 says that the position can be larger than
               the length, so as long as you are specifying a
               reasonable number of bytes I won't complain. *)
       let val op + = FoxWord8.+
	   val op * = FoxWord8.*
	   val required_length = (FoxWord8.intToWord 3) + (FoxWord8.intToWord 4) * FoxWord8.intToWord (List.length route)
       in FoxWord8.>= (position, (FoxWord8.intToWord 4)) andalso
	  FoxWord8.>= (length, required_length)
       end
    | check_option (Loose_Route {length, position, route}) =
       let  val op + = FoxWord8.+
	   val op * = FoxWord8.*
	   val required_length = (FoxWord8.intToWord 3) + (FoxWord8.intToWord 4) * FoxWord8.intToWord (List.length route)
       in FoxWord8.>= (position, (FoxWord8.intToWord 4)) andalso
          FoxWord8.>= (length, required_length)
       end
    | check_option (Record_Route {length, position, route}) =
       let val op + = FoxWord8.+
	   val op * = FoxWord8.*
	   val required_length = (FoxWord8.intToWord 3) + (FoxWord8.intToWord 4) * FoxWord8.intToWord (List.length route)
       in FoxWord8.>= (position, (FoxWord8.intToWord 4)) andalso
	  FoxWord8.>= (length, required_length)
       end
    | check_option (Time_Stamp {length, position, overflow, stamps}) =
       let val b1_ip_size = FoxWord8.intToWord ip_size
	   val op + = FoxWord8.+
	   val op * = FoxWord8.*
	   val bytes = case stamps of
	                  Stamp x => (FoxWord8.intToWord 4) * FoxWord8.intToWord (List.length x)
			| Record_Stamp x =>
			   ((FoxWord8.intToWord 4) + b1_ip_size) * FoxWord8.intToWord (List.length x)
			| Given_Stamp x =>
			   ((FoxWord8.intToWord 4) + b1_ip_size) * FoxWord8.intToWord (List.length x)
	   val required_length = (FoxWord8.intToWord 4) + bytes
       in FoxWord8.>= (position, (FoxWord8.intToWord 5)) andalso
	  FoxWord8.>= (length, required_length) andalso
	  (FoxWord8.>= (position, length) orelse overflow = (FoxWord8.intToWord 0))
       end

  fun check_for_only_one_source_route (Strict_Route _ :: rest, 1) = false
    | check_for_only_one_source_route (Loose_Route _ :: rest, 1) = false
    | check_for_only_one_source_route (Strict_Route _ :: rest, 0) =
       check_for_only_one_source_route (rest, 1)
    | check_for_only_one_source_route (Loose_Route _ :: rest, 0) =
       check_for_only_one_source_route (rest, 1)
    | check_for_only_one_source_route (_ :: rest, n) =
       check_for_only_one_source_route (rest, n)
    | check_for_only_one_source_route ([], _) = true

  fun check options =
       case options of
	  [] => false
	| _ =>
	   let fun ifboth (a, b) = a andalso b
	   in (B.V.List.fold ifboth (map check_option options) true) andalso
	      check_for_only_one_source_route (options, 0)
	   end

 end (* struct *)


