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

      timingtest.fun provides a functor to build TIMINGTEST structures.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor TimingTest

		iii.	RCS Log
	
$Log: timingtest.fun,v $
Revision 1.5  1994/09/12  18:21:02  milnes
Added timing for ip_no_icmp/ip.

Revision 1.4  1994/08/17  16:32:15  esb
the benchmarks now work.

Revision 1.3  1994/07/07  16:32:52  esb
minor changes

Revision 1.2  1994/06/16  21:52:46  danwang
Updated to use functorized Fox_Basis.

Revision 1.1  1994/04/20  14:42:54  milnes
Initial revision

		1.     functor TimingTest
*)

functor Timing_Test (structure B: FOX_BASIS): TIMING_TEST =
 struct
  type incoming_data = B.Dyn_Array.T
  type outgoing_data = B.Dyn_Array.T

  datatype protocol = Eth | Ip_No_Icmp | Ip | Udp | Tcp
  exception Emit_Error of string
  exception Parse_Error of string * B.Dyn_Array.T
  (* internal exception *) exception Parse of string

  fun parse_protocol 1u0 = Eth
    | parse_protocol 1u1 = Ip_No_Icmp
    | parse_protocol 1u2 = Ip
    | parse_protocol 1u3 = Udp
    | parse_protocol 1u4 = Tcp
    | parse_protocol _ = raise (Parse "parse_protocol")

  fun emit_protocol Eth  = 1u0
    | emit_protocol Ip_No_Icmp   = 1u1
    | emit_protocol Ip   = 1u2
    | emit_protocol Udp  = 1u3
    | emit_protocol Tcp  = 1u4

  fun makestring_protocol Eth  = "Eth"
    | makestring_protocol Ip_No_Icmp   = "Ip_No_Icmp"
    | makestring_protocol Ip   = "Ip"
    | makestring_protocol Udp  = "Udp"
    | makestring_protocol Tcp  = "Tcp"

  datatype check_data = None | Some | All

  fun parse_check_data 1u0 = None
    | parse_check_data 1u1 = Some
    | parse_check_data 1u2 = All
    | parse_check_data _ = raise (Parse "check_data")

  fun emit_check_data None = 1u0
    | emit_check_data Some = 1u1
    | emit_check_data All = 1u2

  fun makestring_check_data None = "None"
    | makestring_check_data Some = "Some"
    | makestring_check_data All = "All"

  fun parse_byte4 x = x
  fun emit_byte4 x = B.Order.B4.to_big x
  val parse_repetitions = parse_byte4
  val emit_repetitions = emit_byte4
  val parse_size = parse_byte4
  val emit_size = emit_byte4
  val parse_confirm = parse_byte4
  val emit_confirm = emit_byte4

  fun parse_bool 1u0 = false
    | parse_bool 1u1 = true
    | parse_bool _ = raise (Parse "parse_bool")

  fun emit_bool false = 1u0
    | emit_bool true = 1u1

  val parse_print_history = parse_bool
  val emit_print_history = emit_bool
  val parse_print_packets = parse_bool
  val emit_print_packets = emit_bool

  datatype test = Test of {protocol: protocol,
			   repetitions: ubyte4,
			   size: ubyte4,
			   confirm: ubyte4,
			   check_data: check_data,
			   print_packets: bool,
			   print_history: bool}

  val test_byte = 1u99
  (* This really ought to be done with flat types. *)
  val test_byte_at = 0
  val print_packets_at = test_byte_at + 1  (* 1 *)
  val check_data_at = print_packets_at + 1 (* 2 *)
  val protocol_at = check_data_at + 1      (* 3 *)

  val repetitions_at = protocol_at + 1     (* 4 *)
  val size_at = repetitions_at + 4         (* 8 *)
  val confirm_at =  size_at + 4            (* 12 *)
  val print_history_at = confirm_at + 4    (* 16 *)


  val get_byte1 = B.Dyn_Array.sub1

  fun get_byte4 (packet, index) =
       Fox_Basis.Order.B4.from_big (B.Dyn_Array.sub4 (packet, index))

  fun parse_test packet =
       if get_byte1(packet,test_byte_at) = test_byte then
        Test {protocol = parse_protocol (get_byte1 (packet, protocol_at)),
              repetitions = parse_repetitions (get_byte4 (packet,
							  repetitions_at)),
	      size = parse_size (get_byte4 (packet, size_at)),
	      confirm = parse_confirm (get_byte4 (packet, confirm_at)),
	      check_data =
	        parse_check_data (get_byte1 (packet, check_data_at)),
	      print_packets =
	        parse_print_packets (get_byte1 (packet, print_packets_at)),
	      print_history =
	        parse_print_history (get_byte1 (packet, print_history_at))}
	handle Parse s => raise (Parse_Error (s, packet))
	     | _ => raise (Parse_Error ("unknown error", packet))
       else raise (Parse_Error ("packet missing test_byte", packet))

  fun emit_test (allocate, connection,
		 Test {protocol, repetitions, size, confirm,
		       check_data, print_packets, print_history}) =
   let val (packet, send) = allocate (connection, 17)
   in B.Dyn_Array.update1 (packet, test_byte_at, test_byte);
      B.Dyn_Array.update1 (packet, protocol_at, emit_protocol protocol);
      B.Dyn_Array.update1 (packet, check_data_at,
			   emit_check_data check_data);
      B.Dyn_Array.update1 (packet, print_packets_at,
			   emit_print_packets print_packets);
      B.Dyn_Array.update4 (packet, repetitions_at,
			   emit_repetitions repetitions);
      B.Dyn_Array.update4 (packet, size_at, emit_size size);
      B.Dyn_Array.update4 (packet, confirm_at, emit_confirm confirm);
      B.Dyn_Array.update1 (packet, print_history_at,
			   emit_print_history print_history);
      send
    end

  fun makestring_test (Test {protocol, repetitions, size, confirm,
			     check_data, print_packets, print_history}) =
   ("Test {\n" ^
    "protocol = " ^ (makestring_protocol protocol)^ ", " ^
    "repetitions = " ^ (Byte4.makestring repetitions) ^ ", " ^
    "size = " ^ (Byte4.makestring size) ^ ", " ^
    "confirm = " ^ (Byte4.makestring confirm)  ^ ", " ^
    "check_data = " ^ (makestring_check_data check_data) ^ ", " ^
    "print_packets = " ^ (if print_packets then "true" else "false") ^ ", " ^
    "print_history = " ^ (if print_history then "true" else "false") ^ "}\n"
   )

   end

