structure Word =
 struct
  type word = FoxWord32.word
  fun word i = FoxWord32.intToWord i
  exception Value
  fun value w = (FoxWord32.wordToInt w) handle Overflow => raise Value
  val mask = SW.n32 "0xffff"
  fun split w = 
       let val lo = value (FoxWord32.andb (w, mask))
	   val hi = value (FoxWord32.rshiftl (w, 16))
       in (hi, lo)
       end
  fun join (x, y) =
       FoxWord32.orb (FoxWord32.lshift (FoxWord32.intToWord x, 16), 
		      FoxWord32.intToWord y)
  val andw = FoxWord32.andb
  val orw = FoxWord32.orb
  val xorw = FoxWord32.xorb
 end (* Word *)

structure MachIPC: MACHIPC  = 
 struct
  type port = Word.word
  type buf = ByteArray.bytearray
  type word = Word.word

  val port_size = 4
  val sub_port = FoxWord32.sub 
  val update_port = FoxWord32.update

  fun port_to_word p = p
  fun word_to_port w = w

  fun assign_ptr (dest, offset, pointer) =
       let val p = ByteArray.array (4, 0)
       in (System.Unsafe.cast p) := pointer;
	  FoxWord32.update (dest, offset, FoxWord32.sub (p, 0))
       end
       
  datatype msg_kind  = 
      MSG_TYPE_NORMAL
    | MSG_TYPE_EMERGENCY
(*
    | MSG_TYPE_CAMELOT
    | MSG_TYPE_ENCRYPTED
    | MSG_TYPE_RPC
*)

  exception Msg_Kind

  fun msg_kind_to_int MSG_TYPE_NORMAL = 0x0
    | msg_kind_to_int MSG_TYPE_EMERGENCY = 0x1
(*
	 | msg_kind_to_int MSG_TYPE_CAMELOT   = (1 << 6)
	 | msg_kind_to_int MSG_TYPE_ENCRYPTED = (1 << 7)
	 | msg_kind_to_int MSG_TYPE_RPC       = (1 << 8)
*)

  val op && = Bits.andb
  infix 9 &&
  val op || = Bits.orb
  infix 9 ||
  val op << = Bits.lshift
  infix 9 <<
  val op >> = Bits.rshift
  infix 9 >>

  fun int_to_msg_kind x  = 
       (case (x && 0x1) of
	   0x0 => MSG_TYPE_NORMAL
	 | 0x1 => MSG_TYPE_EMERGENCY
	 | _ => raise Msg_Kind)

  datatype msg_header_right = 
      MAKE_SEND | COPY_SEND | MOVE_SEND | MAKE_SEND_ONCE |
      MOVE_SEND_ONCE | MOVE_RECEIVE | MAKE_NONE

  exception Msg_Header_Right
	
  fun msg_header_right_to_int MOVE_RECEIVE = 16
    | msg_header_right_to_int MOVE_SEND = 17
    | msg_header_right_to_int MOVE_SEND_ONCE = 18
    | msg_header_right_to_int COPY_SEND = 19
    | msg_header_right_to_int MAKE_SEND = 20
    | msg_header_right_to_int MAKE_SEND_ONCE = 21
    | msg_header_right_to_int MAKE_NONE = 0

  fun int_to_msg_header_right 16 = MOVE_RECEIVE
    | int_to_msg_header_right 17 = MOVE_SEND
    | int_to_msg_header_right 18 = MOVE_SEND_ONCE
    | int_to_msg_header_right 19 = COPY_SEND
    | int_to_msg_header_right 20 = MAKE_SEND
    | int_to_msg_header_right 21 = MAKE_SEND_ONCE
    | int_to_msg_header_right 0  = MAKE_NONE
    | int_to_msg_header_right _  = raise Msg_Header_Right

  datatype msg_header = MSG_HEADER of
      {complex: bool, 
       remote_right: msg_header_right, 
       remote_port: port, 
       local_right: msg_header_right, 
       local_port: port, 
       size: int, 
       kind: msg_kind, 
       id: Word.word}

  datatype port_right = 
      PORT_RIGHT_SEND     | PORT_RIGHT_RECEIVE   | PORT_RIGHT_SEND_ONCE
    | PORT_RIGHT_PORT_SET | PORT_RIGHT_DEAD_NAME | PORT_RIGHT_NUMBER

  val port_right_send      = SW.n32 "0"
  val port_right_receive   = SW.n32 "1"
  val port_right_send_once = SW.n32 "2"
  val port_right_port_set  = SW.n32 "3"
  val port_right_dead_name = SW.n32 "4"
  val port_right_number    = SW.n32 "5"

  val port_right_to_word  = 
        fn PORT_RIGHT_SEND      => port_right_send
	 | PORT_RIGHT_RECEIVE   => port_right_receive
	 | PORT_RIGHT_SEND_ONCE => port_right_send_once
	 | PORT_RIGHT_PORT_SET  => port_right_port_set
	 | PORT_RIGHT_DEAD_NAME => port_right_dead_name
	 | PORT_RIGHT_NUMBER    => port_right_number

  val port_right_to_int  = 
	fn PORT_RIGHT_SEND      => 0
	 | PORT_RIGHT_RECEIVE   => 1
	 | PORT_RIGHT_SEND_ONCE => 2
	 | PORT_RIGHT_PORT_SET  => 3
	 | PORT_RIGHT_DEAD_NAME => 4
	 | PORT_RIGHT_NUMBER    => 5

  exception Port_Right

  fun word_to_port_right w = 
      (case FoxWord32.wordToInt w of
	 0 => PORT_RIGHT_SEND
       | 1 => PORT_RIGHT_RECEIVE
       | 2 => PORT_RIGHT_SEND_ONCE
       | 3 => PORT_RIGHT_PORT_SET
       | 4 => PORT_RIGHT_DEAD_NAME
       | 5 => PORT_RIGHT_NUMBER
       | _ => raise Port_Right)
	 handle Overflow => raise Port_Right

  datatype msg_field_name  = 
	MSG_TYPE_UNSTRUCTURED | MSG_TYPE_INTEGER_16
      | MSG_TYPE_INTEGER_32   | MSG_TYPE_CHAR   | MSG_TYPE_BYTE
      | MSG_TYPE_REAL_32      | MSG_TYPE_STRING | MSG_TYPE_PORT_NAME
      | MSG_TYPE_MOVE_RECEIVE | MSG_TYPE_MOVE_SEND | MSG_TYPE_MOVE_SEND_ONCE
      | MSG_TYPE_COPY_SEND    | MSG_TYPE_MAKE_SEND | MSG_TYPE_MAKE_SEND_ONCE
      | MSG_TYPE_POLYMORPHIC

  val msg_field_name_to_int  = 
	fn MSG_TYPE_UNSTRUCTURED   => 0
	 | MSG_TYPE_INTEGER_16     => 1
	 | MSG_TYPE_INTEGER_32     => 2
	 | MSG_TYPE_CHAR           => 8
	 | MSG_TYPE_BYTE           => 9
	 | MSG_TYPE_REAL_32        => 10
	 | MSG_TYPE_STRING         => 12
	 | MSG_TYPE_PORT_NAME      => 15
	 | MSG_TYPE_MOVE_RECEIVE   => 16
	 | MSG_TYPE_MOVE_SEND      => 17
	 | MSG_TYPE_MOVE_SEND_ONCE => 18
	 | MSG_TYPE_COPY_SEND      => 19
	 | MSG_TYPE_MAKE_SEND      => 20
	 | MSG_TYPE_MAKE_SEND_ONCE => 21
	 | MSG_TYPE_POLYMORPHIC    => 0xff

  exception Msg_Field_Name

  val int_to_msg_field_name  = 
	fn 0  => MSG_TYPE_UNSTRUCTURED
	 | 1  => MSG_TYPE_INTEGER_16
	 | 2  => MSG_TYPE_INTEGER_32
	 | 8  => MSG_TYPE_CHAR
	 | 9  => MSG_TYPE_BYTE
	 | 10 => MSG_TYPE_REAL_32
	 | 12 => MSG_TYPE_STRING
	 | 15 => MSG_TYPE_PORT_NAME
	 | 16 => MSG_TYPE_MOVE_RECEIVE
	 | 17 => MSG_TYPE_MOVE_SEND
	 | 18 => MSG_TYPE_MOVE_SEND_ONCE
	 | 19 => MSG_TYPE_COPY_SEND
	 | 20 => MSG_TYPE_MAKE_SEND
	 | 21 => MSG_TYPE_MAKE_SEND_ONCE
	 | 0xff => MSG_TYPE_POLYMORPHIC
	 | _  => raise Msg_Field_Name

  val field_name_to_word = Word.word o msg_field_name_to_int
  val word_to_field_name = int_to_msg_field_name o Word.value

  datatype msg_field_header =
      MSG_FIELD_HEADER of {name: msg_field_name, 
			   size: int, 
			   number: int, 
			   inline: bool, 
			   longform: bool, 
			   deallocate: bool}

  datatype msg_option = 
	MSG_OPTION_NONE | MSG_OPTION_SEND | MSG_OPTION_RCV | 
	MSG_OPTION_SEND_TIMEOUT | MSG_OPTION_SEND_NOTIFY |
	MSG_OPTION_SEND_CANCEL | MSG_OPTION_SEND_INTERRUPT | 
	MSG_OPTION_RCV_INTERRUPT | MSG_OPTION_RCV_TIMEOUT | 
	MSG_OPTION_RCV_NOTIFY | MSG_OPTION_RCV_LARGE

  val msg_option_to_int  = 
	fn MSG_OPTION_NONE           => 0x00000000
	 | MSG_OPTION_SEND           => 0x00000001
	 | MSG_OPTION_RCV            => 0x00000002
	 | MSG_OPTION_SEND_TIMEOUT   => 0x00000010
	 | MSG_OPTION_SEND_NOTIFY    => 0x00000020
	 | MSG_OPTION_SEND_INTERRUPT => 0x00000040
	 | MSG_OPTION_SEND_CANCEL    => 0x00000080
	 | MSG_OPTION_RCV_TIMEOUT    => 0x00000100
	 | MSG_OPTION_RCV_NOTIFY     => 0x00000200
	 | MSG_OPTION_RCV_INTERRUPT  => 0x00000400
	 | MSG_OPTION_RCV_LARGE      => 0x00000800

  exception Mach_Error of string
  local
   fun cfun x =
        ((System.Unsafe.CInterface.c_function "FoxNet" x)
	 handle e as System.Unsafe.CInterface.CFunNotFound x =>
	         (print (x ^ "\n");
		  raise e))
		      
   exception SysError = System.Unsafe.CInterface.SysError
   fun make_safe f x =
        ((f x)
	 handle SysError (_, s) => raise (Mach_Error s))
   val u_task_self: unit -> port = cfun "task_self"
   val u_port_allocate: port * int -> port = cfun "port_allocate"
   fun port_allocate' (ipc_space, port_right)  = 
        u_port_allocate (ipc_space, port_right_to_int port_right)
   val u_port_insert_right: port * port * port * int -> unit = 
        cfun "port_insert_right"
   fun port_insert_right' (ipc_space, port1, port2, r)  = 
        u_port_insert_right (ipc_space, port1, port2, 
			     msg_header_right_to_int r)
   val u_mach_msg: buf * int * int * int * port * int * port -> int32 =
        cfun "mach_msg"
   type 'a io_arg = (buf * int * int * int * port * int * port * 
		     'a * buf * bool)

   fun strip l =
        let fun strip_it ((_, _, _, _, _, _, _, d, b, _): 'a io_arg) = (d, b)
	in map strip_it l
	end
   fun options_to_int options  = 
        let fun f (opt, i) = (msg_option_to_int opt) || i
	in fold f options 0
	end
   val n4u0 = SW.n32 "0"
   fun mach_msg' (msg, options, send_size, rcv_size, 
		  rcv_port, timeout, notify)  = 
        let val code = u_mach_msg (msg, options_to_int options, 
				   send_size, rcv_size, rcv_port, 
				   timeout, notify)
	in if code = n4u0 then ()
	   else (print ("mach_msg': " ^ makestring code ^ " <> " ^
			makestring n4u0 ^ "\n");
		 raise Mach_Error (makestring code))
	end
  in
   val task_self = make_safe u_task_self
   val port_allocate = make_safe port_allocate'
   val port_insert_right = make_safe port_insert_right'
   val mach_msg = make_safe mach_msg'
  end (* local *)

  fun make_msg_bits (complex, remote_rt, local_rt)  = 
       let val hi = if complex then 0x8000 else 0x0000
	   val encoded_remote = msg_header_right_to_int remote_rt
	   val encoded_local = msg_header_right_to_int local_rt
	   val low = encoded_remote || (encoded_local << 8)
       in Word.join (hi, low)
       end

  fun unmake_msg_bits w  = 
       let val (hi, lo) = Word.split w
	   val complex = not((hi && 0x8000) = 0)
	   val r = lo && 0xff
	   val l = (lo >> 8) && 0xff
	   val remote_rt = int_to_msg_header_right r
	   val local_rt = int_to_msg_header_right l
       in (complex, remote_rt, local_rt)
       end

  val msg_header_size = 4 + 4 + 4 + 4 + 4 + 4

  fun set_header (MSG_HEADER {complex, remote_right, local_right, 
			      remote_port, local_port, size, kind, id}, 
		  buf) = 
       let val bits = make_msg_bits (complex, remote_right, local_right)
	   val k = msg_kind_to_int kind
       in FoxWord32.update (buf, 0, bits);
	  FoxWord32.update (buf, 4, FoxWord32.intToWord size);
	  FoxWord32.update (buf, 8, remote_port);
	  FoxWord32.update (buf, 12, local_port);
	  FoxWord32.update (buf, 16, FoxWord32.intToWord k);
	  FoxWord32.update (buf, 20, id)
       end

  fun get_header buf = 
       let val bits = FoxWord32.sub (buf, 0)
	   val (complex, remote_right, local_right) = unmake_msg_bits bits
	   val size = FoxWord32.wordToInt (FoxWord32.sub (buf, 4))
	   val remote_port = FoxWord32.sub (buf, 8)
	   val local_port = FoxWord32.sub (buf, 12)
	   val k = FoxWord32.wordToInt (FoxWord32.sub (buf, 16))
	   val kind = int_to_msg_kind k
	   val id = FoxWord32.sub (buf, 20)
       in MSG_HEADER {complex = complex, 
		      remote_right = remote_right, 
		      remote_port = remote_port, 
		      local_right = local_right, 
		      local_port = local_port, 
		      size = size, 
		      kind = kind, 
		      id = id}
       end

  fun field_header_size (MSG_FIELD_HEADER{longform, ...}) = 
       if longform then 12 else 4

  fun bits2bytes x = (x + 7) div 8

  fun next_longword x = ((x + 3) div 4) * 4

  fun field_size (x as (MSG_FIELD_HEADER{size, number, inline, ...})) = 
       (if inline then next_longword (bits2bytes (size * number))
	else 4) + (field_header_size x)

  fun bit2bool 0 = false
    | bit2bool _ = true

  fun bool2bit true  = 1
    | bool2bit false = 0

  fun decode_byte x = 
       let val x0 = bit2bool (x && 1)
	   val x1 = bit2bool (x && 2)
	   val x2 = bit2bool (x && 4)
	   val x3 = bit2bool (x && 8)
       in (x0, x1, x2, x3)
       end

  fun encode_byte (x0, x1, x2, x3)  = 
	(bool2bit x0) || ((bool2bit x1) << 1) || ((bool2bit x2) << 2) ||
	((bool2bit x3) << 3)

    (* the position is the buffer position in bytes where the field
       header starts *)
  fun get_field_header (pos, buf)  = 
       let val name = int_to_msg_field_name (ByteArray.sub (buf, pos))
	   val size = ByteArray.sub (buf, pos + 1)
	   val n1 = FoxWord16.wordToInt (FoxWord16.sub (buf, pos + 2))
	   val number = n1 && 0xfff
	   val (inline, longform,
		deallocate, _) = decode_byte ((n1 && 0xf000) >> 12)
       in if longform then
	   let val name = int_to_msg_field_name
	                    (FoxWord16.wordToInt
			     (FoxWord16.sub (buf, pos + 4)))
	       val size = FoxWord16.wordToInt (FoxWord16.sub (buf, pos + 6))
	       val number = FoxWord32.wordToInt (FoxWord32.sub (buf, pos + 8))
	   in MSG_FIELD_HEADER {name = name, size = size, 
				number = number, inline = inline,
				deallocate = deallocate, longform = true}
	   end
	  else
	   MSG_FIELD_HEADER {name = name, size = size, number = number, 
			     inline = inline, deallocate = deallocate, 
			     longform = false}
       end

  fun set_field_header ((MSG_FIELD_HEADER {name, size, number, inline,
					   longform, deallocate}), 
			pos, buf) = 
       let val n = msg_field_name_to_int name
	   val b = encode_byte (inline, longform, deallocate, false)
       in if longform then
	   let val n1 = b << 12
	   in FoxWord16.update (buf, pos, FoxWord16.intToWord 0);
	      FoxWord16.update (buf, pos + 2, FoxWord16.intToWord n1);
	      FoxWord16.update (buf, pos + 4, FoxWord16.intToWord n);
	      FoxWord16.update (buf, pos + 6, FoxWord16.intToWord size);
	      FoxWord32.update (buf, pos + 8, FoxWord32.intToWord number)
	   end
	  else
	   let val n1 = number || (b << 12)
	   in ByteArray.update (buf, pos, n);
	      ByteArray.update (buf, pos + 1, size);
	      FoxWord16.update (buf, pos + 2, FoxWord16.intToWord n1)
	   end
       end

  fun bits2bytes x = (x + 7) div 8

  fun get_field_headers buf  = 
       let val msg_size = FoxWord32.wordToInt (FoxWord32.sub (buf, 4))
	   fun loop (prev, pos)  = 
		if pos >=  msg_size then (rev prev)
		else
		 let val f as (MSG_FIELD_HEADER {size, ...})  = 
		          get_field_header (pos, buf)
		     val pos' = pos + (field_size f)
		 in loop (f :: prev, pos')
		 end
       in loop ([], msg_header_size)
       end

  fun set_field_headers (l, b)  = 
       let fun loop (p, []) = ()
	     | loop (p, ((f as (MSG_FIELD_HEADER{size, ...})) :: rest))  = 
	       let val p' = p + (field_header_size f) + bits2bytes size
	       in set_field_header (f, p, b);
		  loop (p', rest)
	       end
       in loop (msg_header_size, l)
       end

  val port_null: port = SW.n32 "0"

 end (* struct *)

structure Mig_Base: MIG_BASE  = 
 struct
  type word = Word.word
  type port = MachIPC.port
  exception MIG_ARRAY_TO_LARGE
  exception MIG_SERVER_EXCEPTION
  exception MIG_NO_REPLY
  exception MIG_BAD_ARGUMENTS
  exception MIG_BAD_ID
  exception MIG_REMOTE_ERROR
  exception MIG_REPLY_MISMATCH
  exception MIG_TYPE_ERROR

(*
  (* This is the old version of the function -- word32s are no longer
     constructors so cannot be pattern matched in 107 *)

      fun mig_return (f: word -> unit , w: word): unit = 
	  (case w of
	       4u0  => () (* success *)
	     | 4uxfffffecd => (raise MIG_ARRAY_TO_LARGE)
	     | 4uxfffffece => (raise MIG_SERVER_EXCEPTION)
	     | 4uxfffffecf => (raise MIG_NO_REPLY)
	     | 4uxfffffed0 => (raise MIG_BAD_ARGUMENTS)
	     | 4uxfffffed1 => (raise MIG_BAD_ID)
	     | 4uxfffffed2 => (raise MIG_REMOTE_ERROR)
	     | 4uxfffffed3 => (raise MIG_REPLY_MISMATCH)
	     | 4uxfffffed4 => (raise MIG_TYPE_ERROR)
	     | _ => f w)
*)

  local
   val min_error = SW.n32 "0xffff0000"
   val mask = FoxWord32.rshiftl (min_error, 16)
   val zero32 = SW.n32 "0"
   fun get_error n  = 
        (FoxWord32.wordToInt (FoxWord32.rshiftl (n, 16)), 
         FoxWord32.wordToInt (FoxWord32.andb (n, mask)))

  in
   fun mig_return (f, w) = 
       if w = zero32 then ()		(* success *)
       else if FoxWord32.<= (w, min_error) then f w
       else
	case get_error w of
	   (0xffff, 0xfecd) => raise MIG_ARRAY_TO_LARGE
	 | (0xffff, 0xfece) => raise MIG_SERVER_EXCEPTION
	 | (0xffff, 0xfecf) => raise MIG_NO_REPLY
	 | (0xffff, 0xfed0) => raise MIG_BAD_ARGUMENTS
	 | (0xffff, 0xfed1) => raise MIG_BAD_ID
	 | (0xffff, 0xfed2) => raise MIG_REMOTE_ERROR
	 | (0xffff, 0xfed3) => raise MIG_REPLY_MISMATCH
	 | (0xffff, 0xfed4) => raise MIG_TYPE_ERROR
	 | _ => f w

  end (* local *)

  local
   val reply_port: port option ref = ref NONE

  in
   fun get_reply_port () = 
        case ! reply_port of 
	   (SOME p) => p
	 | NONE => 
	    let val p = MachIPC.port_allocate (MachIPC.task_self (), 
					       MachIPC.PORT_RIGHT_RECEIVE)
	    in reply_port := SOME p;
	       p
	    end

  end (* local *)
 end (* struct *)
