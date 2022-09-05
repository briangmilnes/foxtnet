(* this code was originally produced by SML/MIG, but needed major
 * fixes to start working right. Only the functions used for the
 * foxnet are likely to actually work as advertised.
 *)
(*
$Log: deviceUser.sml,v $
Revision 1.10  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.9  1995/02/21  16:25:33  robby
took out the threads system (which was being shadowed anyways)

Revision 1.8  1994/11/09  22:08:50  esb
removed some commented-out code, to make maintenance simpler.

Revision 1.7  94/06/05  18:43:47  milnes
Updated an arbitrary constant in device_set_filter that prevented
long filters.

Revision 1.6  1994/03/07  14:52:25  esb
commented out unused functions.

Revision 1.5  94/03/01  22:00:12  esb
added RCS log.

*)

functor Device (structure B: FOX_BASIS
		structure MachIPC: MACHIPC
		structure Word: WORD
		structure Mig_Base: MIG_BASE
		sharing type MachIPC.word = Word.word = Mig_Base.word =
		             FoxWord32.word
                    and type MachIPC.buf = ByteArray.bytearray
                    and type MachIPC.port = Mig_Base.port = int32): DEVICE =
 struct

  type word = Word.word
  type port = MachIPC.port
  type msg_field_name = MachIPC.msg_field_name
  type buf = ByteArray.bytearray

  exception Device of Word.word
  fun mig_return' w = Mig_Base.mig_return (fn x => (raise Device x), w)

  val absolute_max_size = 4132		(* max (max_size for all max_size) *)
  val msg_buf = ByteArray.array (absolute_max_size, 0)

  fun device_open (master_port, mode, name) =
       let val in_size = 172
           val out_size = 40
	   val max_size = 172
	   val reply_port' = Mig_Base.get_reply_port ()
	   val hdr' = MachIPC.MSG_HEADER
	                   {complex = false, size = in_size,
			    local_port = reply_port',
			    local_right = MachIPC.MAKE_SEND_ONCE,
			    remote_port = master_port,
			    remote_right = MachIPC.COPY_SEND,
			    kind = MachIPC.MSG_TYPE_NORMAL,
			    id = Word.word (2800)}
(*
	   val msg_buf = get_buf' max_size
*)
	   val modeType' = MachIPC.MSG_FIELD_HEADER 
	                        {name = MachIPC.MSG_TYPE_INTEGER_32,
				 size = 32,
				 number = 1,
				 inline = true,
				 longform = false,
				 deallocate = false}
	   val nameType' = MachIPC.MSG_FIELD_HEADER 
	                        {name = MachIPC.MSG_TYPE_STRING,
				 size = 1024,
				 number = 1,
				 inline = true,
				 longform = true,
				 deallocate = false}
	   val _ = MachIPC.set_header (hdr', msg_buf)
	   val infield_headers = [modeType', nameType']
	   val _ = MachIPC.set_field_headers (infield_headers, msg_buf)
	   val _ = FoxWord32.update (msg_buf, 28, mode)
	   fun cstrcopy ("", array, index) = ByteArray.update (array, index, 0)
	     | cstrcopy (s, array, index) =
	        (ByteArray.update (array, index,
				   B.V.Char.ord (B.V.String.ordof (s, 0)));
		 if size s > 1 then
		  cstrcopy (substring (s, 1, size s - 1), array, index + 1)
		 else
		  cstrcopy ("", array, index + 1))
	   val _ = cstrcopy (name, msg_buf, 44)
	   val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	   val _ = MachIPC.mach_msg (msg_buf, msg_options, 172, 40,
				     reply_port', 0, MachIPC.port_null)
	   val (MachIPC.MSG_HEADER {size, id, ...}) =
	             MachIPC.get_header msg_buf
	   val (_, id) = Word.split id
	   val field_headers = MachIPC.get_field_headers msg_buf
	   val (high, low) = Word.split (FoxWord32.sub (msg_buf, 28))
	   val res = (makestring high) ^ "." ^ (makestring low)
       in case (size, id, field_headers) of 
	     (40, 2900,
	      [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32,
					  size = 32, number = 1,
					  inline = true, ...}),
	       (MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_MOVE_SEND,
					  size = 32, number = 1,
					  inline = true, ...})]) =>
	     (* check mach return *)
	      (mig_return' (FoxWord32.sub (msg_buf, 28));
	       MachIPC.sub_port (msg_buf, 36))           (* return port *)
	   | (32, 2900,
	      [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32,
					  size = 32, number = 1,
					  inline = true, ...})]) =>
	     (* check mach return *)
	      (mig_return' (FoxWord32.sub (msg_buf, 28));
	       print ("device_open returned message with length 32, " ^
		      "code 2900, but no mach error code\n");
	       raise Device (Word.word 0))            (* no port returned *)
           | _ => 
	      (print ("device_open returned message with length " ^
		      makestring size ^
		      ", code " ^ makestring id ^ " (expected 40, 2900)\n");
	       raise Device (Word.word 0))          (* something else wrong *)
       end (* let *)

  fun device_close device =
       let val in_size = 24
           val out_size = 32
	   val max_size = 40
	   val reply_port' = Mig_Base.get_reply_port ()

	val hdr' = MachIPC.MSG_HEADER 
	  {complex = false, size = in_size, local_port = reply_port',
	   local_right = MachIPC.MAKE_SEND_ONCE, remote_port = device,
	   remote_right = MachIPC.COPY_SEND, kind = MachIPC.MSG_TYPE_NORMAL,
	   id = Word.word 2801}

(*
	val msg_buf = get_buf' max_size
*)
	val _ = MachIPC.set_header (hdr', msg_buf)
	val infield_headers = []
	val _ = MachIPC.set_field_headers (infield_headers, msg_buf)
	val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	val _ = MachIPC.mach_msg (msg_buf, msg_options, 24, 32,
				  reply_port', 0, MachIPC.port_null)
	val (MachIPC.MSG_HEADER {size, id, ...}) = MachIPC.get_header msg_buf
	val (_, id_low) = Word.split id
	val field_headers = MachIPC.get_field_headers msg_buf
    in case (size, id_low, field_headers) of 
          (32, 2901,
	   [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32,
				       size = 32, number = 1,
				       inline = true, ...})]) =>
	  (mig_return' (FoxWord32.sub (msg_buf,28));
	   ())
      | _ =>
	  (print ("device_close returned message with length "^makestring size^
		  ", code " ^ makestring id_low ^" (expected 32, 2901)\n");
	   raise Device (Word.word 0))
    end (* let *)

  fun device_get_status (device, flavor, status_len) =
      let val in_size = 40
	val out_size = 4132
	val max_size = 4132
	val reply_port' = Mig_Base.get_reply_port ()

	val hdr' = MachIPC.MSG_HEADER 
	  {complex = false, size = in_size,
	   local_port = reply_port', local_right = MachIPC.MAKE_SEND_ONCE,
	   remote_port = device, remote_right = MachIPC.COPY_SEND,
	   kind = MachIPC.MSG_TYPE_NORMAL, id = Word.word (2811)}

	val flavorType' = MachIPC.MSG_FIELD_HEADER 
	  {name = MachIPC.MSG_TYPE_INTEGER_32, size = 32,
	   number = 1, inline = true, longform = false,
	   deallocate = false}

	val status_lenType' = MachIPC.MSG_FIELD_HEADER 
	  {name = MachIPC.MSG_TYPE_INTEGER_32, size = 32,
	   number = 1, inline = true, longform = false,
	   deallocate = false}

(*
	val msg_buf = get_buf' max_size
*)
	val _ = MachIPC.set_header (hdr',msg_buf)
	val infield_headers = [flavorType', status_lenType']
	val _ = MachIPC.set_field_headers (infield_headers, msg_buf)
	val _ = (FoxWord32.update (msg_buf,28,flavor))
	val _ = (FoxWord32.update (msg_buf,36,status_len))
	val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	val _ = MachIPC.mach_msg (msg_buf,msg_options,40,4132,
				  reply_port',0,MachIPC.port_null)
	val (MachIPC.MSG_HEADER{size,id,...}) = MachIPC.get_header msg_buf
	val (_,id) = Word.split id
	val field_headers = MachIPC.get_field_headers msg_buf
      in case (size, id, field_headers) of 
	    (out_size, 2911,
	     [(MachIPC.MSG_FIELD_HEADER 
	       {name = MachIPC.MSG_TYPE_INTEGER_32,
		size = 32, number = 1,
		inline = true, ...}),
	      (MachIPC.MSG_FIELD_HEADER 
	       {name = MachIPC.MSG_TYPE_INTEGER_32,
		size = 32, number = status_len,
		inline = true, ...})]) =>
	    (mig_return' (FoxWord32.sub (msg_buf, 28));
	     (B.Create.copy_create (msg_buf, 36, 4096), status_len))
	  | (32,(* false,*) 2911,
	     [(MachIPC.MSG_FIELD_HEADER 
	       {name = MachIPC.MSG_TYPE_INTEGER_32,
	        size = 32, number = 1,
	        inline = true, ...})]) =>
	    (mig_return' (FoxWord32.sub (msg_buf, 28));
	     raise Device (Word.word 0))
	  | _ => raise Device (Word.word 0)
      end

  fun device_set_filter (device, (receive_port, receive_port_poly),
			  priority, (filter, filter_len)) =
        let val filter_bytes = filter_len * 2
            val filter_round = if filter_bytes mod 4 <> 0 then
	                        filter_bytes + 4 - (filter_bytes mod 4)
			       else filter_bytes
(* in old version of deviceUser:    val filter_type_longform = true *)
	    val filter_type_longform = false (* in latest mach *)
	    val filter_offset = if filter_type_longform then 52 else 44
	    val in_size = filter_offset + filter_round
	    val out_size = 32
	    val max_size = 244 (* 2 * 174, but who knows wy 174. *)
	    val reply_port' = Mig_Base.get_reply_port ()
	
	    val c_val = MachIPC.field_name_to_word receive_port_poly
	    val (_, c_low) = Word.split c_val
	    val complex = c_low >= 16 andalso c_low <= 21
	    val hdr' = MachIPC.MSG_HEADER
	                   {complex = c_low >= 16 andalso c_low <= 21,
			    size = in_size, 
			    local_port = reply_port',
			    local_right = MachIPC.MAKE_SEND_ONCE,
			    remote_port = device,
			    remote_right = MachIPC.COPY_SEND,
			    kind = MachIPC.MSG_TYPE_NORMAL,
			    id = Word.word 2812}

	    val receive_portType' = MachIPC.MSG_FIELD_HEADER
	                                {name = receive_port_poly,
					 size = 32,
					 number = 1, 
					 inline = true,
					 longform = false,
					 deallocate = false}

	    val priorityType' = MachIPC.MSG_FIELD_HEADER 
	                            {name = MachIPC.MSG_TYPE_INTEGER_32,
				     size = 32,
				     number = 1,
				     inline = true,
				     longform = false,
				     deallocate = false}

	    val filterType' = MachIPC.MSG_FIELD_HEADER
	                          {name = MachIPC.MSG_TYPE_INTEGER_16,
				   size = 16,
				   number = filter_len,
				   inline = true,
				   longform = filter_type_longform,
				   deallocate = false}
(*
	    val msg_buf = get_buf' max_size
*)
	    val _ = MachIPC.set_header (hdr', msg_buf)
	    val field_headers = [receive_portType', priorityType', filterType']
	    val _ = MachIPC.set_field_headers (field_headers, msg_buf)
	    val _ = FoxWord32.update (msg_buf, 28, receive_port)
	    val _ = FoxWord32.update (msg_buf, 36, priority)
	    val _ = B.Copy.copy (filter, 0, ByteArray.length filter,
				 msg_buf, filter_offset)
	    val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	    val _ = MachIPC.mach_msg (msg_buf, msg_options, in_size, out_size, 
				      reply_port', 0, MachIPC.port_null)
	    val (MachIPC.MSG_HEADER {size, id, ...}) =
	             MachIPC.get_header msg_buf
	    val (_, id_low) = Word.split id
	    val field_headers = MachIPC.get_field_headers msg_buf
	in case (size, id_low, field_headers) of 
	      (32, 2912,
	       [(MachIPC.MSG_FIELD_HEADER 
		 {name = MachIPC.MSG_TYPE_INTEGER_32,
		  size = 32,
		  number = 1,
		  inline = true, ...})]) =>
	       (mig_return' (FoxWord32.sub (msg_buf, 28));
		())
	    | _ =>
	       (print ("device_set_filter returned length " ^
		       makestring size ^ ", code " ^ makestring id_low ^
		       " (expected 32, 2912)\n");
		raise Device (Word.word 0))
	end
 end

