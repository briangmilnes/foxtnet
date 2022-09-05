(*
$Log: mach_portUser.sml,v $
Revision 1.2  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.1  1994/10/20  17:56:54  cline
Initial revision

Revision 1.3  1994/03/07  14:52:25  esb
commented out unused functions.

Revision 1.2  94/03/01  22:00:12  esb
added RCS log.

*)
(* functor *) structure Mach_Port (* (
                structure MachIPC : MACHIPC
                structure Word    : WORD
                structure Mig_Base: MIG_BASE
                structure Thread_System : THREAD_SYSTEM
                sharing type MachIPC.word = Word.word =
                             Mig_Base.word
                sharing type MachIPC.port = Mig_Base.port
                sharing type MachIPC.buf = ByteArray.bytearray) *)
                    : MACH_PORT =
 struct

  type word = MachIPC.word
  type port = MachIPC.port
  type msg_field_name = MachIPC.msg_field_name
  type buf = ByteArray.bytearray

  exception Mach_Port of Word.word

  fun mig_return' w = Mig_Base.mig_return (fn x => (raise Mach_Port x), w)

  val absolute_max_size = 64		(* max (max_size for all max_size) *)
  val msg_buf = ByteArray.array (absolute_max_size, 0)

   fun mach_port_allocate (port', right) =
        let val in_size = 32
	    val out_size = 40
	    val max_size = 40
	    val reply_port' = Mig_Base.get_reply_port ()

	    val hdr' = MachIPC.MSG_HEADER {
					   complex = false,
					   size = in_size,
					   local_port = reply_port',
					   local_right =
					      MachIPC.MAKE_SEND_ONCE,
					   remote_port = port',
					   remote_right = MachIPC.COPY_SEND,
					   kind = MachIPC.MSG_TYPE_NORMAL,
					   id = Word.word (3204)}

	    val rightType' = MachIPC.MSG_FIELD_HEADER
	                        {name = MachIPC.MSG_TYPE_INTEGER_32,
				 size = 32,
				 number = 1,
				 inline = true,
				 longform = false,
				 deallocate = false}

(*
	    val msg_buf = get_buf' max_size
*)
	    val _ = MachIPC.set_header (hdr', msg_buf)
	    val infield_headers = [rightType']
	    val _ = MachIPC.set_field_headers (infield_headers, msg_buf)
	    val _ = (FoxWord32.update (msg_buf, 28, right))
	    val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	    val _ = MachIPC.mach_msg (msg_buf, msg_options, 32, 40,
				      reply_port', 0, MachIPC.port_null)
	    val (MachIPC.MSG_HEADER {size, complex, id, ...}) =
	          MachIPC.get_header msg_buf
	    val (_, id) = Word.split id
	    val field_headers = MachIPC.get_field_headers msg_buf
	in case (size, (* complex, *) id, field_headers) of 
	      (40, 3304,
	       [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32,
					   size = 32,
					   number = 1,
					   inline = true, ...}),
		(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_PORT_NAME,
					   size = 32,
					   number = 1,
					   inline = true, ...})]) =>
	       (mig_return' (FoxWord32.sub (msg_buf, 28));
		((MachIPC.sub_port (msg_buf,36))))
	    | (32, (* false,*) 3304,
	       [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32,
					   size = 32,
					   number = 1,
					   inline = true, ...})]) =>
	       (mig_return' (FoxWord32.sub (msg_buf,28));
		raise Mach_Port (Word.word 0))
            | _ => raise Mach_Port (Word.word 0)
	end

   fun mach_port_destroy (port', name) =
        let val in_size = 32
	 val out_size = 32
	 val max_size = 32
	 val reply_port' = Mig_Base.get_reply_port ()

	 val hdr' = MachIPC.MSG_HEADER {complex = false,
					size = in_size,
					local_port = reply_port',
					local_right = MachIPC.MAKE_SEND_ONCE,
					remote_port = port',
					remote_right = MachIPC.COPY_SEND,
					kind = MachIPC.MSG_TYPE_NORMAL,
					id = Word.word (3205)}

	 val nameType' = MachIPC.MSG_FIELD_HEADER
	                    {name = MachIPC.MSG_TYPE_PORT_NAME,
			     size = 32,
			     number = 1,
			     inline = true,
			     longform = false,
			     deallocate = false}

(*
	 val msg_buf = get_buf' max_size
*)
	 val _ = MachIPC.set_header (hdr',msg_buf)
	 val infield_headers = [nameType']
	 val _ = MachIPC.set_field_headers (infield_headers, msg_buf)
	 val _ = (MachIPC.update_port (msg_buf,28,name))
	 val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	 val _ = MachIPC.mach_msg (msg_buf, msg_options, 32, 32,
				   reply_port', 0, MachIPC.port_null)
	 val (MachIPC.MSG_HEADER {size, complex, id, ...}) =
	       MachIPC.get_header msg_buf
	 val (_, id) = Word.split id
	 val field_headers = MachIPC.get_field_headers msg_buf
	in case (size, (* complex, *) id, field_headers) of 
	    (32, 3305,
	     [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32,
					 size = 32,
					 number = 1,
					 inline = true, ...})]) =>
	     (mig_return' (FoxWord32.sub (msg_buf,28));
	      ())
          | _ => raise Mach_Port (Word.word 0)
	end

   fun mach_port_set_qlimit (port', name, qlimit) =
        let val in_size = 40
	    val out_size = 32
	    val max_size = 40
	    val reply_port' = Mig_Base.get_reply_port ()

	    val hdr' = MachIPC.MSG_HEADER
	                {complex = false, 
			 size = in_size, 
			 local_port = reply_port', 
			 local_right = MachIPC.MAKE_SEND_ONCE, 
			 remote_port = port', 
			 remote_right = MachIPC.COPY_SEND, 
			 kind = MachIPC.MSG_TYPE_NORMAL, 
			 id = Word.word (3210)}

	    val nameType' = MachIPC.MSG_FIELD_HEADER
	                       {name = MachIPC.MSG_TYPE_PORT_NAME, 
				size = 32, 
				number = 1, 
				inline = true, 
				longform = false, 
				deallocate = false}

	    val qlimitType' = MachIPC.MSG_FIELD_HEADER
	                          {name = MachIPC.MSG_TYPE_INTEGER_32, 
				   size = 32, 
				   number = 1, 
				   inline = true, 
				   longform = false, 
				   deallocate = false}

(*
	    val msg_buf = get_buf' max_size
*)
	    val _ = MachIPC.set_header (hdr', msg_buf)
	    val infield_headers = [nameType', qlimitType']
	    val _ = MachIPC.set_field_headers (infield_headers, msg_buf)
	    val _ = (MachIPC.update_port (msg_buf, 28, name))
	    val _ = (FoxWord32.update (msg_buf, 36, qlimit))
	    val msg_options = [MachIPC.MSG_OPTION_SEND, MachIPC.MSG_OPTION_RCV]
	    val _ = MachIPC.mach_msg (msg_buf, msg_options, 40, 32,
				      reply_port', 0, MachIPC.port_null)
	    val (MachIPC.MSG_HEADER {size, complex, id, ...}) =
	           MachIPC.get_header msg_buf
	    val (_, id) = Word.split id
	    val field_headers = MachIPC.get_field_headers msg_buf
	in case (size, (* complex, *) id, field_headers) of 
	      (32, 3310, 
	       [(MachIPC.MSG_FIELD_HEADER {name = MachIPC.MSG_TYPE_INTEGER_32, 
					   size = 32, 
					   number = 1, 
					   inline = true, ...})]) =>
	       (mig_return' (FoxWord32.sub (msg_buf, 28));
		())
            | _ => raise Mach_Port (Word.word 0)
	end

 end
