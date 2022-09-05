signature WORD =
 sig
  eqtype word
  val word: int -> word
  val value: word -> int
  val split: word -> (int * int)
  val join: (int * int) -> word
  val andw: word * word -> word
  val orw: word * word -> word
  val xorw: word * word -> word
 end

signature MACHIPC =
 sig
  eqtype buf
  eqtype word
  eqtype port

  val port_size: int
  val sub_port: buf * int -> port
  val update_port: buf * int * port -> unit

  val port_to_word: port -> word
  val word_to_port: word -> port

  val assign_ptr: buf * int * buf -> unit

  datatype msg_kind =
      MSG_TYPE_NORMAL
    | MSG_TYPE_EMERGENCY

  datatype msg_header_right = 
      MAKE_SEND | COPY_SEND | MOVE_SEND | MAKE_SEND_ONCE |
      MOVE_SEND_ONCE | MOVE_RECEIVE | MAKE_NONE

  datatype msg_header =
      MSG_HEADER of {complex: bool,
		     remote_right: msg_header_right,
		     remote_port: port,
		     local_right: msg_header_right,
		     local_port: port,
		     size: int,
		     kind: msg_kind,
		     id: word}

  val msg_header_size: int

  datatype port_right = 
      PORT_RIGHT_SEND     | PORT_RIGHT_RECEIVE   | PORT_RIGHT_SEND_ONCE
    | PORT_RIGHT_PORT_SET | PORT_RIGHT_DEAD_NAME | PORT_RIGHT_NUMBER

  exception Port_Right
  val port_right_to_word: port_right -> word
  val word_to_port_right: word -> port_right

  datatype msg_field_name =
      MSG_TYPE_UNSTRUCTURED | MSG_TYPE_INTEGER_16
    | MSG_TYPE_INTEGER_32   | MSG_TYPE_CHAR   | MSG_TYPE_BYTE
    | MSG_TYPE_REAL_32      | MSG_TYPE_STRING | MSG_TYPE_PORT_NAME
    | MSG_TYPE_MOVE_RECEIVE | MSG_TYPE_MOVE_SEND | MSG_TYPE_MOVE_SEND_ONCE
    | MSG_TYPE_COPY_SEND    | MSG_TYPE_MAKE_SEND | MSG_TYPE_MAKE_SEND_ONCE
    | MSG_TYPE_POLYMORPHIC

  exception Msg_Field_Name
  val field_name_to_word: msg_field_name -> word
  val word_to_field_name: word -> msg_field_name

  datatype msg_field_header =
      MSG_FIELD_HEADER of {name: msg_field_name,
			   size: int,
			   number: int,
			   inline: bool,
			   longform: bool,
			   deallocate: bool}

  val field_header_size: msg_field_header -> int
  val field_size: msg_field_header -> int

  val get_header: buf -> msg_header
  val set_header: msg_header * buf -> unit
  val get_field_header: int * buf -> msg_field_header
  val set_field_header: msg_field_header * int * buf -> unit

  val get_field_headers: buf -> msg_field_header list
  val set_field_headers: msg_field_header list * buf -> unit
  datatype msg_option = 
      MSG_OPTION_NONE | MSG_OPTION_SEND | MSG_OPTION_RCV | 
      MSG_OPTION_SEND_TIMEOUT | MSG_OPTION_SEND_NOTIFY | 
      MSG_OPTION_SEND_CANCEL | MSG_OPTION_SEND_INTERRUPT |
      MSG_OPTION_RCV_INTERRUPT | MSG_OPTION_RCV_TIMEOUT | 
      MSG_OPTION_RCV_NOTIFY | MSG_OPTION_RCV_LARGE

  exception Mach_Error of string
  val task_self: unit -> port
  val port_allocate: port * port_right -> port
  val port_insert_right: port * port * port * msg_header_right -> unit
  val mach_msg: buf * msg_option list * int * int * port * int * port -> unit
  val port_null: port
 end


signature MIG_BASE =
 sig
  eqtype word
  type port

  exception MIG_ARRAY_TO_LARGE
  exception MIG_SERVER_EXCEPTION
  exception MIG_NO_REPLY
  exception MIG_BAD_ARGUMENTS
  exception MIG_BAD_ID
  exception MIG_REMOTE_ERROR
  exception MIG_REPLY_MISMATCH
  exception MIG_TYPE_ERROR

  val mig_return: (word -> unit) * word -> unit
  val get_reply_port: unit -> port
 end

