(*
$Log: deviceUser.sig,v $
Revision 1.5  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.4  1994/03/07  14:52:25  esb
commented out unused functions.

Revision 1.3  94/03/01  21:58:36  esb
added RCS log.

*)
signature DEVICE =
 sig
  eqtype word
  type buf
  type port
  type msg_field_name
  exception Device of word

  val device_open: port * word * string -> port
  val device_close: port -> unit

  val device_get_status: port * word * word -> (buf * int)
  val device_set_filter: port * (word * msg_field_name) * word * (buf * int)
                       -> unit

 end
