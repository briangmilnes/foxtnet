(*
$Log: mach_portUser.sig,v $
Revision 1.2  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.1  1994/10/20  17:56:54  cline
Initial revision

Revision 1.3  1994/03/07  14:52:25  esb
commented out unused functions.

Revision 1.2  94/03/01  22:00:12  esb
added RCS log.

To speed up compilation, I've commented out most of these functions.
Feel free to add back in any that you need to use.
*)

signature MACH_PORT =
 sig
  eqtype word
  type buf
  type port
  type msg_field_name
  exception Mach_Port of word

  val mach_port_allocate : port * word -> port
  val mach_port_destroy : port * port -> unit
  val mach_port_set_qlimit : port * port * word -> unit
 end
