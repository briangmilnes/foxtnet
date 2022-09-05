(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	Access to addressing information for the machine executing the
	program.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	structure Constants
	2.	functor Initialize
	3.	structure Dev
	4.	structure Eth
	5.	structure Ip
	6.	structures Udp and Tcp
	7.	structure Dns

		iii.	RCS Log
	
$Log: osf1init.fun,v $
Revision 1.4  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.3  97/03/06  15:47:18  cline
Added error checking for failure cases.

Revision 1.2  1996/07/22  17:47:27  cline
fixed module name for Trace.

Revision 1.1  1996/04/22  16:11:11  esb
Initial revision

# Revision 1.1  1996/04/11  16:45:11  cline
# Initial revision
#
Revision 1.4  1996/03/05  20:18:04  cline
added dns_port, fixed Dns.domain_list, improved Dns.setup

Revision 1.3  1996/03/04  21:47:36  esb
made TCP and UDP initialization same as IP.

Revision 1.2  1996/03/04  21:32:45  esb
fixed a minor bug.

Revision 1.1  1996/02/26  15:23:34  cline
Initial revision


	1.	structure Constants
*)

structure Constants =
  struct

    val xmeter_pathname = "/afs/cs/project/fox/foxnet/bin/xmeter"

    val ip_protocol = Word16.fromInt 0x800
    val arp_protocol = Word16.fromInt 0x806

    val icmp_protocol = 0wx01:Word8.word
    val tcp_protocol =  0wx06:Word8.word
    val udp_protocol =  0wx11:Word8.word

    val dns_port = Word16.fromInt 53

    structure TCP =
      struct
	val initial_window_size = 4096
	val user_timeout = 10000
      end
  end
  

(*
	2.	functor Initialize
*)

functor Initialize (structure Device: RAW_DEVICE
		    structure V: VENDOR
		    val hostname_to_ip: string -> Ip_Host_Id.T option
		    val interface_number: int) =
  struct

    val rc_config = "/etc/rc.config"
    val resolv_conf = "/etc/resolv.conf"
    val routes = "/etc/routes"

    exception Initialize

    structure Trace = Trace (structure V = V
			     val makestring = fn _ => NONE
			     val module_name = "osf1init.fun"
			     val debug_level = NONE)

    structure Read =
      struct

	exception Not_Found of string*string

	fun find (filename, pattern, add, base) =
	  let
	    val instream = V.IO.open_in filename
	    fun loop x =
		 let
		   val s = V.IO.input_line instream
		   fun compare (_, []) = true
		     | compare ([], _) = false
		     | compare ((hd0:char)::tl0, hd1::tl1) =
		         hd0=hd1 andalso compare (tl0, tl1)
		 in
		   if compare (explode s, explode pattern)
		     then loop (add (s, x))
		          handle x => (V.IO.close_in instream; raise x)
		   else if V.IO.end_of_stream instream
		     then (V.IO.close_in instream; x)
		   else loop x
		end
	  in
	    loop base
	  end

	fun findline (filename, pattern) =
	  let exception Found of string
	  in
	    (find (filename, pattern, fn (s, ()) => raise Found s, ());
	     raise Not_Found (filename, pattern))
	    handle Found s => s
	  end

	fun findlines (filename, pattern) =
	  V.List.reverse (find (filename, pattern, op::, []))

      end

    fun protect (prot, f) =
      (f ()) handle
        Read.Not_Found (filename, pattern) =>
	  (Trace.local_print
	   ("Cannot find `" ^ pattern ^ "' in " ^ filename ^
	    " while constructing " ^ prot ^ " setup.");
	   Trace.print_raise (Initialize, NONE))
      | x => (Trace.print_handled (x, NONE);
	      Trace.local_print
	      ("Unexpected exception while constructing " ^ prot ^ " setup.");
	      Trace.print_raise (Initialize, NONE))

    fun get_quoted_field s =
      let fun isQuote c = c = #"\"" (* emacs fodder follows *) before "\""
      in V.List.nth (V.String.fields isQuote s, 1)
      end

    fun parse_host_id s =
      let
	exception Empty
      in
	case Ip_Host_Id.parse s of
	  SOME addr => addr
	| NONE => raise Empty
      end
	  
(*
	3.	structure Dev
*)
    structure Dev =
      struct
	fun interface () =
	  let
	    val pattern = "NETDEV_" ^ Int.toString interface_number ^ "="
	    val l = Read.findline (rc_config, pattern)
	  in
	    protect ("Dev", fn () => get_quoted_field l)
	  end
      end

(*
	4.	structure Eth
*)
    structure Eth =
      struct
	fun local_address () =
	  let
	    fun session_fun ({local_address, ...}:Device.session) =
	      (fn () => local_address, fn _ => ())
	  in
	    protect ("Eth", fn () => Device.session (Dev.interface (),
						     session_fun))
	  end
      end

(*
	5.	structure Ip
*)
    structure Ip =
      struct

        local
	 fun getname () =
	      ((get_quoted_field (Read.findline (rc_config,
						 "FOXNET_HOSTNAME=")))
	       handle _ => get_quoted_field (Read.findline
					     (rc_config, "HOSTNAME=")))
	in
	 fun local_hostname () = protect ("Ip (local_hostname)", getname)
	end

	fun setup () =
	  let
	    (* extract mask from ifconfig args *)
	    fun get_mask [] = NONE
	      | get_mask ("netmask"::mask::_) =
	          (case Ip_Host_Id.parse mask of
		     SOME addr => SOME (addr,{serve=false})
		   | NONE => NONE)
	      | get_mask (_::rest) = get_mask rest

	    (* extract mtu from ifconfig args *)
	    fun get_mtu [] = NONE
	      | get_mtu ("ipmtu"::mtu::_) = Int.fromString mtu
	      | get_mtu (_::rest) = get_mtu rest

	    val ifconfig_pattern = ("IFCONFIG_" ^
				    Int.toString interface_number ^ "=")

	    fun get_ifconfig_line pat =
	          V.String.tokens V.Char.isSpace
	            (get_quoted_field (Read.findline (rc_config, pat)))

	    fun get_std_info () =
	      let
		val ifc = get_ifconfig_line ifconfig_pattern
		val host = local_hostname ()
		val ip = case hostname_to_ip (host) of
		           SOME x => x
			 | NONE => raise
			     Read.Not_Found ("function hostname_to_ip", host)
	      in
		(ip, get_mask ifc, get_mtu ifc)
	      end

	    fun get_info () =
	      let
		val ifc = get_ifconfig_line ("FOXNET_"^ifconfig_pattern)
	      in
		(parse_host_id (hd ifc), get_mask ifc, get_mtu ifc)
	      end
	    handle Read.Not_Found _ => get_std_info ()
		 | x => raise x

	    val gateways_msg = "Can't read default gateway from /etc/routes"
	    fun gateways () =
	      [parse_host_id
	         (V.List.nth (V.String.tokens V.Char.isSpace
			        (Read.findline (routes, "default")), 1))]
	      handle x => Trace.print_raise_again (x, SOME gateways_msg)

	    val interface_msg = "Can't get interface - check that the " ^
				"packetfilter is properly configured"
	    fun get_interface () =
	      Dev.interface () handle x =>
		Trace.print_raise_again (x, SOME interface_msg)

	    fun get_info_failure () =
	      (print "error getting ifconfig info - check interface number\n";
	       Trace.print_raise (Initialize, SOME "setup"))
	  in
	    protect ("IP",
		     fn () => let val (local_address, mask, mtu) = get_info ()
		                  handle _ => get_info_failure ()
			      in [{local_id = local_address,
				   interface = get_interface (),
				   gateways = gateways (),
				   mask = mask, mtu = mtu}]
			      end)
	  end
      end

(*
	6.	structures Udp and Tcp
*)
    structure Udp = Ip
    structure Tcp = Ip

(*
	7.	structure Dns
*)
    structure Dns =
      struct
	fun domain_list () = tl (String.tokens Char.isSpace
				 (Read.findline (resolv_conf, "search")))
	fun servers () =
	  let
	    fun extract_server s =
	      parse_host_id (V.List.nth (V.String.tokens Char.isSpace s, 1))
	  in
	    protect
	      ("DNS", fn () => (V.List.map extract_server
				(Read.findlines (resolv_conf, "nameserver"))))
	  end

	fun setup t = {domain_list = domain_list (),
		       servers = servers (),
		       transport = t}
      end

  end
