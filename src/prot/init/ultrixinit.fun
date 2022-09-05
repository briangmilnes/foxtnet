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
	
$Log: ultrixinit.fun,v $
Revision 1.4  1996/07/25  17:13:11  cline
*** empty log message ***

Revision 1.3  1996/07/22  17:48:53  cline
added IntiFile support

Revision 1.2  1996/05/08  02:01:48  esb
tested for the case where an input line has the wrong format.

Revision 1.1  1996/04/22  16:11:11  esb
Initial revision

# Revision 1.2  1996/04/18  21:33:55  cline
# Complete revision based on OSF1_init.fun
#
# Revision 1.1  1996/04/15  20:35:29  cline
# Initial revision
#

	1.	structure Constants
*)
structure Constants =
  struct

    (* init_file is only supported on ultrix for now *)
    val init_file_name = "/etc/foxnet"

    val ip_protocol = Word16.fromInt 0x800
    val arp_protocol = Word16.fromInt 0x806

    val icmp_protocol = 0wx01:word8
    val tcp_protocol =  0wx06:word8
    val udp_protocol =  0wx11:word8

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
    val etc = "/etc/"

    val rc = etc ^ "rc"
    val hostname = etc ^ "hostname"
    val resolv_conf = etc ^ "resolv.conf"
    val hosts = etc ^ "hosts"

    exception Initialize

    structure Trace = Trace (structure V = V
			     val makestring = fn _ => NONE
			     val module_name = "untrixinit.fun"
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

	fun search_hosts name =
	     let val instream = V.IO.open_in hosts
	         fun compare [] = false
		   | compare (n::rest) = n=name orelse compare rest
		 fun continue () =
		      if V.IO.end_of_stream instream then
		       raise Not_Found (hosts, name)
		      else loop ()
	         and loop () =
	              case V.String.tokens V.Char.isSpace
                              (V.IO.input_line instream) of
		         (ip :: names) =>
		          if compare names then (V.IO.close_in instream; ip)
		          else continue ()
		       | _ => continue ()
	     in case Ip_Host_Id.parse (loop ()) of
	           SOME ip => ip
	         | NONE => raise Not_Found (hosts,
				            name ^ "(cannot parse ip number)")
	     end

      end

    structure Init_File =
      struct
	val suffix = "_" ^ Int.toString interface_number ^ ":"

	fun get_field line =
	  let val SOME colon = V.String.index (":", line, 0)
	      fun is_space i = V.Char.isSpace (V.String.ordof (line, i))
	      fun skip_whitespace (i, j) =
		if is_space i then
		  skip_whitespace (i+1, j)
		else if is_space (j-1) then
		  skip_whitespace (i, j-1)
		else (i, j)
	      val (first, last) = skip_whitespace (colon+1,
						   V.String.length line)
	  in
	    V.String.substring (line, first, last - first)
	  end

	fun field proto name =
	  SOME (get_field
		(Read.findline (Constants.init_file_name,
				proto ^ "." ^ name ^ suffix)))
	  handle _ => NONE

	fun fields proto name =
	  (case Read.findlines (Constants.init_file_name,
				proto ^ "." ^ name ^ suffix) of
	     [] => NONE
	   | lines => SOME (map get_field lines))
	  handle _ => NONE
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

    fun either (SOME x) y = x
      | either NONE y = y

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
	val i_field = Init_File.field "Dev"
	fun interface () =
	  either (i_field "interface")
	    (protect ("Dev",
		      fn () => (V.List.nth
				(V.String.tokens V.Char.isSpace
				 (Read.findline (rc, "/etc/ifconfig")), 1))))
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
	      (protect ("Eth", fn () => Device.session (Dev.interface (),
							session_fun)))
	  end
      end

(*
	5.	structure Ip
*)
    structure Ip =
      struct

	val i_field  = Init_File.field "IP"
	val i_fields = Init_File.fields "IP"

	fun trim s =
	  let
	    val len = V.String.length s
	  in
	    if len=0 orelse V.String.ordof (s, len-1) <> #"\n"
	      then s
	    else V.String.substring (s, 0, len-1)
	  end

	fun local_hostname () =
	  protect ("Ip", fn () => trim (Read.findline (hostname, "")))

	fun setup () =
	  let
	    fun parse_hostname name =
	      case hostname_to_ip name of
		SOME ip => ip
	      | NONE => (case Ip_Host_Id.parse name of
			   SOME ip => ip
			 | NONE => Read.search_hosts name)

	    (* extract mask from ifconfig args *)
	    fun parse_mask m = (case Ip_Host_Id.parse m of
				  SOME addr => SOME (addr,{serve=false})
				| NONE => NONE)
	    fun get_mask [] = NONE
	      | get_mask ("netmask"::mask::_) = parse_mask mask
	      | get_mask (_::rest) = get_mask rest

	    (* extract mtu from ifconfig args *)
	    fun get_mtu [] = NONE
	      | get_mtu ("ipmtu"::mtu::_) = Int.fromString mtu
	      | get_mtu (_::rest) = get_mtu rest

fun mask_ms (addr, {serve}) = ("(" ^ Ip_Host_Id.makestring addr ^
			       ", {serve = " ^ Bool.toString serve ^ "})")
	    fun get_mask_mtu () =
	      let
		val ifc = (V.String.tokens V.Char.isSpace
			   (Read.findline (rc, "/etc/ifconfig")))
	      in
		(either (case i_field "mask" of
			   SOME mask => SOME (parse_mask mask)
			 | NONE => NONE)
		   (get_mask ifc) ,
		 either (case i_field "mtu"of
			   SOME i => SOME (Int.fromString i)
			 | NONE => NONE)
		   (get_mtu ifc))
	      end

	    fun local_address () =
	      parse_hostname (either (i_field "hostname") (local_hostname ()))

	    fun gateways () =
	      map parse_hostname
	        (either (i_fields "gateway")
		   [V.List.nth (V.String.tokens V.Char.isSpace
				(Read.findline (rc, "/etc/route add default")),
				3)])
	  in
	    protect ("IP",
		     fn () => let val (mask, mtu) = get_mask_mtu ()
			      in [{local_id = local_address(),
				   interface = Dev.interface (),
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
	val i_field  = Init_File.field "DNS"
	val i_fields = Init_File.fields "DNS"

	fun domain_list () =
	  (either (i_fields "domain")
	   (protect ("DNS",
		     fn () => [V.List.nth
			       (String.tokens Char.isSpace
				(Read.findline (resolv_conf, "domain")), 1)])))
	  @ ["."]

	fun servers () =
	  let
	    fun extract_server s =
	      V.List.nth (V.String.tokens Char.isSpace s, 1)
	  in
	    map parse_host_id
	        (either (i_fields "nameserver")
		 (protect
		  ("DNS", fn () => (V.List.map extract_server
				    (Read.findlines (resolv_conf,
						     "nameserver"))))))
	  end

	fun setup t = {domain_list = domain_list () ,
		       servers = servers (),
		       transport = t}
      end

  end
