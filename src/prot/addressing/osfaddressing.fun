(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract
	
	A functor for gathering hostname/ip number/gateway ip from
     the OSF Operating System's etc files.
     

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	1.	structure OS_Addressing
	iii.	RCS Log
	2.	functor OSF_Addressing
	3.	datatype declarations
	4.	parsing the file
	5.	get_addressing
        3.	get_addressing
	1.	structure OS_Addressing

		iii.	RCS Log
	
$Log: osfaddressing.fun,v $
Revision 1.6  1995/03/07  23:47:10  esb
took out the structure declaration.

Revision 1.5  1995/02/13  23:30:14  esb
adapted to 1.07.

Revision 1.4  1995/02/04  20:39:28  robby
updated to 107

Revision 1.3  1994/11/10  16:13:48  milnes
Fixe to file paths and some other simple things.

Revision 1.2  1994/10/20  21:34:00  milnes
Fixed a small type error.

Revision 1.1  1994/10/19  23:18:06  milnes
Initial revision

		2.	functor OSF_Addressing
*)

functor Osf_Addressing (structure B: FOX_BASIS
                        structure AT: ADDRESSING_TYPES
			structure Link_Parser: LINK_PARSER): OS_ADDRESSING =
 struct

  local

   val -- = Link_Parser.Parser.--
   infix 2 --
   val ## = Link_Parser.Parser.##
   infix 2 ##
   val && = Link_Parser.Parser.&&
   infixr 3 &&
   val wth = Link_Parser.Parser.wth
   infix 2 wth
   val suchthat = Link_Parser.Parser.suchthat
   infix 2 suchthat
   val return = Link_Parser.Parser.return
   infix 2 return
   val guard = Link_Parser.Parser.guard
   infix 2 guard
   val || = Link_Parser.Parser.||
   infixr 1 ||

   val cor = Link_Parser.Parsing_Utils.cor
   infixr 4 cor 
   val cand = Link_Parser.Parsing_Utils.cand
   infixr 4 cand 
   val &-& = Link_Parser.Parsing_Utils.&-&
   infixr 3 &-&

   val literal = Link_Parser.Parser.literal
   val first = Link_Parser.Parser.first
   val any = Link_Parser.Parser.any
   val opt = Link_Parser.Parser.opt
   val repeat1 = Link_Parser.Parser.repeat1
   val parse = Link_Parser.Parser.parse
   val transform = Link_Parser.Parser.transform

   val pint = Link_Parser.Parsing_Utils.pint
   val pint_to_int = Link_Parser.Parsing_Utils.pint_to_int
   val literals = Link_Parser.Parsing_Utils.literals
   val id = Link_Parser.Parsing_Utils.id
   val uptoeol = Link_Parser.Parsing_Utils.uptoeol
   val sharp_comment = Link_Parser.Parsing_Utils.sharp_comment
   val is_digit = Link_Parser.Parsing_Utils.is_digit
   val is_alphabetic = Link_Parser.Parsing_Utils.is_alphabetic

  in (* local *)

(*
		3.	datatype declarations
*)

   open AT
  (* datatype gateway = Gateway of {name: string, ip: FoxWord32.word}
     datatype interface = 
       Interface of {name: string, ip: FoxWord32.word, 
                     mask: FoxWord32.word option,
                     gateways: gateway list}
     datatype host = Host of {name: string, interfaces: interface list}
     datatype alias = Alias of {name: string option, host: host}
     exception Bad_Initialization of string
   *)

(*
local  
 open Link_Parser
 open Parser
 open Parsing_Utils
 infix  2 -- ##
 infixr 3 &&
 infix  2 wth suchthat return guard
 infixr 1 ||
 infixr 4 cor 
 infixr 4 cand 
 infixr 3 &-&
 val explode=B.V.String.explode
in
*)

(*
		4.	parsing the file

 OSF gets its addressing information from three sources;

 rc.config (* HOSTNAME="inbox.fox.cs.cmu.edu"
              NUM_NETCONFIG="1"
              IFCONFIG_0="128.2.198.48 netmask 255.255.0.0"
              and each network interface seems to be named
              "le"<number>. 
            *)

 routes    (* default 128.2.254.36 *) 
 
 hosts     (* /etc/hosts: 128.2.198.48 inbox.fox.cs.cmu.edu *)

The algorithm used here is to parse rc.config for the HOSTNAME, the
NUM_NETCONFIG and the ifconfig. Get the gateway's address from routes
(and use it for all interfaces) and then ignore /etc/hosts which is likely
not to contain its name anyway.
*)

 val lex_ip_as_string =
    (pint && (literals [#"."]) && pint && (literals [#"."]) && 
     pint && (literals [#"."]) && pint) wth 
    (fn (s1,(c2,(s3,(c4,(s5,(c6,s7)))))) =>
     s1 @ c2 @ s3 @ c4 @ s5 @ c6 @ s7)

 val word32 = pint wth FoxWord32.intToWord o pint_to_int 

 val lex_ip_as_word32 =
   ((first word32 (literal #".")) && (first word32 (literal #".")) &&
    (first word32 (literal #".")) && word32)
     wth (fn (b0,(b1,(b2,b3))) => 
          FoxWord32.orb(FoxWord32.lshift(b0,24),
             FoxWord32.orb(FoxWord32.lshift(b1,16),
                 FoxWord32.orb(FoxWord32.lshift(b2,8), b3))))
   
 fun is_dot c = (c = #".")

 val lex_hostname =
   ((any suchthat is_alphabetic) &&
    (repeat1 (any suchthat (is_alphabetic cor is_digit cor is_dot)))) wth implode o op::

 datatype rcconfig = Hostname of string | 
                     Max_Netdevs of int | 
                     Ifconfig of string * FoxWord32.word * FoxWord32.word

 val HOSTNAME = (literals (explode "HOSTNAME=")) &-& 
		 (literal #"\"") && lex_hostname && (literal #"\"")
		 wth (fn (_,(_,(hn,_))) => SOME (Hostname hn))

 val MAX_NETDEVS = (literals (explode "MAX_NETDEVS=")) &-& 
		    (literal #"\"") && (pint wth pint_to_int) && (literal #"\"")
		    wth (fn (_,(_,(i,_))) => SOME (Max_Netdevs i))

 val IF_CONFIG = (literals (explode "IFCONFIG_"))
                  && pint && (literal #"=") &-&
		  (literal #"\"") &-& lex_ip_as_word32 &-& 
		   (literals (explode "netmask")) &-& 
		   lex_ip_as_word32 &-& 
		   (literal #"\"")
		    wth (fn (_,(N,(_,(_,(ip,(_,(mask,_))))))) => 
			 SOME (Ifconfig (implode N, ip, mask)))
 
 val rcconfig = HOSTNAME || MAX_NETDEVS || IF_CONFIG || 
                sharp_comment wth (fn _ => NONE) || uptoeol wth (fn _ => NONE)
                 
 fun remove_NONEs option_list =
   fold (fn (NONE,l) => l | (O as SOME a,l) => O :: l) option_list []

 fun parse_etc_rcconfig () = 
  remove_NONEs (Stream.to_list (((transform rcconfig) o 
				 Link_Parser.Position.markstream o
				 Link_Parser.Input.readfile) 
				"/etc/rc.config"))

(*
  This could be more robust by really knowing the format of an /etc/routes
 file that contains more than a default route.
*)                             

 val routes = ((literals (explode "default"))
		&-& lex_ip_as_word32) wth (fn (_,i) => i)

 fun parse_etc_routes () = ((parse routes) o Link_Parser.Position.markstream o
			    Link_Parser.Input.readfile) "/etc/routes"

 val hosts_entry = (lex_ip_as_string &-& lex_hostname &-& (opt lex_hostname))

 val hosts = hosts_entry wth SOME || 
             sharp_comment wth (fn _ => NONE) || uptoeol wth (fn _ => NONE)

 fun parse_etc_hosts () = 
  remove_NONEs 
   (Stream.to_list (((transform hosts) o
		     Link_Parser.Position.markstream o
		     Link_Parser.Input.readfile) 
   "/etc/hosts"))

(*
		5.	get_addressing
*)

 fun get_addressing () =
 let
   val rcconfig = parse_etc_rcconfig ()

   fun find_hostname [] = NONE
     | find_hostname (NONE :: rest)  = find_hostname rest
     | find_hostname ((SOME (Hostname name)) :: rest)  = SOME name
     | find_hostname ((SOME l) :: rest)  = find_hostname rest

   fun find_if_configs [] = []
     | find_if_configs (NONE :: rest) = find_if_configs rest
     | find_if_configs ((SOME (Ifconfig s)) :: rest) =
            s :: (find_if_configs rest)
     | find_if_configs ((SOME l) :: rest) = find_if_configs rest
  in
    case find_hostname rcconfig of
      NONE => 
        raise (Bad_Initialization 
                  "could not find host name in /etc/rcconfig")
    | SOME hostname =>
      (case find_if_configs rcconfig of
         [] => 
          raise (Bad_Initialization "could not if configs /etc/rcconfig")
       | ifconfigs =>
        (case parse_etc_routes () of
           NONE => 
            raise (Bad_Initialization
                    "could not find default gateway address in /etc/routes")
         | SOME gateway_ip =>
           let
            fun build_interface (n,ip,mask) =
             Interface{name = "le" ^ n, ip = ip, mask = SOME mask,
                       gateways = [Gateway{name="",ip=gateway_ip}]}
           in
             Host{name=hostname,
                  interfaces = (map build_interface ifconfigs)}
           end))
   end 
  end (* local *)
 end (* struct *)



