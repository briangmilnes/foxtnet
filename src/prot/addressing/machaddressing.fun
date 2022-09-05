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
     the Mach Operating System.
     

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Mach_Addressing
	2.	datatype declarations
	3.	parsing the files
	4.	fetch


		iii.	RCS Log
	
$Log: machaddressing.fun,v $
Revision 1.6  1995/03/07  23:47:10  esb
took out the structure declaration.

Revision 1.5  1995/02/13  23:30:14  esb
adapted to 1.07.

Revision 1.4  1995/02/04  20:39:28  robby
updated to 107

Revision 1.3  1994/11/10  21:53:39  esb
added some changes that had been added to osfaddressing.fun

Revision 1.2  1994/11/01  16:05:11  milnes
Updated the table of contents.

Revision 1.1  1994/10/19  23:18:06  milnes
Initial revision

		1.	functor Mach_Addressing
*)

functor Mach_Addressing (structure B: FOX_BASIS
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
   val repeat1 = Link_Parser.Parser.repeat1

   val pint = Link_Parser.Parsing_Utils.pint
   val literals = Link_Parser.Parsing_Utils.literals
   val id = Link_Parser.Parsing_Utils.id
   val is_digit = Link_Parser.Parsing_Utils.is_digit
   val is_alphabetic = Link_Parser.Parsing_Utils.is_alphabetic

  in (* local *)

(*
		2.	datatype declarations
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
		3.	parsing the files
*)

   fun flatten [] = []
     | flatten (head :: rest) = head @ (flatten rest)

   val lex_ip_as_string =
          (pint && (literals [#"."]) && pint && (literals [#"."]) &&
	   pint && (literals [#"."]) && pint)
           wth flatten o Link_Parser.Parsing_Utils.list7

   val word32 = pint wth FoxWord32.intToWord o
                         Link_Parser.Parsing_Utils.pint_to_int

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
	 (repeat1 (any suchthat (is_alphabetic cor is_digit cor is_dot))))
	wth B.V.String.implode o op::

   val lex_interface = id 

(* /etc/gateways: net 0.0.0.0 gateway gw.cs.cmu.edu metric 1 passive *)

   val gateways_entry =
        (literals [#"n", #"e", #"t"]) &-& lex_ip_as_word32 &-&
	(literals [#"g", #"a", #"t", #"e", #"w", #"a", #"y"]) &-&
	lex_hostname &-& 
	(literals [#"m", #"e", #"t", #"r", #"i", #"c"]) &-&
	(pint wth FoxWord32.intToWord o Link_Parser.Parsing_Utils.pint_to_int) 
	wth (fn (_, (mask, (_, (hn, (_, metric))))) => (mask, hn, metric))

(* /etc/hosts: 128.2.102.42  alpha4.speech.cs.cmu.edu alpha4 *)

   val hosts_entry = (lex_ip_as_string &-& lex_hostname &-&
		      (Link_Parser.Parser.opt lex_hostname))

(* /etc/NETWORKS: 128.2.206.165 on SE0 *)

   val networks_entry = (lex_ip_as_string &-& (literals [#"o", #"n"]) &-&
			 lex_interface)
                        wth (fn (ip, (_, interface)) =>
			       (ip, implode interface))

   fun parse_etc_gateways () =
        ((Link_Parser.Parser.parse gateways_entry o
	  Link_Parser.Parser.Position.markstream o
	  Link_Parser.Input.readfile) "/etc/gateways");

   fun parse_etc_networks () =
        ((Link_Parser.Parser.parse networks_entry o
	  Link_Parser.Parser.Position.markstream o
	  Link_Parser.Input.readfile) "/etc/NETWORKS")

   fun first_two_words input =
        let fun word ([], #" " :: rest) = word ([], rest)
	      | word ([], #"\t" :: rest) = word ([], rest)
	      | word (l,  #" " :: rest) = (implode (rev l), rest)
	      | word (l,  #"\t" :: rest) = (implode (rev l), rest)
	      | word (l,  #"#" :: rest) = (implode (rev l), #"#" :: rest)
	      | word (l,  #"\n" :: rest) = (implode (rev l), [])
	      | word (l,  c :: rest) = word (c :: l, rest)
	      | word (l, []) = (implode (rev l), [])
	    val (w1, rest) = word ([], explode input)
	    val (w2, rest) = word ([], rest)
	in (w1, w2)
	end 

   fun parse_etc_hosts (host_ip, gateway_name) =
        let val parse_host_line = first_two_words 
	    val etc_hosts = open_in "/etc/hosts"
	    val host_name = ref NONE
	    val gateway_ip = ref NONE
	    fun done () =
	        (case (!host_name,!gateway_ip) of
		    (SOME _, SOME _) => true
		  | _ => false)
	    fun parse_lines () =
	         let val (ip,name) = parse_host_line (input_line etc_hosts)
		 in if ip = host_ip then host_name := SOME name else ();
		    if name = gateway_name then gateway_ip := SOME ip else ();
		    if done () then (!host_name,!gateway_ip)
		    else parse_lines ()
		 end
	in parse_lines ()
	end 

   exception Bad_Initialization of string

   fun ip_string_to_word32 ip =
        (case ((Link_Parser.Parser.parse lex_ip_as_word32 o
		Link_Parser.Parser.Position.markstream o
		Link_Parser.Input.readstring) ip) of 
	    SOME b => b
	  | NONE => 
	     raise (Bad_Initialization
		    "/etc/hosts failed to provided a bad ip number"))

(*
		4.	fetch
*)

   fun get_addressing () =
        (case parse_etc_networks () of
	    NONE => raise (Bad_Initialization "/etc/networks failed to parse")
	  | SOME (ip, interface) =>
	     (case parse_etc_gateways () of 
	         NONE =>
		  raise (Bad_Initialization "/etc/gateways failed to parse")
	       | SOME (mask, gateway_name, hop) =>
		  (case parse_etc_hosts (implode ip, gateway_name) of
		      (SOME host_name, SOME gateway_ip) =>
		       let val gate_ip_word = ip_string_to_word32 gateway_ip
		           val gateways = [Gateway {name = gateway_name,
						    ip = gate_ip_word}]
			   val own_ip = ip_string_to_word32 (implode ip)
			   val interfaces = [Interface
					     {name = interface,
					      mask = SOME mask,
					      ip = own_ip,
					      gateways = gateways}]
		       in Host {name = host_name, interfaces = interfaces}
		       end
		    | (NONE, _) =>
		       raise (Bad_Initialization
			      "/etc/hosts failed to provide local host name")
		    | (_, NONE) => 
		       raise (Bad_Initialization
			      "/etc/hosts failed to provide gateway ip"))))

  end (* local *)
 end (* struct *)



