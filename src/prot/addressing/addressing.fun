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

	A functor to allow the user to hand set or read addressing information
    from files.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor addressing 
	2.	datatype declarations
	3.	datatype declarations
	4.	lexer
	5.	initialize

		iii.	RCS Log
	
$Log: addressing.fun,v $
Revision 1.8  1995/03/12  17:56:08  esb
adapted to new trace.sig.

Revision 1.7  1995/03/10  03:50:39  esb
adapted to new vendor.sig.

Revision 1.6  1995/03/07  23:46:44  esb
made AT a substructure.

Revision 1.5  1995/02/13  23:30:14  esb
adapted to 1.07.

Revision 1.4  1995/02/04  20:39:28  robby
updated to 107

Revision 1.3  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.2  1994/11/10  16:13:48  milnes
Fixe to file paths and some other simple things.

Revision 1.1  1994/10/19  23:18:06  milnes
Initial revision


		1.	functor addressing 
*)

functor Addressing (structure B: FOX_BASIS
		    structure Addressing_Types: ADDRESSING_TYPES
		    structure Os_Addressing: OS_ADDRESSING
		    structure Link_Parser: LINK_PARSER
		     sharing type Os_Addressing.host = Addressing_Types.host
			 and type Os_Addressing.alias = Addressing_Types.alias
                         and type Os_Addressing.interface
                                = Addressing_Types.interface
                         and type Os_Addressing.gateway
                                = Addressing_Types.gateway
		    val debug_level: int ref option): ADDRESSING =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "addressing.fun")
  val trace_print = Trace.trace_print

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
   val !! = Link_Parser.Parser.!!

   val cor = Link_Parser.Parsing_Utils.cor
   infixr 4 cor 
   val cand = Link_Parser.Parsing_Utils.cand
   infixr 4 cand 
   val &-& = Link_Parser.Parsing_Utils.&-&
   infixr 3 &-&
   val sem = Link_Parser.Parsing_Utils.sem
   infixr 2 sem

   val literal = Link_Parser.Parser.literal
   val first = Link_Parser.Parser.first
   val second = Link_Parser.Parser.second
   val middle = Link_Parser.Parser.middle
   val third = Link_Parser.Parser.third
   val done = Link_Parser.Parser.done
   val parse = Link_Parser.Parser.parse
   val any = Link_Parser.Parser.any
   val transform = Link_Parser.Parser.transform
   val repeat1 = Link_Parser.Parser.repeat1
   val separate0 = Link_Parser.Parser.separate0

   val pint = Link_Parser.Parsing_Utils.pint
   val literals = Link_Parser.Parsing_Utils.literals
   val id = Link_Parser.Parsing_Utils.id
   val is_digit = Link_Parser.Parsing_Utils.is_digit
   val is_alphabetic = Link_Parser.Parsing_Utils.is_alphabetic
   val whitespace = Link_Parser.Parsing_Utils.whitespace
   val string = Link_Parser.Parsing_Utils.string

(*
		2.	datatype declarations
*)

   open Addressing_Types
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
		3.	timing
*)

  fun start_timing () =
       if Trace.trace_on () then B.Time.time_of_day () else B.Time.zero_time

  fun end_timing (start) = 
       if Trace.trace_on () then
	B.Time.makestring (B.Time.- (B.Time.time_of_day (), start))
       else ""

  in (* local *)

   datatype source =
          (* Look up a file to read from the shell's environment
             variable of this name. *)
        EnvVar of string              
          (* Read from a file. *)
      | File of string                
          (* Use the operating system's default techniques, most
	     likely a variety of files in /etc. *)
      | Default            
          (* Use this provided host addressing. *)
      | Host_Value of host
          (* Use this host if the string option is NONE,
             otherwise only if the string is the OS's idea of
             the machine name. *)
      | Alternate of alias

  type addressing_path = source list

  val default =
     ref [EnvVar "FOXNET", 
          File "/etc/foxnet",
          File "/afs/cs/project/fox/foxnet/etc/foxnet",
          Default]

(*
	4.	lexer

  Combinatory parser via C. Okasaki and P. Lee from Hutton.

*)

  datatype lexeme = Open_Brace | Close_Brace | Comma | Equal | Open_List
                  | Close_List | LString of char list | LId of char list
                  | LFoxWord32 of FoxWord32.word | LSOME | LNONE

  val ten = SW.n32 "10"
  val zero = SW.n32 "0"

  local  
   fun digit_to_word32 d = FoxWord32.intToWord (B.V.Char.ord d - 
						B.V.Char.ord #"0")
   fun pint_to_word32 digits = 
        (revfold (fn (d, n) => FoxWord32.+ (FoxWord32.* (ten, n),
					    (digit_to_word32 d)))
	 digits zero)

   val word32 = pint wth pint_to_word32
   val lexer'' = literal #"{" return Open_Brace
              || literal #"}" return Close_Brace
              || literal #"," return Comma
              || literal #"=" return Equal
              || literal #"[" return Open_List
              || literal #"]" return Close_List
              || (literals (explode "SOME")) return LSOME
              || (literals (explode "NONE")) return LNONE
              || word32 wth LFoxWord32
              || string wth LString
              || id     wth LId
   val lexer' = second whitespace (!! lexer'')
  in (* local *)
   val lexer = transform lexer'
  end (* local *)

(*
       3.	Parser 
*)

  datatype token  = 
      TUbyte4 of FoxWord32.word | TString of string | 
      TId of string | TList of token list | 
      TNONE | TSOME of token | 
      Gat of gateway         |
      Int of interface       |
      Hos of host            |
      Ali of alias           | 
      Err of string

  fun parse_list item  = 
       (middle (literal Open_List) (separate0 item (literal Comma))
	(literal Close_List))
       wth TList
   
  exception Parser_Error of string

  fun pe (message, position) = 
       raise (Parser_Error (message ^ "location " ^
			    Link_Parser.Position.makestring position))

  fun lexeme_to_token (LString s) = TString (implode s)
    | lexeme_to_token (LId s) = TId (implode s)
    | lexeme_to_token (LFoxWord32 b) = TUbyte4 b
    | lexeme_to_token _ =
       raise (Parser_Error "parser error, asked to translate a bad lexeme")

  fun parse_field name value = 
       third (literal (LId (explode name))) (literal Equal)
       (value wth (fn v => (explode name, v)))

  fun parse_record parse_fields = 
       middle (literal Open_Brace) (separate0 parse_fields (literal Comma))
       (literal Close_Brace)

  fun parse_constructor name value = 
       second (literal (LId (explode name))) (value
					      wth (fn v => (explode name, v)))

  val parse_string = any suchthat (fn (LString _) => true | _ => false)
                         wth lexeme_to_token

  val parse_id = any suchthat (fn (LId _) => true | _ => false)
                      wth lexeme_to_token

  val parse_ubyte4 = any suchthat (fn (LFoxWord32 _) => true | _ => false)
                       wth lexeme_to_token

  fun find_field (name, extractor, P) [] = 
       pe ("no field name " ^ name ^ "found, parser error", P)
    | find_field (name, extractor, P) ((fname, fvalue) :: rest)= 
       if name = implode fname then
	((extractor fvalue) 
	 handle Match => 
                 pe ("type error field " ^ name ^ 
		     "does not contain a value of the required type", P))
       else find_field (name, extractor, P) rest

  fun find_list_field (name, extractor, P) [] =
       pe ("no field name " ^ name ^ "found, parser error",P)
    | find_list_field (name,extractor,P) ((fname, fvalue) :: rest) = 
       if name = implode fname then
	(case fvalue of
            TList l => 
             ((map extractor l)
               handle Match =>
               pe ("type error field " ^ name ^ 
                   "does not contain a value of the required type",
                   P))
          | _ => pe ("type error list field " ^ name ^ 
		     "does not contain a list of the required type",
		     P))
       else find_list_field (name, extractor, P) rest

  fun extract_gateway (([#"G", #"a", #"t", #"e", #"w", #"a", #"y"],
			l as [(f1_name, f1_value), (f2_name, f2_value)]), P) =
       (if f1_name = f2_name then
	 pe ("duplicate fields " ^ implode f1_name ^ " in Gateway record", P)
	else
	 SOME 
	  (Gat (Gateway 
		{name = find_field ("name",
				    fn TString name => name 
				     | _ => raise Match, P) l,
		 ip = find_field ("ip",
				  fn TUbyte4 b => b
				   | _ => raise Match, P) l})))
    | extract_gateway (_,P) = 
       (pe ("wrong number of fields in gateway record", P); NONE)

  val parse_gateway =
       (parse_constructor "Gateway" 
	(parse_record ((parse_field "name" parse_string) ||
		       (parse_field "ip" parse_ubyte4))))
       sem extract_gateway

  fun extract_interface (([#"I", #"n", #"t", #"e", #"r", #"f", #"a",
			   #"c", #"e"],
			  l as [(f1_name,f1_value),(f2_name,f2_value),
				(f3_name,f3_value),(f4_name,f4_value)]),
			 P) =
        (if f1_name = f2_name orelse f1_name = f3_name orelse 
	    f1_name = f4_name then
	  pe ("duplicate fields " ^ implode f1_name ^ "in Interface record", P)
	 else ();
	 if f2_name = f3_name orelse f2_name = f4_name then
	  pe ("duplicate fields " ^ implode f2_name ^ "in Interface record", P)
	 else ();
	 if f3_name = f4_name then
	  pe ("duplicate fields " ^ implode f3_name ^ "in Interface record", P)
	 else ();
	 let fun match_tstring (TString name) = name
	       | match_tstring _ = raise Match
	     fun match_tubyte4 (TUbyte4 b) = b
	       | match_tubyte4 _ = raise Match
	     fun match_tubyte4_opt TNONE = NONE
	       | match_tubyte4_opt (TSOME (TUbyte4 b)) = SOME b
	       | match_tubyte4_opt _ = raise Match
	     fun match_gate (Gat g) = g
	       | match_gate _ = raise Match
	 in SOME (Int (Interface 
		       {name = find_field ("name", match_tstring, P) l,
			mask = find_field ("mask", match_tubyte4_opt, P) l,
			ip = find_field ("ip", match_tubyte4, P) l,
			gateways =
			find_list_field ("gateways", match_gate, P) l}))
	 end)
    | extract_interface (_, P) =
       (pe ("wrong number of fields interface record", P); NONE )

  fun parse_option value = (second (literal LSOME) value) wth TSOME ||
                           (literal LNONE) return TNONE

  val parse_interface = 
       ((parse_constructor "Interface" 
	 (parse_record 
	  ((parse_field "name" parse_string) || 
	   (parse_field "ip" parse_ubyte4)   ||
	   (parse_field "mask" (parse_option parse_ubyte4)) ||
	   (parse_field "gateways" (parse_list parse_gateway))))))
       sem extract_interface

  fun extract_host (([#"H", #"o", #"s", #"t"],
		     l as [(f1_name, f1_value), (f2_name, f2_value)]), P) =
       (if f1_name = f2_name then
	 pe ("duplicate fields " ^ implode f1_name ^ "in Host record", P)
	else ();
	SOME (Hos (Host {name = 
			 find_field ("name",
				     fn TString name => name
				      | _ => raise Match, P) l,
			 interfaces = 
			 find_list_field ("interfaces",
					  fn Int g => g
					   | _ => raise Match, P) l})))
    | extract_host (_, P) = 
       (pe ("wrong number of fields in Host record", P); NONE)

val parse_host = 
  (parse_constructor "Host" 
       (parse_record 
        ((parse_field "name" parse_string) ||
         (parse_field "interfaces" (parse_list parse_interface)))))
   sem extract_host

fun extract_alias (([#"A", #"l", #"i", #"a", #"s"],
		    l as [(f1_name,f1_value),(f2_name,f2_value)]), P) =
 (if f1_name = f2_name 
  then  pe ("duplicate fields " ^ implode f1_name ^ "in Alias record", P)
  else ();
  SOME (
  Ali(Alias {name =
                  find_field ("name",fn (TSOME (TString name)) => SOME name 
                                      | TNONE => NONE 
                                      | _ => raise Match,
                                     P) l,
             host = 
                  find_field ("host",fn Hos g => g | _ => raise Match, P) l
                })))
| extract_alias (_,P) = 
  (pe ("wrong number of fields in Alias record", P); 
   NONE)

val parse_alias = 
 (parse_constructor "Alias" 
       (parse_record 
   ((parse_field "name" (parse_option parse_string)) ||
    (parse_field "host" parse_host))))
   sem extract_alias

val parse_addressing = parse_alias || parse_host 

val parse_addressings = first (repeat1 parse_addressing) (done ())

fun parse_from_string_using p  = 
 (parse p) o (lexer o (Link_Parser.Position.markstream o
		       Link_Parser.Input.readstring))

val lex_from_string = 
 lexer o (Link_Parser.Position.markstream o Link_Parser.Input.readstring)

val parse_from_string = 
 (parse parse_addressings) o
 (lexer o (Link_Parser.Position.markstream o Link_Parser.Input.readstring))

val parse_from_file  = 
 (parse parse_addressings) o
 (lexer o (Link_Parser.Position.markstream o Link_Parser.Input.readfile))

(*
	5.	initialize
*)


fun get_host_from_default () = 
     let val start = start_timing ()
         val returnv = SOME (Os_Addressing.get_addressing ())
     in trace_print (fn _ => "get_host_from_default time " ^
		     end_timing start);
        returnv
     end

(* 
 If the alias has a NONE name, then its host may be returned. If it
has a name, then only use it if it is the system's default idea of a
host.
*)

fun get_host_from_alias (Alias {name = NONE, host}) = SOME host
  | get_host_from_alias (Alias {name = SOME alias_name, host}) =
    (case get_host_from_default () of
       NONE => NONE
     | SOME (Host{name,...}) =>
        if name = alias_name
        then SOME host 
        else NONE)

fun get_host_from_file (file_name) =
 let
   val start = start_timing ()
   val addressings =
     (parse_from_file file_name)
      handle Io s => NONE
           | Parser_Error string =>
               (B.V.Print.print string;
                raise (Bad_Initialization "bad initialization file"))

   (* Search the returned addressings for a host, returning it if found. *)

   fun find_host ((Hos h) :: rest) = SOME h
     | find_host (_:: rest) = find_host rest
     | find_host [] = NONE

  val returnv = 
   (case addressings of  
      NONE => NONE
    | SOME addressings =>
      (case find_host addressings of 
         SOME h => SOME h
       | NONE =>
          let
            val default_host = get_host_from_default ()
             (* Search the returned addressings for an alias. If the alias has
               a NONE name, then its host may be returned. If it has a name,
               then only use it if it is the system's default idea of a host. *)
          
             fun find_alias (a as (Ali (Alias{name = NONE,host})) :: rest) = SOME host
               | find_alias (a as (Ali (Alias{name = SOME hostname,host})) :: rest) =
                  (case default_host of
                     NONE => find_alias rest
                   | SOME (Host{name,...}) => 
                      if name = hostname 
                      then SOME host 
                      else find_alias rest)
               | find_alias (_:: rest) = find_alias rest
               | find_alias [] = NONE
          in 
          (case find_alias addressings of 
              NONE => NONE
            | SOME a => SOME a)
          end))
 in 
  trace_print (fn _ =>
	       "get_host_from_file " ^ file_name ^ " time " ^
	       end_timing start);
  returnv 
 end

fun get_host_from_environment (var_name) =
 let 
   val start = start_timing ()
   val returnv =
    (case B.Environment.find var_name of 
      NONE => NONE 
    | SOME file_name => get_host_from_file file_name)
 in 
  trace_print (fn _ => "get_host_from_environment time " ^ (end_timing start));
  returnv 
 end 
 
fun get_source (EnvVar variable_name) = get_host_from_environment variable_name
  | get_source (File file_name) = get_host_from_file file_name
  | get_source Default = get_host_from_default ()
  | get_source (Host_Value host) = SOME host
  | get_source (Alternate alias) = get_host_from_alias alias

fun do_sources [] = NONE
  | do_sources (source :: rest) = 
    (case get_source source of
      NONE => do_sources rest
    | SOME host => SOME host)

val cache = ref NONE: host option ref

fun initialize addressing_path =
 (case do_sources addressing_path of
    NONE => (cache := NONE;
             raise (Bad_Initialization "ran out of sources"))
  | SOME host => cache := SOME host)

fun fetch () = !cache   

   structure AT = Addressing_Types
  end (* local *)
 end (* struct *)
