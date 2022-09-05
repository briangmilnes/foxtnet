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

	A test functor for the addressing parser.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Addressing 
	2.	structure Test_Addressing 

		iii.	RCS Log
	
$Log: addressing.tst,v $
Revision 1.2  1995/03/08  18:13:01  esb
partial fix, but still won't compile.

Revision 1.1  1994/10/19  23:18:06  milnes
Initial revision


		1.	functor Test_Addressing 

	Note: this does not work, since it is requires functions and
	values internal to the Addressing structure. It might be worth
	trying to get it to work.
*)


functor Test_Addressing (structure B: FOX_BASIS): TEST_STRUCTURE =
 struct
  structure Link_Parser = Link_Parser (structure V = B.V)

  structure AT =
   struct
    datatype gateway = Gateway of {name: string, ip: FoxWord32.word}
    datatype interface = 
     Interface of {name: string, ip: FoxWord32.word, 
		   mask: FoxWord32.word option,
		   gateways: gateway list}
    datatype host = Host of {name: string, interfaces: interface list}
    datatype alias = Alias of {name: string option, host: host}
    exception Bad_Initialization of string
   end (* struct *)

  structure Os_Addressing = Os_Addressing (structure B = B
					   structure AT = AT
					   structure Link_Parser = Link_Parser)

  structure Addressing = Addressing (structure B = B
				     structure Addressing_Types = AT
				     structure Os_Addressing = Os_Addressing
				     structure Link_Parser = Link_Parser
				     val debug_level = NONE)

(*
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
*)

(*
*)
  open Link_Parser.Parser
  infix  2 -- ##
  infixr 3 &&
  infix  2 wth suchthat return guard
  infixr 1 ||
  open Link_Parser.Position

  open Addressing

  val zero = SW.n32 "0"

  fun test_lex (s, l) () = Stream.to_list (lex_from_string s) = l

  fun test_lexer () =
       (B.Test.test
	("lex Host{name=\"sly.fox.cs.cmu.edu\",interfaces=[]}", 
	 test_lex ("Host{name=\"sly.fox.cs.cmu.edu\",interfaces=[]}",
		   [(LId "Host",
		     Between ({char = 1, line = 1},{char = 5, line = 1})),
		    (Open_Brace,
		     Between ({char = 5, line = 1},{char = 6, line = 1})),
		    (LId "name",
		     Between ({char = 6, line = 1},{char = 10, line = 1})),
		    (Equal,
		     Between ({char = 10, line = 1},{char = 11, line = 1})),
		    (LString "sly.fox.cs.cmu.edu",
		     Between ({char = 11, line = 1},{char = 31, line = 1})),
		    (Comma,
		     Between ({char = 31, line = 1},{char = 32, line = 1})),
		    (LId "interfaces",
		     Between ({char = 32, line = 1},{char = 42, line = 1})),
		    (Equal,
		     Between ({char = 42, line = 1},{char = 43, line = 1})),
		    (Open_List,
		     Between ({char = 43, line = 1},{char = 44, line = 1})),
		    (Close_List,
		     Between ({char = 44, line = 1},{char = 45, line = 1})),
		    (Close_Brace,
		     Between ({char = 45, line = 1},{char = 46, line = 1}))]));
	B.Test.test
	("lex Gateway 4u04",
	 test_lex ("Gateway 4u04",
		   [(LId "Gateway",
		     Between ({char=1,line=1},{char=8,line=1})),
		    (LUbyte4 4ux4,
		     Between ({char=9,line=1},{char=13,line=1}))]));
	B.Test.test
	("lex Gateway Host",
	 test_lex ("Gateway Host",
		   [(LId "Gateway",
		     Between ({char = 1, line = 1}, {char = 8, line = 1})), 
		    (LId "Host",
		     Between ({char = 9, line = 1}, {char = 13, line = 1}))]));
	B.Test.test
	("lex Gateway { Host 4ux04", 
	 test_lex ("Gateway { Host 4ux04", 
		   [(LId "Gateway",
		     Between ({char = 1, line = 1}, {char = 8, line = 1})), 
		    (Open_Brace,
		     Between ({char = 9, line = 1}, {char = 10, line = 1})), 
		    (LId "Host",
		     Between ({char = 11, line = 1},
			      {char = 15, line = 1}))]));
	B.Test.test
	("lex Gateway{name = \"hi\", ip = 4u0}", 
	 test_lex ("Gateway{name = \"hi\", ip = 4u0}", 
		   [(LId "Gateway",
		     Between ({char = 1, line = 1}, {char = 8, line = 1})), 
		    (Open_Brace,
		     Between ({char = 8, line = 1}, {char = 9, line = 1})), 
		    (LId "name",
		     Between ({char = 9, line = 1}, {char = 13, line = 1})), 
		    (Equal,
		     Between ({char = 13, line = 1}, {char = 14, line = 1})), 
		    (LString "hi",
		     Between ({char = 14, line = 1}, {char = 18, line = 1})), 
		    (Comma,
		     Between ({char = 18, line = 1}, {char = 19, line = 1})), 
		    (LId "ip",
		     Between ({char = 19, line = 1}, {char = 21, line = 1})), 
		    (Equal,
		     Between ({char = 21, line = 1}, {char = 22, line = 1})), 
		    (LUbyte4 zero,
		     Between ({char = 22, line = 1}, {char = 25, line = 1})), 
		    (Close_Brace,
		     Between ({char = 25, line = 1},
			      {char = 26, line = 1}))]));
	B.Test.test
	("lex Alias{name = SOME \"sly.fox.cs.cmu.edu\", host = " ^
	 "Host{name = \"sly.fox.cs.cmu.edu\", interfaces = []}}", 
	 test_lex ("Alias{name = SOME \"sly.fox.cs.cmu.edu\", host = " ^
		   "Host{name = \"sly.fox.cs.cmu.edu\", interfaces = []}}", 
		   [(LId "Alias",
		     Between ({char = 1, line = 1}, {char = 6, line = 1})), 
		    (Open_Brace,
		     Between ({char = 6, line = 1}, {char = 7, line = 1})), 
		    (LId "name",
		     Between ({char = 7, line = 1}, {char = 11, line = 1})), 
		    (Equal,
		     Between ({char = 11, line = 1}, {char = 12, line = 1})), 
		    (LSOME,
		     Between ({char = 12, line = 1}, {char = 16, line = 1})), 
		    (LString "sly.fox.cs.cmu.edu",
		     Between ({char = 17, line = 1}, {char = 37, line = 1})), 
		    (Comma,
		     Between ({char = 37, line = 1}, {char = 38, line = 1})), 
		    (LId "host",
		     Between ({char = 39, line = 1}, {char = 43, line = 1})), 
		    (Equal,
		     Between ({char = 43, line = 1}, {char = 44, line = 1})), 
		    (LId "Host",
		     Between ({char = 44, line = 1}, {char = 48, line = 1})), 
		    (Open_Brace,
		     Between ({char = 48, line = 1}, {char = 49, line = 1})), 
		    (LId "name",
		     Between ({char = 49, line = 1}, {char = 53, line = 1})), 
		    (Equal,
		     Between ({char = 53, line = 1}, {char = 54, line = 1})), 
		    (LString "sly.fox.cs.cmu.edu",
		     Between ({char = 54, line = 1}, {char = 74, line = 1})), 
		    (Comma,
		     Between ({char = 74, line = 1}, {char = 75, line = 1})), 
		    (LId "interfaces",
		     Between ({char = 75, line = 1}, {char = 85, line = 1})), 
		    (Equal,
		     Between ({char = 86, line = 1}, {char = 87, line = 1})), 
		    (Open_List,
		     Between ({char = 87, line = 1}, {char = 88, line = 1})), 
		    (Close_List,
		     Between ({char = 88, line = 1}, {char = 89, line = 1})), 
		    (Close_Brace,
		     Between ({char = 89, line = 1}, {char = 90, line = 1})), 
		    (Close_Brace,
		     Between ({char = 90, line = 1},
			      {char = 91, line = 1}))])))

  fun tparse (p, s, l) () = parse_from_string_using p s = l

  fun test_parser () =
       (B.Test.test ("parse \"hi\"", 
		     tparse (parse_string, "\"hi\"", SOME (TString "hi")));
	B.Test.test ("parse \"sly.fox.cs.cmu.edu\"", 
		     tparse (parse_string, "\"sly.fox.cs.cmu.edu\"", 
			     SOME (TString "sly.fox.cs.cmu.edu")));
	B.Test.test ("parse \"\"", 
		     tparse (parse_string, "\"\"", SOME (TString "")));
	B.Test.test ("parse hi", 
		     tparse (parse_id, "hi", SOME (TId "hi")));
	B.Test.test ("parse Hi", 
		     tparse (parse_id, "Hi", SOME (TId "Hi")));
	B.Test.test ("parse 4u04", 
		     tparse (parse_ubyte4, "4u04", SOME (TUbyte4 4ux4)));
	B.Test.test ("parse 4u04", 
		     tparse ((parse_ubyte4 || parse_string),
			     "4u04", SOME (TUbyte4 4ux4)));
	B.Test.test ("parse \"hi\"", 
		     tparse ((parse_ubyte4 || parse_string), "\"hi\"",
			     SOME (TString "hi")));
	B.Test.test ("parse {\"hi\"}", 
		     tparse ((parse_record (parse_ubyte4 || parse_string)),
			     "{\"hi\"}", SOME [TString "hi"]));
	B.Test.test ("parse 4u04", 
		     tparse ((parse_record (parse_ubyte4 || parse_string)),
			     "{\"hi\", 4u04}", 
			     SOME [TString "hi", TUbyte4 4ux4]));
	B.Test.test ("parse Gateway 4u04", 
		     tparse ((parse_constructor "Gateway" parse_ubyte4),
			     "Gateway 4u04", 
			     SOME ("Gateway", TUbyte4 4ux4)));
	B.Test.test ("parse Gateway{name = \"hi\", ip = 4u0}", 
		     tparse (parse_gateway, "Gateway{name = \"hi\", ip = 4u0}",
			     SOME (Gat (Gateway {ip = zero, name = "hi"}))));
	B.Test.test ("parse Interface .. ", 
		     tparse (parse_interface,
			     "Interface{name = \"SE0\", " ^
			     "ip = 4u0, mask = SOME 4u0, gateways = []}", 
			     SOME (Int (Interface {gateways = [], ip = zero,
						   mask = SOME 4u0,
						   name = "SE0"}))));
	B.Test.test ("parse Host", 
		     tparse (parse_host,
			     "Host{name = \"sly.fox.cs.cmu.edu\", " ^
			     "interfaces = []}", 
			     SOME (Hos (Host {interfaces = [],
					      name = "sly.fox.cs.cmu.edu"}))));
	B.Test.test ("parse SOME sly.fox.cs.cmu.edu", 
		     tparse ((parse_option parse_string),
			     "SOME \"sly.fox.cs.cmu.edu\"", 
			     SOME (TSOME (TString "sly.fox.cs.cmu.edu"))));
	B.Test.test ("parse NONE", 
		     tparse ((parse_option parse_string), "NONE", SOME TNONE));
	B.Test.test ("parse Alias", 
		     tparse (parse_alias, 
			     "Alias{name = SOME \"sly.fox.cs.cmu.edu\", \n" ^
			     "host = Host{name = \"sly.fox.cs.cmu.edu\", " ^
			     "interfaces = []}}", 
			     SOME (Ali
				   (Alias
				    {host = Host {interfaces = [],
						  name = "sly.fox.cs.cmu.edu"},
				     name = SOME "sly.fox.cs.cmu.edu"}))));
	let val h2a = "Host{name = \"sly.fox.cs.cmu.edu\", " ^
	              "interfaces = [Interface{name = \"SE0\", " ^
		      "mask = SOME 4u0, ip = 4u0, " ^
		      "gateways = [Gateway{name = \"gw.cs.cmu.edu\", " ^
		      "ip = 4u0}]}]}\n" ^
		      "Host{name = \"quick.fox.cs.cmu.edu\", " ^
		      "interfaces = [Interface{name = \"SE0\", " ^
		      "mask = SOME 4u0, " ^
		      "ip = 4u0, " ^
		      "gateways = [Gateway{name = \"gw.cs.cmu.edu\", " ^
		      "ip = 4u0}]}]}\n" ^
		      "Alias{name = NONE, " ^
		      "host = Host{name = \"sly.fox.cs.cmu.edu\", " ^
		      "interfaces = [Interface{name = \"SE0\", " ^
		      "mask = SOME 4u0, ip = 4u0, " ^
		      "gateways = [Gateway{name = \"gw.cs.cmu.edu\", " ^
		      "ip = 4u0}]}]}}"
	    val gw = [Gateway {ip = zero, name = "gw.cs.cmu.edu"}]
	    val interfaces = [Interface {gateways = gw, mask = SOME 4u0,
					 ip = zero, name = "SE0"}]
	in B.Test.test
	   ("parse 2 hosts and an Alias", 
	    tparse (parse_addressings, h2a, 
		    SOME [Hos (Host {interfaces = interfaces,
				     name = "sly.fox.cs.cmu.edu"}), 
			  Hos (Host {interfaces = interfaces,
				     name = "quick.fox.cs.cmu.edu"}), 
			  Ali (Alias
			       {host = Host {interfaces = interfaces, 
					     name = "sly.fox.cs.cmu.edu"},
				name = NONE})]))
	end)

  fun run () = 
       (test_lexer ();
	test_parser ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		2.	structure Test_Addressing 
*)

structure Test_Addressing = Test_Addressing (structure B = Fox_Basis)
