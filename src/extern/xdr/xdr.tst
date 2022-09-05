(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Nick Haines (Nick.Haines@cs.cmu.edu)
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	Test file for the different XDR marshal/unmarshal functions.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	iv.	Note
	1.	functor Test_Xdr
	2.	test of XInt
	3.	test of XVoid
	4.	test of XEnum
	5.	test of XString and XFString
	6.	test of XBool
	7.	test of XUByte4
	8.	test of XOpaque and XFOpaque
	9.	test of XArray and XFArray
	10.	test of union functors
	11.	test of XList
	12.	test of XIntOpt
	13.	test of XDR-"structure" functors
	14.	function test_runs
	15.	function run
	16.	structure Test_Xdr
	1.      functor Test_Xdr

	iii.	RCS Log

$Log: xdr.tst,v $
Revision 1.3  1995/02/13  22:58:02  esb
adapted to new interfaces which don't depend on Fox_Basis.

Revision 1.2  1995/02/07  23:26:57  esb
adapted to new extern.sig.

Revision 1.1  1994/10/14  12:00:03  kcchiang
Initial revision

	iv.	Note

	Any tests that end with suffix "oob" are "out of bounds" tests.

	1.	functor Test_Xdr
*)

functor Test_Xdr (structure B: FOX_BASIS): TEST_STRUCTURE =
 struct

  fun eql (a, b) = (a = b)

  fun test_xdr (eq, size, marshal, unmarshal, internal) =
       let val length = size internal
	   val extern = B.Dyn_Array.new length
	   val marshaled = marshal (extern, internal) extern
	   val (result, unmarshaled) = unmarshal (extern, extern)
	in eq (result, internal) andalso
	   B.Dyn_Array.size marshaled = B.Dyn_Array.size unmarshaled andalso
	   B.Dyn_Array.size extern - B.Dyn_Array.size marshaled = length
       end

  fun test_many (test_one, list) () =
       fold (fn (a, b) => a andalso b) (map test_one list) true

  fun thunk f () = f

  fun make_pair (high, low) =
       FoxWord32.orb (FoxWord32.lshift (FoxWord32.intToWord high, 16),
		      FoxWord32.intToWord low)

  val n4uxffffffff = make_pair (0xffff, 0xffff)
  val n4uxabcdef00 = make_pair (0xabcd, 0xef00)
  val n4ux00001030 = make_pair (0x0000, 0x1030)
  val n4ux00000000 = make_pair (0x0000, 0x0000)

(*
	2.	test of XInt
*)

  local   
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
  in
   fun test_intxdr x = 
	test_xdr (eql, XInt.size, XInt.marshal, XInt.unmarshal, x)

   fun test_int_oob () =
	(test_intxdr ~1;
	 false)
	handle XInt.Extern => true
  end

(*
	3.	test of XVoid
*)

  local
   structure XVoid = XVoid (structure D = B.Dyn_Array)
  in
   fun test_voidxdr () =
	test_xdr (eql, XVoid.size, XVoid.marshal, XVoid.unmarshal, ())
  end

(*
	4.	test of XEnum
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XEnum = XEnum (structure Int = XInt
			    structure D = B.Dyn_Array
			    val range = (0, 5))
  in
   fun test_enumxdr x =
        test_xdr (eql, XEnum.size, XEnum.marshal, XEnum.unmarshal, x)

   fun test_enum_oob () =
        (test_enumxdr 6;
	 false)
	handle XEnum.Extern => true
  end

(*
	5.	test of XString and XFString
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XStr = XString (structure Int = XInt
			     structure D = B.Dyn_Array
			     structure Access = B.Access
			     structure V = B.V)
   structure XFStr = XFString (val number_of_bytes = 12
			       structure String = XStr
			       structure D = B.Dyn_Array)
  in
   fun test_strxdr x =
	test_xdr (eql, XStr.size, XStr.marshal, XStr.unmarshal, x)

   fun test_fstrxdr x = 
	test_xdr (eql, XFStr.size, XFStr.marshal, XFStr.unmarshal, x)

   fun test_fstr_oob () =
	(test_fstrxdr  "123456789";
	 false)
	handle XFStr.Extern => true
  end

(*
	6.	test of XBool
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XBool = XBool (structure Int = XInt)
  in
   fun test_boolxdr x =
        test_xdr (eql, XBool.size, XBool.marshal, XBool.unmarshal, x)
  end

(*
	7.	test of XUByte4
*)

  local
   structure XUByte4 = XUByte4 (structure D = B.Dyn_Array
				structure B4 = B.Order.B4)
  in
   fun test_ubyte4xdr x =
        test_xdr (eql, XUByte4.size, XUByte4.marshal, XUByte4.unmarshal, x)
  end

(*
	8.	test of XOpaque and XFOpaque
*)

  local 
   structure XFOpaque = XFOpaque (val fixed_size = 10
				  structure D = B.Dyn_Array)
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XOpaque = XOpaque (structure Int = XInt
				structure D = B.Dyn_Array)

   fun eql (ba1, ba2) =
        let val arr_len1 = ByteArray.length ba1
	    val arr_len2 = ByteArray.length ba2
	    fun loop 0 = true
	      | loop n =
		 ByteArray.sub (ba1, n - 1) = ByteArray.sub (ba2, n - 1)
		 andalso loop (n - 1)
	in if arr_len1 = arr_len2 then
	    loop arr_len1
	   else false
	end
  in
   fun test_fopaque x = 
	test_xdr (eql, XFOpaque.size, XFOpaque.marshal, XFOpaque.unmarshal, x)

   fun test_opaque x =
        test_xdr (eql, XOpaque.size, XOpaque.marshal, XOpaque.unmarshal, x)

   fun test_fopaque_oob () =
        (test_xdr (eql, XFOpaque.size, XFOpaque.marshal, XFOpaque.unmarshal, 
		   ByteArray.array (13, 5));
	 false)
	handle XFOpaque.Extern => true
  end

(*
	9.	test of XArray and XFArray
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XFArray = XFArray (val fixed_size = 10
				structure Element = XInt)
   structure XArray = XArray (structure Int = XInt
			      structure Element = XInt)

   fun eql (a1,a2) =
        let val arr_len1 = Array.length a1
	    val arr_len2 = Array.length a2
	    fun loop 0 = true
	      | loop n = (Array.sub (a1,n-1) = Array.sub (a2,n-1))
		         andalso loop (n-1)
        in if arr_len1 = arr_len2 then loop arr_len1
	    else false
	end
  in
   fun test_farr x =
	test_xdr (eql, XFArray.size, XFArray.marshal, XFArray.unmarshal, x)

   fun test_farr_oob () =
	(test_xdr (eql, XFArray.size, XFArray.marshal,
		   XFArray.unmarshal, (Array.array (11,0)));
	 false)
	handle XFArray.Extern => true

   fun test_arr x =
	test_xdr (eql, XArray.size, XArray.marshal, XArray.unmarshal, x)
  end

(*
	10.	test of union functors
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XString = XString (structure Int = XInt
				structure D = B.Dyn_Array
				structure Access = B.Access
				structure V = B.V)
   structure XBool = XBool (structure Int = XInt)
   structure XEnum = XEnum (structure Int = XInt
			    structure D = B.Dyn_Array
			    val range = (1, 5))
   structure XUnion2 = XUnion2 (structure Variant_1 = XInt
				structure Variant_2 = XString
				structure Enum = XEnum
				val discriminants = (1, 2))
   structure XUnion2d = XUnion2_Default (structure Variant_1 = XInt
					 structure Variant_2 = XString
					 structure Enum = XEnum
					 val discriminants = (1, 2)
					 val skip = B.Dyn_Array.tail)
   structure XUnion3d = XUnion3_Default (structure Variant_1 = XInt
					 structure Variant_2 = XBool
					 structure Variant_3 = XString
					 structure Enum = XEnum
					 val discriminants = (1, 2, 3)
					 val skip = B.Dyn_Array.tail)

   fun eql (XUnion2.T1 a, XUnion2.T1 b) = (a=b)
     | eql (XUnion2.T2 a, XUnion2.T2 b) = (a=b)
     | eql _ = false

   fun eql_d (XUnion2d.T1 a, XUnion2d.T1 b) = (a=b)
     | eql_d (XUnion2d.T2 a, XUnion2d.T2 b) = (a=b)
     | eql_d _ = false

   fun eql3 (XUnion3d.T1 a, XUnion3d.T1 b) = (a=b)
     | eql3 (XUnion3d.T2 a, XUnion3d.T2 b) = (a=b)
     | eql3 (XUnion3d.T3 a, XUnion3d.T3 b) = (a=b)
     | eql3 _ = false

  in
   fun test_union2 () =
        let fun test_one x =
	         test_xdr (eql, XUnion2.size, XUnion2.marshal,
			   XUnion2.unmarshal, x)
	in test_many (test_one,
		      [(XUnion2.T1 1000), (XUnion2.T1 2),
		       (XUnion2.T2 "hello"), (XUnion2.T2 "foo")]) ()
	end

   fun test_union2d () =
        let fun test_one x =
	         test_xdr (eql_d, XUnion2d.size, XUnion2d.marshal,
			   XUnion2d.unmarshal, x)
	    fun test_def () =		(* test default tag in union *)
	      (* make a string with different union tag *)
	         let val str = "secret"
		     val tag_size = 4
		     val length = XString.size str + tag_size
		     val extern = B.Dyn_Array.new length
		     val fake = B.Dyn_Array.tail (extern, tag_size)
		     val marshaled = XString.marshal (fake, str) fake
		 in case XUnion2d.unmarshal (extern, extern) of
		       (XUnion2d.T1 _,_) => false
		     | (XUnion2d.T2 st,_) => st = str
		 end
	in test_many (test_one,
		      [XUnion2d.T1 3000, XUnion2d.T1 77,
		       XUnion2d.T2 "bar", XUnion2d.T2 "mee"]) () andalso
	   test_def () 
	end

   fun test_union3d () =
        let fun test_one x =
	         test_xdr (eql3, XUnion3d.size, XUnion3d.marshal,
			   XUnion3d.unmarshal, x)
	    fun test_def () =		(* test default tag in union *)
	      (* make a string with different union tag *)
	         let val str = "secret"
		     val tag_size = 4
		     val length = XString.size str + tag_size
		     val extern = B.Dyn_Array.new length
		     val fake = B.Dyn_Array.tail (extern, tag_size)
		     val marshaled = XString.marshal (fake, str) fake
		 in case XUnion3d.unmarshal (extern, extern) of
		       (XUnion3d.T1 _, _) => false
		     | (XUnion3d.T2 _, _) => false
		     | (XUnion3d.T3 st, _) => st = str
		 end
	in test_many (test_one,
		      [XUnion3d.T1 3000, XUnion3d.T1 77,
		       XUnion3d.T2 false, XUnion3d.T2 true,
		       XUnion3d.T3 "bar", XUnion3d.T3 "mee"]) () andalso
	   test_def ()
      end

  end

(*
	11.	test of XList
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XBool = XBool (structure Int = XInt)
   structure XList = XList (structure Element = XInt
			    structure Bool = XBool)
  in
   fun test_list x = 
	test_xdr (eql, XList.size, XList.marshal, XList.unmarshal, x)
  end

(*
	12.	test of XIntOpt
*)

  local
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XUByte4 = XUByte4 (structure D = B.Dyn_Array
				structure B4 = B.Order.B4)
   structure XIntOpt = XIntOpt (structure Int = XInt
				structure B4 = XUByte4)
  in
   fun test_intopt x =
	test_xdr (eql, XIntOpt.size, XIntOpt.marshal, XIntOpt.unmarshal, x)
  end

(*
	13.	test of XDR-"structure" functors
*)

  (* structs *)
  local 
   structure XInt = XInt (structure D = B.Dyn_Array
			  structure B4 = B.Order.B4)
   structure XBool = XBool (structure Int = XInt)
   structure XString = XString (structure Int = XInt
				structure D = B.Dyn_Array
				structure Access = B.Access
				structure V = B.V)

   structure Xs2 = XStruct2 (structure X1 = XInt
			     structure X2 = XBool)
   structure Xs3 = XStruct3 (structure X1 = XInt
			     structure X2 = XBool
			     structure X3 = XString)
   structure Xs4 = XStruct4 (structure X1 = XInt
			     structure X2 = XBool
			     structure X3 = XString
			     structure X4 = XInt)
   structure Xs5 = XStruct5 (structure X1 = XInt
			     structure X2 = XBool
			     structure X3 = XString
			     structure X4 = XInt
			     structure X5 = XBool)
   structure Xs6 = XStruct6 (structure X1 = XInt
			     structure X2 = XBool
			     structure X3 = XString
			     structure X4 = XInt
			     structure X5 = XBool
			     structure X6 = XString)
   structure Xs7 = XStruct7 (structure X1 = XInt
			     structure X2 = XBool
			     structure X3 = XString
			     structure X4 = XInt
			     structure X5 = XBool
			     structure X6 = XString
			     structure X7 = XInt)
  in
   fun test_struct2 x =
	test_xdr (eql, Xs2.size, Xs2.marshal, Xs2.unmarshal, x)

   fun test_struct3 x =
	test_xdr (eql, Xs3.size, Xs3.marshal, Xs3.unmarshal, x)

   fun test_struct4 x =
	test_xdr (eql, Xs4.size, Xs4.marshal, Xs4.unmarshal, x)

   fun test_struct5 x =
	test_xdr (eql, Xs5.size, Xs5.marshal, Xs5.unmarshal, x)

   fun test_struct6 x =
	test_xdr (eql, Xs6.size, Xs6.marshal, Xs6.unmarshal, x)

   fun test_struct7 x =
	test_xdr (eql, Xs7.size, Xs7.marshal, Xs7.unmarshal, x)
  end

(*
	14.	function test_runs
*)

  fun test_runs () = 
       (B.Test.test ("intxdr",
		     test_many (test_intxdr, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
	B.Test.test ("int_oob test", test_int_oob);

	B.Test.test ("voidxdr", test_voidxdr);

	B.Test.test ("enumxdr",
		     test_many (test_enumxdr, [0, 1, 2, 3, 4, 5]));
	B.Test.test ("enum_oob test", test_enum_oob);
    
	B.Test.test ("strxdr",
		     test_many (test_strxdr,
				["abc", "Brian is a slave driver",
				 "I hate testing"]));
	B.Test.test ("fstrxdr",
		     test_many (test_fstrxdr, ["12345678", "12345", "1"]));
	B.Test.test ("fstroob test", test_fstr_oob);
    
	B.Test.test ("boolxdr", 
		     test_many (test_boolxdr, [true, false]));

	B.Test.test ("ubyte4xdr",
		     test_many (test_ubyte4xdr,
				[n4uxffffffff, n4uxabcdef00,
				 n4ux00001030, n4ux00000000]));

	B.Test.test ("fopaque",
		     test_many (test_fopaque,
				[ByteArray.array (10, 2),
				 ByteArray.array (10, 255)]));
	B.Test.test ("fopaque_oob", test_fopaque_oob);
	B.Test.test ("opaque",
		     test_many (test_opaque,
				[ByteArray.array (1, 1),
				 ByteArray.array (1000, 6),
				 ByteArray.array (50, 200)]));

	B.Test.test ("farray",
		     test_many (test_farr,
				[Array.array (10, 2), Array.array (10, 999)]));
	B.Test.test ("farray_oob", test_farr_oob);
	B.Test.test ("array",
		     test_many (test_arr,
				[Array.array (1, 67), Array.array (1000, 777),
				 Array.array (50, 2)]));

	B.Test.test ("union2", test_union2);
	B.Test.test ("union2d", test_union2d);
	B.Test.test ("union3d", test_union3d);

	B.Test.test ("list",
		     test_many (test_list,
				[[1, 2, 3], [100, 200, 300, 400, 500, 600],
				 nil]));

	B.Test.test ("intopt",
		     test_many (test_intopt,
				[SOME 1, SOME 1000, NONE, SOME 255]));

	B.Test.test ("struct2",
		     test_many (test_struct2,
				[(1,false),(9999,true),(82,false)]));
	B.Test.test ("struct3",
		     test_many (test_struct3,
				[(1, false, "foo"), (9999, true, "bar"),
				 (82, false, "t")]));
	B.Test.test ("struct4",
		     test_many (test_struct4,
				[(1, false, "dalala", 9999),
				 (9999, true, "why like that", 0),
				 (82, false, "hmmm,  delicious", 11)]));
	B.Test.test ("struct5",
		     test_many (test_struct5,
				[(1, false, "dalala", 9999, true),
				 (9999, true, "why like that", 0, true),
				 (82, false, "hmmm,  delicious", 11, false)]));
	B.Test.test ("struct6",
		     test_many (test_struct6,
				[(1, false, "dalala", 9999, true,
				  "oh no! help"),
				 (9999, true, "why like that", 0, true,
				  "long boring test"),
				 (82, false, "hmmm,  delicious", 11,
				  false, "please!")]));
	B.Test.test ("struct7",
		     test_many (test_struct7,
				[(1, false, "dalala", 9999, true,
				  "oh no! help", 91),
				 (9999, true, "why like that", 0,
				  true, "long bore test", 0),
				 (82, false, "hmmm,  delicious", 11,
				  false, "please!", 91)]));
	())

(*
	15.	function run
*)

  fun run () =
      (B.Test.tests ("xdr", 27, test_runs);
       ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end

(*
	16.	structure Test_Xdr
*)

structure Test_Xdr = Test_Xdr (structure B = Fox_Basis)
