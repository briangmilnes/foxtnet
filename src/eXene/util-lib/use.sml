(*
$Log: use.sml,v $
Revision 1.1  1994/08/17  21:46:52  robby
Initial revision

*)

val util_lib=map (fn x=>"/usr/misc/.sml/lib/smlnj-lib/"^x)
  ["lib-base-sig.sml",		"lib-base.sml",
   "ctype-sig.sml",		"ctype.sml",
   "hash-string.sml",
   "hash-key-sig.sml",
   "hash-table-sig.sml",	"hash-table.sml",
   "hash-string.sml",
   "makestring-sig.sml",	"makestring.sml",
   "string-cvt-sig.sml",	"string-cvt.sml",
   "format-sig.sml",		"format.sml",
   "random-sig.sml",		"random.sml",
   "charset-sig.sml",		"charset.sml",
   "string-util-sig.sml",	"string-util.sml",
   "listsort-sig.sml",
   "list-mergesort.sml"]
