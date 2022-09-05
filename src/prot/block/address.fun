(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	multiplex.fun

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Address

	iii.	RCS Log

$Log: address.fun,v $
Revision 1.3  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.2  1995/06/27  19:15:01  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Address
*)

functor Address (structure Lower: PROTOCOL
		 structure Address: EXTERN_KEY
		  sharing type Lower.Incoming.T = Address.extern_in
		      and type Lower.Outgoing.T = Address.extern_out
(* val verify_receiver: bool *)
		 val source_precedes_dest: bool
		 val cursor_in: Address.extern_in -> Address.cursor
		 val cursor_out: Address.extern_out -> Address.cursor
		 structure B: FOX_BASIS): PROTOCOL =
 struct

  local
   structure Addresses: EXTERN_KEY =
    struct
     type T = {self: Address.T, peer: Address.T}
     type extern_in = Address.extern_in
     type extern_out = Address.extern_out
     type cursor = Address.cursor

     fun makestring {self, peer} = 
          Address.makestring self ^ "," ^ Address.makestring peer

     fun equal ({self = s1, peer = p1}, {self = s2, peer = p2}) =
          Address.equal (s1, s2) andalso Address.equal (p1, p2)

     fun hash {self, peer} = Address.hash self + Address.hash peer

     exception Extern

     fun size {self, peer} = Address.size self + Address.size peer

     fun marshal_source_first (extern, {self, peer}) =
          Address.marshal (extern, peer) o Address.marshal (extern, self)
     fun marshal_dest_first (extern, {self, peer}) =
          Address.marshal (extern, self) o Address.marshal (extern, peer)
     val marshal = if source_precedes_dest then marshal_source_first
		   else marshal_dest_first

     fun unmarshal_source_first (arg as (extern, _)) =
          let val (peer, pos) = Address.unmarshal arg
	      val (self, final) = Address.unmarshal (extern, pos)
	  in ({self = self, peer = peer}, final)
	  end
     fun unmarshal_dest_first (arg as (extern, _)) =
          let val (self, pos) = Address.unmarshal arg
	      val (peer, final) = Address.unmarshal (extern, pos)
	  in ({self = self, peer = peer}, final)
	  end
     val unmarshal = if source_precedes_dest then unmarshal_source_first
		     else unmarshal_dest_first

    end

   structure Multiplex = Multiplex (structure Lower = Lower
				    structure Selector = Addresses
				    val cursor_in = cursor_in
				    val cursor_out = cursor_out
				    structure B = B
				    val name = "address.fun")
  in
   open Multiplex
  end (* local *)

 end (* struct *)
