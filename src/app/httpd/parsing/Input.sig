(* Chris Okasaki
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
   cokasaki@cs.cmu.edu *)

signature INPUT =
sig
  val readfile  : string -> char Stream.T  (* return a stream of characters *)
  val readkeybd : unit   -> char Stream.T  (* return a stream of characters *)
  val readstring : string -> char Stream.T (* return a stream of characters *)
end
