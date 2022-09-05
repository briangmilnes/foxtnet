signature CYCLECOUNT =
sig
  val count: unit -> word32
  val count_process: unit -> word32
end

structure CycleCount : CYCLECOUNT =
struct

  fun makeCode (instructions: word32 list) =
    let 
      type ba = Word8Array.array

      val mkCode : ba -> (ba * (unit->word32)) = 
            System.Unsafe.CInterface.c_function "SMLNJ-RunT" "mkCode"

      val code = Word8Array.array ((length instructions + 1) * 4, 0w0)

      fun loop ([],_) = ()
        | loop (i::rest, n) =
	    (Pack32Little.update(code, n, i);
	     loop (rest, n+1))

      val _ = loop (instructions,1)
    in
      #2(mkCode(code))
    end

(*
  NOTE:

  The opcode values below were determined by running the assembly
  code in comments through as on the alpha OSF1 2.0 assembler, (e.g
  "as tmp.s").  The result was viewed by running "gdb a.out" and using
  the command "x/20i count".
*)

  val count = makeCode [
			(*	.set	noat			*)
			(*	.globl 	count			*)
			(*	count:				*)
	0wx601fc000,	(*		rpcc	$0		*)
	0wx401f0000,	(*		addl	$0, $31, $0	*)
	0wx6b810000	(*		jmp     $28, ($1)	*)
    ]

  val count_process = makeCode [
			(*	.globl 	count_process		*)
			(*	count_process:			*)
	0wx601fc000,	(*		rpcc	$0		*)
	0wx4804173c,	(*		sll	$0, 32, $28	*)
	0wx401c0400,	(*		addq	$0, $28, $0	*)
	0wx48041780,	(*		sra	$0, 32, $0	*)
	0wx6b810000	(*		jmp     $28, ($1)	*)
    ]

end
