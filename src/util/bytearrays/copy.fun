(*
% This file should be printable using TeX (not latex).
%
% To print under Unix:
%
% cp copy.fun copy.tex
% tex copy.tex
% dvips copy.dvi > copy.ps
% lpr copy.ps
%

\def\uncatcodespecials{\def\do##1{\catcode`##1=12 }\dospecials}

\newcount\lineno % the number of file lines listed (TeX book, p. 381)
\def\setupverbatim{\tt \lineno=0
  \obeylines \uncatcodespecials \obeyspaces}
{\obeyspaces\global\let =\ }  % let active space = control space

\def\verbatim{\begingroup\setupverbatim\doverbatim}
\def\doverbatim#1{\def\next##1#1{##1\endgroup}\next}

\def\ignore#1{}

{\obeylines
\frenchspacing


        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
        Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

\bigskip
		i.	Abstract

        Efficient copy functions in ML.
\bigskip

\hrule
% ---------------------------------------------------------------------
\smallskip
		ii.	Table of Contents  \bigskip

	i.	Abstract
	ii.	Table of Contents  \bigskip
	iii.	RCS Log
	1.	Functor Copy
	2.	Internal function unaligned
	3.	Internal function common
	4.	Internal function sixteen
	5.	Internal function aligned
	6.	Internal function eightlittle
	7.	Internal function eightbig
	8.	Internal function eight
	9.	Internal function semialigned
	10.	Function illegal
	11.	Function copy

\smallskip
\hrule
% ---------------------------------------------------------------------
\smallskip
		iii.	RCS Log
        
\ignore{
$Log: copy.fun,v $
Revision 1.25  1997/02/06  16:37:58  cline
added Word32fox structure for compatibility with CM

Revision 1.24  1996/03/12  22:27:47  esb
adapted to new FOXWORD.

Revision 1.23  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.22  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.21  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.20  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.19  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.18  1994/07/07  02:16:40  esb
optimized eightlittle, eightbig, and introduced common.

Revision 1.17  1994/07/04  21:37:53  esb
extracted the array creation function.

Revision 1.16  94/06/16  17:48:13  danwang
Updated for use with functorized Fox_Basis.

Revision 1.15  1994/06/16  17:31:56  esb
Major optimization, redid proof.

Revision 1.14  94/03/16  16:30:54  esb
rewrote the copy code and wrote a proof of correctness.

Revision 1.13  94/03/10  19:35:21  esb
added create_buffer, discontinued use_safe.

Revision 1.12  94/03/08  22:14:09  esb
fixed a timing board problem.

Revision 1.11  1994/03/02  18:19:56  esb
optimized copying when alignment is on 2-byte boundaries.

Revision 1.10  94/02/25  22:08:51  esb
replaced mod operations with bitwise and operations.

Revision 1.9  94/02/25  18:20:23  milnes
Made safe/unsafe switch.

Revision 1.8  1994/02/20  15:32:39  esb
added COPY signature.

Revision 1.7  94/02/20  13:45:04  esb
fixed a bug and added use_safe.

Revision 1.6  94/02/18  16:21:39  esb
added exception Illegal_Create.

Revision 1.5  94/02/18  15:59:20  jgmorris
 Fixed bug in create:  wasn't checking for bytearrays of size <= 0.
Now raises exception Create_Zero in such as case.

Revision 1.4  94/01/14  17:48:52  cline
Added create functions.

Revision 1.3  1993/11/04  16:11:19  esb
added error checking and substantial optimization.

Revision 1.2  1993/10/29  05:38:29  esb
got rid of the type bytearray declaration.

Revision 1.1  1993/10/29  04:46:02  esb
Initial revision
}

\bigskip
\hrule
% ---------------------------------------------------------------------
}

\vfill\eject
\noindent
		1.	Functor Copy
\medskip

\noindent
        The copy function in this module has a proof of correctness.
        It does not follow that the function is correct.  For example,
        the proof could be invalid or useless for any of the following
        reasons:

        \item{$\bullet$}
	  the specification could be ``wrong''.  For example, the
          requirement that the destination and source arrays not be
          the same may not conform to the programmer's expectations.

        \item{$\bullet$} the proof could be wrong.

        \item{$\bullet$} the code text may not correspond to the proof text.

        \item{$\bullet$} the proof may be missing important cases or steps.

        \item{$\bullet$} some of the assumptions made may not hold.
          For example, we currently assume that

              $Word32.bigEndian = false$ implies that

              $Word16.update (a, 0, x);$ $Word16.update (a, 2, y)$
              is equivalent to
	      $Word32.update (a, 0, y \ll 16 | x)$

          when in fact all that is implied is that

              $Word8Array.update (a, 0, x);$ $Word8Array.update (a, 1, y);$
              $Word8Array.update (a, 2, z);$ $Word8Array.update (a, 3, h);$ 

	      is equivalent to 
              $Word32.sub (a, 0) = (h \ll 24 | z \ll 16 | y \ll 8 | x)$

\medskip
\noindent
        A proof does have advantages:

        \item{$\bullet$}
	  it is good documentation, explaining and motivating what
          the program does.

        \item{$\bullet$}
          it forces someone to think about what the code is doing,
          and therefore examine the code carefully.

        \item{$\bullet$} it encourages clear programming.

\medskip \noindent Following Mason (Stanford PhD thesis 1986, ``The
semantics of Destructive Lisp'', published in 1987 by U. Chicago Press
in the CSLI Lecture Notes series), we use $\simeq$ for program
equivalence.  We use $\ll$ for left shifts, $\gg$ for right shifts,
$\&$ for bit-wise logical {\it and}, and $|$ for bit-wise logical {\it
or}.  For performance, $x \bmod n$ for any $n = 2^m$ is expressed as
{\tt andb(x, m-1)}.


\medskip \noindent The proof begins in section 10.  This is the
functor header.

\bigskip

\verbatim@
*)

functor Copy (): COPY =
 struct

   (* raised if the copy would access data outside one of the arrays *)
  exception Illegal_Copy of {source_length: int, source_offset: int,
                             bytes: int, dest_length: int, dest_offset: int}

  exception Self_Copy

  exception Illegal_Align_Value_In_Copy
         (* never raised; avoids compiler warnings *)

  local

(*
@
% ---------------------------------------------------------------------
\vfill\eject
\noindent
		2.	Internal function unaligned

\medskip
This is the basic function for copying bytes.  We prove that
this is correct, then transform it to the more specific,
more optimized version.  The specification given here is
equivalent to that given in section 10, after replacing
{\tt bytes} with $\hbox{\tt endsrc} - \hbox{\tt srcpos}$
and {\tt Illegal\_Copy} with \hbox{Subscript}.

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$.
\item{b} $\forall i \in \{0\ldots length(src)-1\}$
          the initial value of $src_i$ is $src0_i$.

\medskip
\noindent
Specification:
\medskip
\item{1} if $endsrc - srcpos \le 0$ then
      $\hbox{\tt unaligned} \simeq \hbox{null function}$
\itemitem{}{\bf Proof}:
               on the first call to {\tt loop}, $i = srcpos$, so
               $(i \ge endsrc) = (srcpos \ge endsrc)$ is true so
               $\hbox{\tt unaligned} \simeq \hbox{null function}$.

\item{2} if $srcpos < 0 \vee destpos < 0$ then
     $\hbox{\tt unaligned} \simeq \hbox{\tt raise Subscript}$
     (partial copies to $dest$ are allowed).
\itemitem{}{\bf Proof}:
               on the first call to {\tt loop},
               $i = srcpos$ and $j = destpos$.
               On each iteration of {\tt loop}
               when $i < endsrc$,
               we access both $src_i$ and $dest_j$.
               Therefore, since on the first iteration of loop
               when $i = srcpos < endsrc$,
               if $srcpos < 0$ we raise {\tt Subscript},
               and likewise if $destpos < 0$.

%parametrized proof of the next two cases.
\def\proofsubscript#1#2#3#4#5{
               In successive calls to {\tt loop}, $#4$ steps by $1$
               from $#1$ to $#2$ (since $#1 < #2$), so
               if ever $#4 \ge length(#5)$ in the else clause of {\tt loop},
               then $#2 - 1 \ge length(#5)$.
               In that case, when $#4$ reaches the value $#2 - 1$,
               the code ``{\tt #3}'' will raise {\tt Subscript}.}

\item{3} if $\exists i \in \{srcpos\ldots endsrc-1\} : i \ge length(src)$ then
     $\hbox{\tt unaligned} \simeq \hbox{\tt raise Subscript}$
     (partial copies to $dest$ are allowed but not required).
\itemitem{}{\bf Proof}:
\proofsubscript{srcpos}{endsrc}{Word8Array.sub (src, i)}{i}{src}

\item{4}
if $\exists j\in\{destpos\ldots endsrc+destpos-srcpos-1\}:j\ge length(dest)$
then \item{}$\hbox{\tt unaligned} \simeq \hbox{\tt raise Subscript}$
     (partial copies to $dest$ are allowed but not required).
\itemitem{}{\bf Proof}:
\proofsubscript{destpos}{destpos+endsrc-srcpos}%
{Word8Array.update (dest, j, ...)}{j}{dest}

\item{5} otherwise, after the call,
   $\forall i \in \{srcpos\ldots endsrc-1\}
     (src_i = src0_i = dest_{i+destpos-srcpos})$
\itemitem{}{\bf Proof}:
\itemitem{---} {\tt loop} is called exactly $endsrc - srcpos + 1$ times;
               each call, $i$ has a different value, in the range
               $\{srcpos\ldots endsrc\}$, so $i$ takes on every value in
               the range.
\itemitem{---} The last call to {\tt loop} has no effect.
\itemitem{---} In {\tt loop}, $j = i + destpos - srcpos$.
\itemitem{---} Each call to {\tt loop} other than the last
               stores $src_i$ into $dest_j$
               and modifies neither $src$ (since $src \ne dest$)
               nor other locations of $dest$.
\itemitem{} Therefore, {\tt unaligned}
\itemitem{} \quad terminates.
\itemitem{} \quad reads each $src_{srcpos}\ldots src_{endsrc-1}$.
\itemitem{} \quad stores each $src_i$ it reads into $dest_{i+destpos-srcpos}$.
\itemitem{} \quad leaves $\forall i (src_i = src0_i)$
\medskip
\verbatim@
*)

   fun unaligned (src, srcpos, endsrc, dest, destpos) =
        let fun loop (i, j) =
                 if i >= endsrc then ()
                 else
                  (Word8Array.update (dest, j, Word8Array.sub (src, i));
                   loop (i + 1, j + 1))
        in loop (srcpos, destpos)
        end

(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		3.	Internal function common

\medskip
\noindent
This function optimizes the implementation of short copies of 1, 2, 3,
4, 8, 16, or 20 bytes.

\medskip
\noindent
Preconditions:
same as for {\tt unaligned}.

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip
\noindent
Proof:  We can transform {\tt unaligned} into this function by a sequence
of steps, none of which changes the meaning of the function:
\item{1} Rename $i$ and $j$ to $srcpos$ and $destpos$.
\item{2} Inline the loop.
\item{3} Break into cases, use a call to {\tt unaligned} for the
default case, and unroll the loop the appropriate number of times for
the specific cases.

\verbatim@
*)

   fun common (src, srcpos, endsrc, dest, destpos) =
        case endsrc - srcpos of
           0 => ()
         | 1 =>
            Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos))
         | 2 =>
            (Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos));
             Word8Array.update (dest, destpos + 1, Word8Array.sub (src, srcpos + 1)))
         | 3 =>
            (Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos));
             Word8Array.update (dest, destpos + 1, Word8Array.sub (src, srcpos + 1));
             Word8Array.update (dest, destpos + 2, Word8Array.sub (src, srcpos + 2)))
         | 4 =>
            (Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos));
             Word8Array.update (dest, destpos + 1, Word8Array.sub (src, srcpos + 1));
             Word8Array.update (dest, destpos + 2, Word8Array.sub (src, srcpos + 2));
             Word8Array.update (dest, destpos + 3, Word8Array.sub (src, srcpos + 3)))
         | 8 =>
            (Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos));
             Word8Array.update (dest, destpos + 1, Word8Array.sub (src, srcpos + 1));
             Word8Array.update (dest, destpos + 2, Word8Array.sub (src, srcpos + 2));
             Word8Array.update (dest, destpos + 3, Word8Array.sub (src, srcpos + 3));
             Word8Array.update (dest, destpos + 4, Word8Array.sub (src, srcpos + 4));
             Word8Array.update (dest, destpos + 5, Word8Array.sub (src, srcpos + 5));
             Word8Array.update (dest, destpos + 6, Word8Array.sub (src, srcpos + 6));
             Word8Array.update (dest, destpos + 7, Word8Array.sub (src, srcpos + 7)))
         | 16 =>
            (Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos));
             Word8Array.update (dest, destpos + 1, Word8Array.sub (src, srcpos + 1));
             Word8Array.update (dest, destpos + 2, Word8Array.sub (src, srcpos + 2));
             Word8Array.update (dest, destpos + 3, Word8Array.sub (src, srcpos + 3));
             Word8Array.update (dest, destpos + 4, Word8Array.sub (src, srcpos + 4));
             Word8Array.update (dest, destpos + 5, Word8Array.sub (src, srcpos + 5));
             Word8Array.update (dest, destpos + 6, Word8Array.sub (src, srcpos + 6));
             Word8Array.update (dest, destpos + 7, Word8Array.sub (src, srcpos + 7));
             Word8Array.update (dest, destpos + 8, Word8Array.sub (src, srcpos + 8));
             Word8Array.update (dest, destpos + 9, Word8Array.sub (src, srcpos + 9));
             Word8Array.update (dest, destpos + 10, Word8Array.sub (src, srcpos + 10));
             Word8Array.update (dest, destpos + 11, Word8Array.sub (src, srcpos + 11));
             Word8Array.update (dest, destpos + 12, Word8Array.sub (src, srcpos + 12));
             Word8Array.update (dest, destpos + 13, Word8Array.sub (src, srcpos + 13));
             Word8Array.update (dest, destpos + 14, Word8Array.sub (src, srcpos + 14));
             Word8Array.update (dest, destpos + 15, Word8Array.sub (src, srcpos + 15)))
         | 20 =>
            (Word8Array.update (dest, destpos, Word8Array.sub (src, srcpos));
             Word8Array.update (dest, destpos + 1, Word8Array.sub (src, srcpos + 1));
             Word8Array.update (dest, destpos + 2, Word8Array.sub (src, srcpos + 2));
             Word8Array.update (dest, destpos + 3, Word8Array.sub (src, srcpos + 3));
             Word8Array.update (dest, destpos + 4, Word8Array.sub (src, srcpos + 4));
             Word8Array.update (dest, destpos + 5, Word8Array.sub (src, srcpos + 5));
             Word8Array.update (dest, destpos + 6, Word8Array.sub (src, srcpos + 6));
             Word8Array.update (dest, destpos + 7, Word8Array.sub (src, srcpos + 7));
             Word8Array.update (dest, destpos + 8, Word8Array.sub (src, srcpos + 8));
             Word8Array.update (dest, destpos + 9, Word8Array.sub (src, srcpos + 9));
             Word8Array.update (dest, destpos + 10, Word8Array.sub (src, srcpos + 10));
             Word8Array.update (dest, destpos + 11, Word8Array.sub (src, srcpos + 11));
             Word8Array.update (dest, destpos + 12, Word8Array.sub (src, srcpos + 12));
             Word8Array.update (dest, destpos + 13, Word8Array.sub (src, srcpos + 13));
             Word8Array.update (dest, destpos + 14, Word8Array.sub (src, srcpos + 14));
             Word8Array.update (dest, destpos + 15, Word8Array.sub (src, srcpos + 15));
             Word8Array.update (dest, destpos + 16, Word8Array.sub (src, srcpos + 16));
             Word8Array.update (dest, destpos + 17, Word8Array.sub (src, srcpos + 17));
             Word8Array.update (dest, destpos + 18, Word8Array.sub (src, srcpos + 18));
             Word8Array.update (dest, destpos + 19, Word8Array.sub (src, srcpos + 19)))
         | _ => unaligned (src, srcpos, endsrc, dest, destpos)

(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		4.	Internal function sixteen

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$ (same as for {\tt unaligned}).
\item{b} $\forall i \in \{srcpos\ldots endsrc-1\}$
          the initial value of $src_i$ is $src0_i$
          (same as for {\tt unaligned}).
\item{c} $srcpos \bmod 4 = destpos \bmod 4 = 0$
\item{d} $(endsrc - srcpos) \bmod 16 = 0$

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip
\noindent
Proof:  We can transform {\tt unaligned} into this function through the
following steps.  None of the steps changes the meaning of the function.

\item{1.} Unroll the loop in {\tt unaligned} four times, to give
\verbatim@
        fun loop (i, j) =
             if i >= endsrc then ()
             else
              (Word8Array.update (dest, j + 0, Word8Array.sub (src, i + 0));
               Word8Array.update (dest, j + 1, Word8Array.sub (src, i + 1));
               Word8Array.update (dest, j + 2, Word8Array.sub (src, i + 2));
               Word8Array.update (dest, j + 3, Word8Array.sub (src, i + 3));
               loop (i + 4, j + 4))
@ The unrolled loop is strongly isomorphic to the original,
since $(endsrc - srcpos) \bmod 16 = 0$, so $(endsrc - srcpos) \bmod 4 = 0$.

\smallskip
\item{2.} Merge the four calls to {\tt Word8Array.sub} and the four calls
to {\tt Word8Array.update} into single calls to {\tt Word32.sub} and
{\tt Word32.update} respectively.
\verbatim@
        fun loop (i, j) =
             if i >= endsrc then ()
             else
              (Word32.update (dest, j + 0, Word32.sub (src, i + 0));
               loop (i + 4, j + 4))
@ Since $srcpos \bmod 4 = destpos \bmod 4 = 0$, this substitution
preserves the meaning of the previous loop.

\smallskip
\item{3.} Unroll the loop again four times (and delete any constant
addition of zero), to give the final code.  The unrolled loop is
strongly isomorphic to the original since $(endsrc - srcpos) \bmod 16 = 0$.

\verbatim@
*)

   local
    fun sixteen (src, srcpos, endsrc, dest, destpos) =
         let fun loop (i, j) =
                  if i >= endsrc then ()
                  else
                   (Word32fox.update (dest, j,      Word32fox.sub (src, i));
                    Word32fox.update (dest, j + 4,  Word32fox.sub (src, i + 4));
                    Word32fox.update (dest, j + 8,  Word32fox.sub (src, i + 8));
                    Word32fox.update (dest, j + 12, Word32fox.sub (src, i + 12));
                    loop (i + 16, j + 16))
         in loop (srcpos, destpos)
         end

(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		5.	Internal function aligned

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$ (same as for {\tt unaligned}).
\item{b} $\forall i \in \{srcpos\ldots endsrc-1\}$
          the initial value of $src_i$ is $src0_i$
          (same as for {\tt unaligned}).
\item{c} $srcpos \bmod 4 = destpos \bmod 4 = srcalign$
\item{d} $srcpos + bytes = endsrc$
\item{e} $bytes \ge 4$

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip
\noindent
Proof:  We can transform {\tt unaligned} into this function through the
following steps.  None of the steps changes the meaning of the function.

\item{1.} Add the {\tt let}:
\verbatim@
    fun aligned (src, srcpos, endsrc, dest, destpos, srcalign, bytes) =
         let val front = case srcalign of 0 => 0 | 1 => 3 | 2 => 2 | 3 => 1
              ...
             val backdest = middest + middle
         in unaligned (src, srcpos, endsrc, dest, destpos)
         end (* let *)
@ This is equivalent to {\tt unaligned} since the {\tt let} only
introduces pure, finite computations.

\item{2.} Split the call to unaligned into:
\verbatim@
     unaligned (src, srcpos, midsrc, dest, destpos);
     sixteen (src, midsrc, backsrc, dest, middest);
     unaligned (src, backsrc, endsrc, dest, backdest)

@ This is equivalent to the original call (since $srcpos\ldots
midsrc\ldots backsrc\ldots endsrc = srcpos\ldots endsrc$,
and likewise for $dest$), provided the preconditions of {\tt sixteen}
are maintained:

\itemitem{a} $midsrc \bmod 4 = middest \bmod 4 = 0$ is true given
precondition $c$ and that $midsrc \bmod 4 = (srcpos + (4 - srcalign))
\bmod 4 = (srcpos + (4 - srcpos \bmod 4)) \bmod 4 = (4 + srcpos -
srcpos) \bmod 4 = 0$.  Likewise for $middest$, since $middest =
destpos + front$ as $midsrc = srcpos + front$ and $srcpos \bmod 4 =
destpos \bmod 4$.

\itemitem{b} $(backsrc - midsrc) \bmod 16 = 0$ is true since $backsrc - midsrc = middle = rest - (rest \bmod 16)$ and $rest \ge 0$
since $rest = bytes - min (x, bytes)$ for some $x > 0$. 

\verbatim@
*)

   in (* local *)
    fun aligned (src, srcpos, endsrc, dest, destpos, srcalign, bytes) =
         let val front = case srcalign of 0 => 0 | 1 => 3 | 2 => 2 | 3 => 1
                                       | _ => raise Illegal_Align_Value_In_Copy
             val rest = bytes - front
             val tail = Bits.andb (rest, 0xf)
             val middle = rest - tail
             val midsrc = srcpos + front
             val middest = destpos + front
             val backsrc = midsrc + middle
             val backdest = middest + middle
         in unaligned (src, srcpos, midsrc, dest, destpos);
            sixteen (src, midsrc, backsrc, dest, middest);
            unaligned (src, backsrc, endsrc, dest, backdest)
         end (* let *)
   end (* local *)

(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		6.	Internal function eightlittle

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$ (same as for {\tt unaligned}).
\item{b} $\forall i \in \{srcpos\ldots endsrc-1\}$
          the initial value of $src_i$ is $src0_i$
          (same as for {\tt unaligned}).
\item{c} $(srcpos + 2) \bmod 4 = destpos\bmod 4 = 0$
\item{d} $endsrc > srcpos$
\item{e} $(endsrc - srcpos) \bmod 8 = 2$
\item{f} $\hbox{\tt Word16.update} (a, 0, x);
          \hbox{\tt Word16.update} (a, 2, y) \simeq
          \hbox{\tt Word32.update} (a, 0, x | y \ll16)$

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip
\noindent
Proof:  We can transform {\tt unaligned} into this function through the
following steps.  None of the steps changes the meaning of the function.

\item{1.} Unroll the loop eight times, and since $(endsrc - srcpos)
\bmod 8 = 2$, add a case at the beginning to handle the additional two
bytes.  Then merge each pair of {\tt Word8Array.sub} and of {\tt
Word8Array.update} operations into the corresponding {\tt Word16} operations
(legitimate since $srcpos \bmod 2 = i \bmod 2 = destpos \bmod 2 = j
\bmod 2 = 0$).
\verbatim@
        let fun loop (i, j) =
                 if i >= endsrc then ()
                 else (Word16.update (dest, j + 0, Word16.sub (src, i + 0));
                       Word16.update (dest, j + 2, Word16.sub (src, i + 2));
                       Word16.update (dest, j + 4, Word16.sub (src, i + 4));
                       Word16.update (dest, j + 6, Word16.sub (src, i + 6));
                       loop (i + 8, j + 8))
         in Word16.update (dest, destpos, Word16.sub (src, srcpos));
            loop (srcpos + 2, destpos + 2)
         end
@

\filbreak

\item{2.} Introduce a carry, and offset $i$ by two so now $j = i +
destpos - srcpos - 2$.  To keep equivalence with {\tt unaligned}, read
the first two bytes before the loop and assign the final carry at the
end of the loop.  Note that in this loop, $i \bmod 4 = (srcpos + 2)
\bmod 4 = 0$ and $j \bmod 4 = destpos \bmod 4 = 0$.
\verbatim@
        let fun loop (i, j, carry) =
                 if i >= endsrc then Word16.update (dest, j, carry)
                 else (Word16.update (dest, j + 0, carry);
                       Word16.update (dest, j + 2, Word16.sub (src, i + 0));
                       Word16.update (dest, j + 4, Word16.sub (src, i + 2));
                       Word16.update (dest, j + 6, Word16.sub (src, i + 4));
                       loop (i + 8, j + 8, Word16.sub (src, i + 6)))
             in loop (srcpos + carrysize, destpos, Word16.sub (src, srcpos)))
@

\filbreak

\item{3.} Replace the first two {\tt Word16.update} and {\tt Word16.sub}
operations by corresponding {\tt Word32} operations (legitimate since
$i \bmod 4 = j \bmod 4 = 0$).  Use precondition {\it f} to compute the
value $src1$ to be stored.
\verbatim@
    val makebyte2 = Word16.fromInt o Word32.toInt
    val makebyte4 = Word32.fromInt o Word16.toInt
    fun mergel (low, high) = Word32.orb (low, Word32.<< (high, 16))
           ...
            else
             let val srcv = Word32.sub (src, i)
             in Word32.update (dest, j, mergel (carry, srcv));
                let val carry = makebyte2 (Word32.>> (srcv, 16))
                in Word16.update (dest, j + 4, carry);
                   Word16.update (dest, j + 6, Word16.sub (src, i));
                   loop (i + 8, j + 8, makebyte4 (Word16.sub (src, i + 6)))
                end
             end
           ...
@

\filbreak

\item{4.} Replace the remaining {\tt Word16} operations by the corresponding
{\tt Word32} operations, again legal since $i \bmod 4 = j \bmod 4 = 0$ and
using precondition {\it f} to compute the value $src2$ to be stored.
\verbatim@
             let val srcv = Word32.sub (src, i)
             in Word32.update (dest, j, mergel (carry, srcv));
                let val carry = Word32.>> (srcv, 16)
                    val srcv = Word32.sub (src, i + 4)
                in Word32.update (dest, j + 4, mergel (carry, srcv));
                   loop (i + 8, j + 8, Word32.>> (srcv, 16))
                end
             end
@

\filbreak

\item{5.} In-line the calls to mergel and compute
new $i$, $j$ as $i+4$, $j+4$.
\verbatim@
                  ...
                   let val srcv = Word32.sub (src, i)
                   in Word32.update (dest, j,
                                    Word32.orb (carry, Word32.<< (srcv, 16)));
                      let val i = i + 4
                          val j = j + 4
                          val carry = Word32.>> (srcv, 16)
                          val srcv = Word32.sub (src, i)
                      in Word32.update (dest, j,
                                       Word32.orb (carry, Word32.<< (srcv, 16)));
                         loop (i + 4, j + 4, Word32.>> (srcv, 16))
                  ...
@
\filbreak

This gives us the final code for {\tt eightlittle}.

\verbatim@
*)

   local
    val carrysize = 2

    val makebyte2 = Word16.fromInt o Word32.toInt
    val makebyte4 = Word32.fromInt o Word16.toInt

    fun eightlittle (src, srcpos, endsrc, dest, destpos) =
         let fun loop (i, j, carry) =
                  if i >= endsrc then Word16.update (dest, j, makebyte2 carry)
                  else
                   let val srcv = Word32fox.sub (src, i)
                   in Word32fox.update (dest, j,
					 Word32.orb (carry, Word32.<< (srcv, 0w16)));
                      let val i = i + 4
                          val j = j + 4
                          val carry = Word32.>> (srcv, 0w16)
                          val srcv = Word32fox.sub (src, i)
                      in Word32fox.update (dest, j,
                                       Word32.orb (carry, Word32.<< (srcv, 0w16)));
                         loop (i + 4, j + 4, Word32.>> (srcv, 0w16))
                      end
                   end
         in loop (srcpos + carrysize, destpos,
                  makebyte4 (Word16.sub (src, srcpos)))
         end
(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		7.	Internal function eightbig

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$ (same as for {\tt unaligned}).
\item{b} $\forall i \in \{srcpos\ldots endsrc-1\}$
          the initial value of $src_i$ is $src0_i$
          (same as for {\tt unaligned}).
\item{c} $(srcpos + 2) \bmod 4 = destpos\bmod 4 = 0$
\item{d} $endsrc > srcpos$
\item{e} $(endsrc - srcpos) \bmod 8 = 2$
\item{f} $\hbox{\tt Word16.update} (a, 0, x);
          \hbox{\tt Word16.update} (a, 2, y) \simeq
          \hbox{\tt Word32.update} (a, 0, x \ll 16 | y)$

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip \noindent
Proof: The proof follows that of {\tt eightlittle}, except the
carry is left-shifted, and the precondition {\it f} is reversed.
This reversal is expressed in the code by the calls to {\tt mergeb}
instead of {\tt mergel} and by the use of $\ll$ instead of $\gg$.

\verbatim@
*)

   (* not tested. *)
    fun eightbig (src, srcpos, endsrc, dest, destpos) =
         let fun loop (i, j, carry) =
                  if i >= endsrc then
                   Word16.update
		     (dest, j, makebyte2 (Word32.>> (carry, 0w16)))
                  else
                   let val srcv = Word32fox.sub (src, i)
                   in Word32fox.update (dest, j,
                                    Word32.orb (carry, Word32.>> (srcv, 0w16)));
                      let val i = i + 4
                          val j = j + 4
                          val carry = Word32.<< (srcv, 0w16)
                          val srcv = Word32fox.sub (src, i)
                       in Word32fox.update (dest, j,
                                        Word32.orb (carry, Word32.>> (srcv, 0w16)));
                          loop (i + 4, j + 4, Word32.<< (srcv, 0w16))
                       end
                   end
         in loop (srcpos + carrysize, destpos,
                  Word32.<< (makebyte4 (Word16.sub (src, srcpos)),
				    0w16))
         end

(*
@
% ---------------------------------------------------------------------
\bigskip\hrule\bigskip
\noindent
		8.	Internal function eight

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$ (same as for {\tt unaligned}).
\item{b} $\forall i \in \{srcpos\ldots endsrc-1\}$
          the initial value of $src_i$ is $src0_i$
          (same as for {\tt unaligned}).
\item{c} $(srcpos + 2) \bmod 4 = destpos\bmod 4 = 0$
\item{d} $endsrc > srcpos$
\item{e} $(endsrc - srcpos) \bmod 8 = 2$

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip
\noindent
Proof: If $\hbox{\tt Word32.bigEndian} = \hbox{false}$,\par\noindent
then $\hbox{\tt Word16.update} (a, 0, x); \hbox{\tt Word16.update} (a, 2, y)
\simeq \hbox{\tt Word32.update} (a, 0, x | y \ll16)$ (asserted without proof)
and $\hbox{\tt eight} \simeq \hbox{\tt eightlittle}$, as implemented
by the code; otherwise, \par\noindent
$\hbox{\tt Word16.update} (a, 0, x); \hbox{\tt Word16.update} (a, 2, y) \simeq
\hbox{\tt Word32.update} (a, 0, x \ll 16 | y)$ (asserted without proof)
and $\hbox{\tt eight} \simeq \hbox{\tt eightbig}$, as implemented by the code.

\verbatim@
*)

    val eight = if Word32fox.bigEndian then eightbig else eightlittle

(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		9.	Internal function semialigned

\medskip
\noindent
Preconditions:
\item{a} $src \ne dest$ (same as for {\tt unaligned}).
\item{b} $\forall i \in \{srcpos\ldots endsrc-1\}$
          the initial value of $src_i$ is $src0_i$
          (same as for {\tt unaligned}).
\item{c} $srcalign = srcpos \bmod 4 = (destpos + 2) \bmod 4$
\item{d} $endsrc - srcpos = bytes$
\item{e} $bytes \ge 12$

\medskip
\noindent
Specification:  Same as for {\tt unaligned}.

\medskip
\noindent
Proof:  We can transform {\tt unaligned} into this function through the
following steps.  None of the steps changes the meaning of the function.

\item{1.} Add the {\tt let}:
\verbatim@
    fun semialigned (src, srcpos, endsrc, dest, destpos, srcalign, bytes) =
         let val front = case srcalign of 0 => 2 | 2 => 0 | 1 => 3 | 3 => 1
              ...
             val backdest = middest + middle
         in unaligned (src, srcpos, endsrc, dest, destpos)
         end (* let *)
@ This is equivalent to {\tt unaligned} since the {\tt let} only
introduces pure, finite computations.

\item{2.} Split the call to unaligned into:
\verbatim@
     unaligned (src, srcpos, midsrc, dest, destpos);
     eight (src, midsrc, backsrc, dest, middest);
     unaligned (src, backsrc, endsrc, dest, backdest)

@ This is equivalent to the original call (since $srcpos\ldots
midsrc\ldots backsrc\ldots endsrc = srcpos\ldots endsrc$,
and likewise for $dest$), provided the preconditions of {\tt eight}
are maintained:

\itemitem{a.} $(midsrc + 2) \bmod 4 = middest \bmod 4 = 0$ is true
given precondition $c$ and that $(midsrc + 2)\bmod 4 = (srcpos + 2 +
front) \bmod 4 = (srcpos + 2 + (2 - srcalign)) \bmod 4 = (srcpos + 2 +
(2 - srcpos \bmod 4)) \bmod 4 = (2 + 2 + srcpos - srcpos) \bmod 4 =
0$.  Likewise $middest \bmod 4 = 0$, since $middest = destpos +
front$ as $midsrc = srcpos + front$ and $(srcpos + 2) \bmod 4 =
destpos \bmod 4$.

\itemitem{b.} $backsrc > midsrc$ is true since $backsrc - midsrc =
middle$ and $front + middle + tail = bytes \ge 12$ (by precondition
{\it e}), and since $front < 4$ and $tail < 8$, so $middle \ge 2$.

\itemitem{c.} $(backsrc - midsrc) \bmod 8 = 2$ is true since
$backsrc - midsrc = middle = rest - ((rest - 2) \bmod 8)$)
and $rest \ge 2$ since $rest \ge middle \ge 2$. 

\verbatim@
*)

   in (* local *)
    fun semialigned (src, srcpos, endsrc, dest, destpos, srcalign, bytes) =
         let val front = case srcalign of 0 => 2 | 2 => 0 | 1 => 1 | 3 => 3
                                       | _ => raise Illegal_Align_Value_In_Copy
             val rest = bytes - front
             val tail = Bits.andb (rest - 2, 0x7)
             val middle = rest - tail
             val midsrc = srcpos + front
             val middest = destpos + front
             val backsrc = midsrc + middle
             val backdest = middest + middle
         in unaligned (src, srcpos, midsrc, dest, destpos);
            eight (src, midsrc, backsrc, dest, middest);
            unaligned (src, backsrc, endsrc, dest, backdest)
         end (* let *)
   end (* local *)

(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		10.	Function illegal

\medskip
\noindent
Specification:

\item{} $\hbox{\tt illegal} \simeq \hbox{\tt raise Illegal\_Copy}$
\item\item{}{\bf Proof:}
            If we ignore the print statement, clearly
            $\hbox{\tt illegal} \simeq \hbox{\tt raise Illegal\_Copy}$.
            However, the print statement terminates and modifies no
            existing memory,
            so it does not change the semantics of {\tt illegal}.

\verbatim@
*)

   fun illegal (src, srcpos, bytes, dest, destpos) =
        (print ("copy.fun: illegal copy (" ^
                Int.toString (Word8Array.length src) ^ ", " ^
                Int.toString srcpos ^ ", " ^
                Int.toString bytes ^ ", " ^
                Int.toString (Word8Array.length dest) ^ ", " ^
                Int.toString destpos ^ ")\n");
         raise Illegal_Copy {source_length = Word8Array.length src,
                             source_offset = srcpos,
                             bytes = bytes,
                             dest_length = Word8Array.length dest,
                             dest_offset = destpos})
(*
@
% ---------------------------------------------------------------------
\vfill\eject\noindent
		11.	Function copy

        Generic copy. We would like this to be correct and highly
        optimized.

Preconditions:
\item{a} $src \ne dest$ (if $src = dest$, the meaning of the function
is undefined)).

\medskip
\noindent
Specification:

\item{1} if $bytes \le 0$ then $\hbox{\tt copy} \simeq \hbox{null function}$.

\item{2} if $srcpos < 0 \vee destpos < 0$
         then $\hbox{\tt copy} \simeq \hbox{\tt raise Illegal\_Copy}$
         (partial copies to $dest$ are allowed but not required).

\item{3} if $\exists i \in \{srcpos\ldots srcpos+bytes-1\} : i \ge length(src)$
         then $\hbox{\tt copy} \simeq \hbox{\tt raise Illegal\_Copy}$
         (partial copies to $dest$ are allowed but not required).

\item{4}
if $\exists j \in \{destpos\ldots destpos+bytes-1\} : j \ge length(dest)$
then $\hbox{\tt copy} \simeq \hbox{\tt raise Illegal\_Copy}$
         (partial copies to $dest$ are allowed but not required).

\item{5} otherwise, assuming $src0_i$ is the value at index $i$ of $src$
   before the call, then after the call 
   \item{}$\forall i \in \{0\ldots bytes-1\}
     (src_{srcpos+i} = src0_{srcpos+i} = dest_{destpos+i})$

\medskip
\noindent
Assumptions:
\item{} The computations cause no arithmetic overflow.
\item{} Any out-of-bounds array access will raise
        the exception {\tt Subscript}.

\medskip
\noindent
Proof:
\item{1, 5}
       Are satisfied by specification clauses 1 and 5 respectively of
       {\tt unaligned}, given that $endsrc$ in {\tt unaligned} is
       computed to be $srcpos+bytes$ and that $\hbox{\tt aligned}
       \simeq \hbox{\tt semialigned} \simeq \hbox{\tt unaligned}
       \simeq \hbox{\tt common}$
       (subject to $(srcpos \bmod 4 = destpos \bmod 4 = srcalign \wedge
       bytes \ge 4)$
       for {\tt aligned} and
       $(srcalign = srcpos \bmod 4 = (destpos + 2) \bmod 4 \wedge
       bytes \ge 12)$ for {\tt semialigned}),
       and that every branch of {\tt copy} ends
       in one of these functions and {\tt copy} does not
       otherwise modify memory.

\item{2, 3, 4} Are satisfied since they trigger (using $endsrc = srcpos+bytes$)
       clauses 2, 3, and 4 of {\tt unaligned}, so
       $\hbox{\tt unaligned} \simeq \hbox{\tt raise Subscript}$ (neglecting
       possible changes to $dest$) and in this case
       $\hbox{\tt copy} \simeq \hbox{\tt illegal} \simeq %
                 \hbox{\tt raise Illegal\_Copy}$ (plus possibly
       some assignment to $dest$).

\bigskip

\verbatim@
*)
  in (* local *)

   fun copy (src, srcpos, bytes, dest, destpos) =
        (if bytes < 25 then
 (* cannot call aligned or semialigned with less than 12 bytes,
    but we go to 25 to optimize common cases. *)
          common (src, srcpos, srcpos + bytes, dest, destpos)
         else
          let val srcalign = Bits.andb (srcpos, 0x3)
              val destalign = Bits.andb (destpos, 0x3)
              val endsrc = srcpos + bytes
          in if srcalign = destalign then
              aligned (src, srcpos, endsrc, dest, destpos, srcalign, bytes)
             else if Bits.andb (srcalign + destalign, 0x1) = 0 then
              semialigned (src, srcpos, endsrc, dest, destpos, srcalign, bytes)
             else
              unaligned (src, srcpos, endsrc, dest, destpos)
          end)
           handle Subscript => illegal (src, srcpos, bytes, dest, destpos)

  end (* local *)

 end (* struct *)

(*
@
\bye
*)




