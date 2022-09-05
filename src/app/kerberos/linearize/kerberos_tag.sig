(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a specialization of the LINEARIZE signature
	to kerberos_tags.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature KERBEROS_TAG_LINEARIZE

		iii.	RCS Log
	
$Log: kerberos_tag.sig,v $
Revision 1.1  1994/08/25  10:24:41  robby
Initial revision

Revision 1.1  1994/07/14  20:29:47  robby
Initial revision

Revision 1.1  94/07/13  18:48:58  robby
Initial revision


	1.	signature KERBEROS_TAG_LINEARIZE
*)
signature KERBEROS_TAG_LINEARIZE=
sig
  include LINEARIZE

  datatype Auth_Msg_Type=Auth_Request | Auth_Reply | Err_Reply |
                         Appl_Request | Appl_Err

  datatype Endian=Big | Little

 (* This holds the version and auth_msg_type fields *)
  datatype kerberos_tag=
    Kerberos_Tag of ubyte1 * Auth_Msg_Type * Endian

  sharing type T=kerberos_tag
end
