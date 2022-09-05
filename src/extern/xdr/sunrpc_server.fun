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

	Implements the SUN RPC server.

	ii.	Table of Contents

	1.      functor SunRPC_server

	iii.	RCS Log

$Log: sunrpc_server.fun,v $
Revision 1.2  1994/10/15  10:18:52  kcchiang
fixed initialization and finalize routines

Revision 1.1  94/10/14  11:59:59  kcchiang
Initial revision

*)
(*
	1.      functor SunRPC_server
*)
functor SunRPC_server (structure B           : FOX_BASIS
		       structure SunRPC_prot : BUILD_SUNRPC_PROT
		       val do_print          : bool
		       val local_interface   : string
		       val local_ip          : Byte4.ubytes
		       sharing type SunRPC_prot.Prot.outgoing_message 
			            = B.Send_Packet.T
			   and type SunRPC_prot.Prot.incoming_message 
			            = B.Receive_Packet.T): SUNRPC_SERVER =
struct
    type Remote_Function = bytestring -> bytestring
    type Proc_ID         = int*int*int	(* prog,ver,proc *)

    (* Network stuff
       ============= *)
    structure Ip       = SunRPC_prot.Ip
    structure Prot     = SunRPC_prot.Prot	(* either UDP or TCP *)
    val header_size    = SunRPC_prot.prot_header_size

    val address_pattern = SunRPC_prot.address_pattern


    (* RPC stuff
       ========= *)
    structure XInt = XInt ()
    structure Xvoid = XVoid ()
    structure S = SunRPC(structure Remote_Proc_Params = Xvoid
			 structure Remote_Proc_Result = Xvoid)
    val void = ()

    val RPC_version   = S.version		(* version supported *)
    val Low_RPC_vers  = S.version
    val High_RPC_vers = S.version

    val AuthFlavor = S.auth_flavor.AUTH_NULL
    val Null_Auth = (AuthFlavor,ByteArray.array (0,0))



    (* Table lookup stuff - for maintaining remote procedures
       ====================================================== *)
    (* table to maintain remote programs   *)
    val prog_table = ref []: (Proc_ID * Remote_Function) list ref

    (* table to maintain remote programs' supported versions *)
    val proc_ver_table = ref []:((int*int)*(int*int)) list ref

    exception NotFoundInTable
    fun lookup table elem =
    let fun helper [] _ = raise NotFoundInTable
	  | helper ((x,y)::xd) a = if a=x then y else helper xd a
    in
	helper (!table) elem
    end

    fun delete table x = 
    let fun helper [] x = raise NotFoundInTable
	  | helper ((a,b)::c) x =
	    if x=a then c
	    else helper c x
    in
	table := helper (!table) x
    end

    fun add table x =
	table := x :: (!table)

    fun update table (x,y) =
	(delete table x;
	 add table (x,y))

    (* for debugging purposes *)
    fun print_ver_table () =
    let val table = ! proc_ver_table
	fun help [] = B.V.print "]\n"
	  | help (((x,y),(b,c))::xd) = 
	    let val prn_str = 
		" ((" ^ (B.V.Integer.makestring x) ^ "," ^
		(B.V.Integer.makestring y) ^ "),(" ^
		(B.V.Integer.makestring b) ^ "," ^
		(B.V.Integer.makestring c) ^ ")) "
	    in
		B.V.print prn_str;
		help xd
	    end
    in
	B.V.print "[";
	help table
    end

    fun print_table () =
    let val table = ! prog_table
	fun help [] = B.V.print "]\n"
	  | help (((a,b,c),f)::xd) = 
	    let val prn_str =
		" (" ^ (B.V.Integer.makestring a) ^ "," ^
		(B.V.Integer.makestring b) ^ "," ^
		(B.V.Integer.makestring c) ^ ") "
	    in
		B.V.print prn_str;
		help xd
	    end
    in
	B.V.print "[";
	help table
    end



    (* Main server routine
       =================== *)
    fun handler connection incoming =
    let val _ = if do_print 
		    then B.V.print "Server handler invoked\n"
		else ()
	val bytestr = (B.Receive_Packet.read incoming, 0)
	val (call_msg, bytestr) = S.unmarshall bytestr
	val send = Prot.send connection

	exception IllegalCall

	fun parse_call (xid,S.Body.T1(rpcvers,prog,vers,proc,cred,verf,void)) =
	let exception WrongVersion
	in 
	    if do_print then
	      let val print_str = 
		  "rpcvers = " ^ (B.V.Integer.makestring rpcvers) ^
		  "\nprog = " ^ (B.V.Integer.makestring prog) ^
		  "\nvers = " ^ (B.V.Integer.makestring vers) ^
		  "\nproc = " ^ (B.V.Integer.makestring proc) ^ "\n"
	      in
		  B.V.print print_str
	      end
	    else ();

	   if rpcvers <> RPC_version 
	   then
	     let val res = S.Body.T2 (S.ReplyBody.T2
				      (S.RejectedReply.T1
				       (Low_RPC_vers,High_RPC_vers)))
		 val (reply,_) = 
		     S.Body.marshall(res,(ByteArray.array(S.Body.size res,0),0))
	     in
		 (xid,reply)		(* Wrong SUNrpc version *)
	     end
	   else 
	     let val f = lookup prog_table (prog,vers,proc)
		 val (reply_body,_) = f bytestr
		 val body_size = ByteArray.length reply_body
		 val reply_header = S.Body.T2 (S.ReplyBody.T1 
					       (Null_Auth,S.ReplyData.T1 ()))
		 val reply_header_size = S.Body.size reply_header
		 val (reply,_) = 
		     S.Body.marshall 
		     (reply_header,(ByteArray.array
				    (reply_header_size+body_size,0),0))
	     in
		 B.Copy.copy (reply_body,0,body_size,reply,reply_header_size);
		 (xid,reply)		(* Valid result *)
	     end
	     handle NotFoundInTable =>
	       let val v = lookup proc_ver_table (prog,proc)
		   val res = S.Body.T2 (S.ReplyBody.T1 
					(Null_Auth, S.ReplyData.T2 v))
		   val (reply,_) =
		       S.Body.marshall
		       (res,(ByteArray.array(S.Body.size res,0),0))
	       in
		   (xid,reply)		(* wrong proc version *)
	       end
	       handle NotFoundInTable =>
		 let val res = 
		     S.Body.T2 (S.ReplyBody.T1 (Null_Auth, S.ReplyData.T3 ()))
		       val (reply,_) =
			   S.Body.marshall
			   (res,(ByteArray.array(S.Body.size res,0),0))
		   in
		       (xid,reply)	(* Garbage arguments etc. *)
		   end
	end
	  | parse_call _ = raise IllegalCall
	      
        fun answer call =
	let val (xid,res) = parse_call call
	    val res_size  = ByteArray.length res
	    val int_size  = XInt.size xid
	    val (reply,_) = XInt.marshall 
		(xid,(ByteArray.array(int_size + res_size,0),0))
	    val _ = Fox_Basis.Copy.copy
		(res,0,res_size,reply,int_size)
	in
	    if do_print then
		(B.V.print "# Server response = ";
		 B.V.Print.print_byte1Uarray res;
		 B.V.print "\n")
	    else ();

	    send (B.Send_Packet.create (reply,header_size));
	    Prot.close connection;
	    ()
	end
        handle IllegalCall => ()		(* ignore *)
    in
	answer call_msg
    end



    (* Remote procedures installation/uninstallation
       ============================================= *)
    exception ProcAlreadyExists
    exception NoSuchProc

    (* install a remote procedure *)
    fun install (prog,vers,proc) f = 
    let val _ = lookup prog_table (prog,vers,proc)
    in
	raise ProcAlreadyExists
    end
    handle NotFoundInTable =>
	let val _ = add prog_table ((prog,vers,proc),f)
	    val (low_v,high_v) = lookup proc_ver_table (prog,proc)
	in
	    if (vers < low_v) then
		update proc_ver_table ((prog,proc),(vers,high_v))
	    else
		if (vers > high_v) then
		    update proc_ver_table ((prog,proc),(low_v,vers))
		else ()
	end
        handle NotFoundInTable =>
	    add proc_ver_table ((prog,proc),(vers,vers))


    (* uninstall a remote procedure *)
    fun uninstall (prog,vers,proc) =
    let val _ = delete prog_table (prog,vers,proc)
	val (low_v,high_v) = lookup proc_ver_table (prog,proc)
    in
	if (vers = low_v) andalso (high_v <> low_v) then
	    update proc_ver_table ((prog,proc),(low_v+1,high_v))
	else 
	    if (vers = high_v) andalso (high_v <> low_v) then
		update proc_ver_table ((prog,proc),(low_v,high_v-1))
	    else delete proc_ver_table (prog,proc)
    end
    handle NotFoundInTable => raise NoSuchProc
    



    (* Listener : loops until connection established
       ============================================= *)
    val state = ref (NONE:int option)	(* this is used to tell the *)
					(* server when to die       *)
    fun listen () =
    let val _ = (SOME (Prot.passive_open (address_pattern, handler))
		 handle _ => NONE)
    in
	case !state of 
	    NONE => ()
	  | SOME _ => listen ()
    end



    (* Server init routine 
       =================== *)
    exception InitializationFailed

    fun initialize () =
     case !state of
       NONE =>
	   (Prot.initialize ()
	    handle _ => raise InitializationFailed;
	    Ip.control (Ip.Set_Interface_Address {interface = local_interface,
						  ip_number = local_ip});
	    state := SOME 1;
	    B.Scheduler.fork (B.Scheduler.Normal listen);	   
	    if do_print then
		B.V.print "Server ready\n"
	    else ();
	    1)				(* return initialization count *)
	   
     | SOME x =>
	   (if do_print then
		B.V.print "Server has already been initialized\n"
	    else ();
	    state := SOME (x+1);	(* increase init count *)
	    x+1)


    (* Server shut down routine
       ======================== *)
    fun finalize () =
     case !state of
       NONE =>
	   (if do_print then
		B.V.print "Server is not running\n"
	    else ();
	    0)
     | SOME x =>
	   (if x>1 then
		(state := SOME (x-1);
		 x-1)
	    else (Prot.finalize ();
		  state := NONE;
		  if do_print then
		      B.V.print "Server shut down\n"
		  else ();
		  0))


    (* Server timer start routine
       ========================== *)
    val start = B.Scheduler.sleep

end (* struct *)
