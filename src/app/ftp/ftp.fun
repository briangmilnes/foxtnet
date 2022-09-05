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

	Implementation of a subset of FTP (see RFC 959).  This module
	is intended to be used by other programs, not directly by the user.

	Still to do:
	    Sensible treatment of ctrl-C:
	      if currently executing a command, abort it
		  (see RFC pages 31-32 and 35)
	      if currently waiting for user input, treat as a null command


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ftp

		iii.	RCS Log
	
$Log: ftp.fun,v $
Revision 1.2  1994/10/27  10:24:54  cokasaki
better dns support, added Remote_Operation_Failed exception

Revision 1.1  94/08/25  11:54:52  cokasaki
Initial revision
	

*)

(*
	1.	functor Ftp
*)




functor Ftp (structure B : FOX_BASIS

	     structure Ip : IP_PROTOCOL
	     structure Tcp : TCP_PROTOCOL
	     structure Dns : DNS
	     sharing type Ip.ip_number = ubyte4
	     sharing type Tcp.port = ubyte2
	     sharing type Tcp.lower_layer_address = Ip.ip_number
	     sharing type Tcp.incoming = B.Dyn_Array.T
	     sharing type Tcp.outgoing = B.Dyn_Array.T

	     val verbose   : bool
	     val do_prints : bool) : FTP =
struct

  (* bitwise operations *)
  infix && >>

  (* debugging *)
  val debug_print =
        if do_prints then
	    fn s =>
	      if s <> "" andalso
		 B.V.String.substring(s,B.V.String.length s - 1,1) = "\n"
	      then B.V.print ("fftp: " ^ s)
	      else B.V.print ("fftp: " ^ s ^ "\n")
	else
	    fn s => ()

  (* verbose *)
  val verbose_print =
        if verbose then fn s => B.V.print s
	           else fn s => ()

  structure Pipe = B.Pipe
  structure DArray = B.Dyn_Array

  exception Initialization_Failed of string

  fun initialize () =
    (Tcp.initialize (); Dns.initialize ())
    handle
      Tcp.Initialization_Failed x => raise Initialization_Failed x
    | Dns.Tcp_Or_Udp.Initialization_Failed x => raise Initialization_Failed x

  fun finalize () = (Dns.finalize (); Tcp.finalize ())

  exception Connection_Failed of string

  exception Remote_Operation_Failed of string

  val dns = ref (Dns.default_authorities ())
  (* Warning: There something odd here since Dns is not initialized *)
  (* at the time of this call, and furthermore, this value might be *)
  (* be preserved across initialization/finalization boundaries.    *)
  (* If Dns changes so that this is not kosher, than the address    *)
  (* queries must simply be called with the default_authorities     *)
  (* each time.                                                     *)

  fun connect server =
  let val _ = debug_print ("Connecting to " ^ server ^ ".")

      val server_ip =
	let val _ = debug_print "Querying name server."
	    val (dns',result) = Dns.address_query (!dns,server)
	in
	    dns := dns';
	    case result of
		SOME ip => ip
	      | NONE => raise Connection_Failed ("Unknown server: " ^ server ^ ".")
	end
(*
	let val dns = Dns.default_authorities ()
	in
	    debug_print "Querying name server.";
	    case Dns.address_query (dns,server) of
		(_,SOME ip) => ip
	      | (_,NONE) => raise Connection_Failed ("Unknown server: " ^ server ^ ".")
	end
*)

(*
      (* loch for now *)
      val server_ip = ((4u128 * 4u256 + 4u2) * 4u256 + 4u206) * 4u256 + 4u49
*)
      val server_control_port = 2u21
      val server_data_port = 2u20

      (* condition variables *)
      type condition_variable = unit Pipe.T
      fun new_cvar () = Pipe.new NONE : condition_variable
      fun wait cvar = Pipe.dequeue cvar
      fun signal cvar = Pipe.enqueue (cvar, ())

      (* buffer incoming packets and convert to bytes *)

      val buffer = Pipe.new NONE : (Tcp.incoming * int) option Pipe.T

      exception BufferEmpty

      fun close_buffer _ = 
	  (debug_print "Server closing control connection.";
	   Pipe.enqueue (buffer,NONE))

      fun write_buffer pkt =
	  (debug_print "Got control packet.";
	   verbose_print (B.Access.to_string (DArray.read pkt));
	   flush_out std_out;
           if DArray.size pkt > 0 then Pipe.enqueue (buffer, SOME (pkt,0))
	                          else ())

      fun read_buffer () =
	    case Pipe.dequeue buffer of
	      SOME (pkt,i) =>
		Byte1.to_int (DArray.sub1 (pkt,i))
		before (if i+1 < DArray.size pkt
			then Pipe.requeue (buffer,SOME (pkt,i+1))
			else ())
	    | NONE => raise BufferEmpty

      val control_connection =
            Tcp.connect(Tcp.Remote_Specified
			      {peer = server_ip,
			       remote_port = server_control_port},
			Tcp.Handler (fn _ =>
			      (debug_print "Connection established.";
			       (write_buffer,close_buffer))))
	                (* The ftp server shouldn't ever send any urgent *)
	                (* data, so I don't bother to check for it       *)

(*
      val Tcp.Key {local_port = my_port,...} =
	    Tcp.connection_key control_connection
*)
      val data_address =
	    SOME (Tcp.Remote_Specified {peer = server_ip,
					remote_port = server_data_port})

      fun send msg =
	    let val n = String.size msg
		val (pkt,sendit) = Tcp.allocate_send (control_connection,n)
		val data = DArray.init (B.Access.from_string msg)
	    in
		DArray.copy (data,0,n,pkt,0);
		debug_print ("Sending " ^ msg);
		sendit ();
		debug_print ("Sent " ^ msg)
	    end

      fun get_reply () =
	    let val DONT = chr 255 ^ chr 254
		val WONT = chr 255 ^ chr 252
		fun getc () =
		  case read_buffer () of
		    255 => (* TELNET IAC *)
		      (case read_buffer () of
			   251 => (* TELNET WILL *)
			     let val option = read_buffer ()
			     in
				 send (DONT ^ chr option)
			     end
			 | 253 => (* TELNET DO *)
			     let val option = read_buffer ()
			     in
				 send (WONT ^ chr option)
			     end
			 | 252 => (* TELNET WON'T *)
			     (read_buffer (); ()) (* discard option *)
			 | 254 => (* TELNET DON'T *)
			     (read_buffer (); ()) (* discard option *)
			 | _ => (); (* ignore other TELNET commands *)
		       getc ())
		  | c => c

	        val d1 = getc ()
	        val d2 = getc ()
		val d3 = getc ()

		val eol  = ord "\n"
	        val spc  = ord " "
		val zero = ord "0"
		val dash = ord "-"

		fun skip () = (* read to end of line *)
		      if getc () = ord "\n"
			  then ()
			  else skip ()

		fun multiskip () = (* read multi-line responses *)
		      if          getc () = d1 
			  andalso getc () = d2 
			  andalso getc () = d3
			  andalso getc () = ord " "
		      then skip ()
		      else (skip (); multiskip ())

	    in
		if getc () = dash
		    then (skip (); multiskip ())
		    else skip ();
		(* major return code, minor return code *)
		(d1 - zero, 10*(d2-zero) + (d3-zero))
	    end

      
      val CRLF = chr 13 ^ chr 10

      fun command cmd = (send (cmd ^ CRLF); get_reply ())

      fun login () =
	let fun ready () = (* is server ready? *)
	          case get_reply () of
		      (1,_) => ready () (* connection delayed.  try again *)
		    | (2,_) => user ()  (* connection established *)
		    | _ => raise Connection_Failed "Unexpected greeting from server."
	    and user () =
		  case command "USER anonymous" of
		      (2,_) => ()
		    | (3,31) => password ()
		    | _ => raise Connection_Failed "Anonymous login failed."
	    and password () =
		  case command "PASS foxnet@cs.cmu.edu" of
		      (2,_) => ()
		    | _ => raise Connection_Failed "Anonymous password not accepted."
	in
	    debug_print "Logging in.";
	    ready ()
	    handle
	      BufferEmpty =>
		(Tcp.close control_connection handle _ => ();
		 raise Connection_Failed "Server closed connection.")
	    | exn =>
		(Tcp.close control_connection handle _ => ();
		 raise exn)     
	end

      fun error msg = raise Remote_Operation_Failed msg

      fun unexpected c (r,r') =
	    error ("Unexpected return code "
		   ^ B.V.Integer.makestring (100*r + r')
		   ^ " for " ^ c ^ ".")

      fun losing_connection () =
	    (Tcp.close control_connection handle _ => ();
	     error "Server closed connection.")

      datatype type_of_data = ASCII | BINARY

      fun typestring ASCII  = "TYPE A"
	| typestring BINARY = "TYPE L 8"

      val current_type = ref ASCII

      fun settype t =
	    if !current_type = t then ()
	    else case command (typestring t) of
		     (2,_) => current_type := t
		   | (4,_) => losing_connection ()
		   | (5,4) => error "Server does not support binary mode."
		   | response => unexpected "TYPE" response

      val port_prefix =
	    case Ip.local_address server_ip of
	      NONE => raise Connection_Failed "Couldn't find local ip number."
	    | SOME (_,my_ip) =>
		  let val string = B.V.Integer.makestring o Byte4.to_int o
		                     (fn w => w && 4u255)
		  in
		      "PORT " ^
		      string (my_ip >> 24) ^ "," ^
		      string (my_ip >> 16) ^ "," ^
		      string (my_ip >> 8)  ^ "," ^
		      string my_ip         ^ ","
		  end

      fun setport p =
	    let val string = B.V.Integer.makestring o Byte2.to_int o
		               (fn w => w && 2u255)
		val port = string (p >> 8) ^ "," ^ string p
	    in
		case command (port_prefix ^ port) of
		    (2,_) => ()
		  | (4,_) => losing_connection ()
		  | response => unexpected "PORT" response
	    end

      fun getdata dest done =
	    let val makestring = B.Access.to_string o DArray.read
	    in
		Tcp.Handler (fn data_connection =>
		  (debug_print "Data connection established.";
		   (fn pkt => (debug_print "Got data packet.";
			       output (dest,makestring pkt)),
		    fn _ => (Tcp.close data_connection;
			     debug_print "Data connection closed.";
			     signal done))))
	    end

      fun get filename dest =
	    let val done = new_cvar ()
		val (stop,_,port) = Tcp.start_passive_port (data_address,
							    getdata dest done,
							    SOME 1)
	    in
	       (settype BINARY;
		setport port;
		case command ("RETR " ^ filename) of
		    (1,_) =>
		      (case get_reply () of
			   (2,_) => wait done
			 | (4,24) => error "Can't open data connection."
			 | (4,_) => error "Transfer aborted."
			 | response => unexpected "GET2" response)
		  | (4,21) => losing_connection ()
		  | (4,50) => error "File busy."
		  | (5,50) => error "File not found."
		  | (5, 0) => error "Syntax error.  (File name too long?)"
		  | (5, 1) => error "Syntax error in file name."
		  | response => unexpected "GET1" response)
		before stop ()
	    end
	    handle
	      Tcp.Connection_Closed _ => error "Invalid connection."
	    | BufferEmpty => losing_connection ()

      fun cd dirname =
	   (case command ("CWD " ^ dirname) of
		(2,_) => ()
	      | (4,_) => losing_connection ()
	      | (5, 0) => error "Syntax error.  (Directory name too long?)"
	      | (5, 1) => error "Syntax error in directory name."
	      | (5, 2) => error "Server does not support cd."
	      | (5,50) => error "Directory not found."
	      | response => unexpected "CWD" response)
	    handle
	      Tcp.Connection_Closed _ => error "Invalid connection."
	    | BufferEmpty => losing_connection ()

      fun ls dirname dest =
	    let val dirname' = case dirname of SOME d => " " ^ d | NONE => ""
		val done = new_cvar ()
		val (stop,_,port) = Tcp.start_passive_port (data_address,
							    getdata dest done,
							    SOME 1)
	    in
	       (settype ASCII;
		setport port;
		case command ("LIST" ^ dirname') of
		    (1,_) =>
		      (case get_reply () of
			   (2,_) => wait done
			 | (4,24) => error "Can't open data connection."
			 | (4,_) => error "Transfer aborted."
			 | response => unexpected "LIST2" response)
		  | (4,50) => error "Directory busy."
		  | (5,50) => error "Directory not found."
		  | (5, 0) => error "Syntax error.  (Directory name too long?)"
		  | (5, 1) => error "Syntax error in directory name."
		  | (5, 2) => error "Server does not support ls."
		  | response => unexpected "LIST1" response)
		before stop ()
	    end
	    handle
	      Tcp.Connection_Closed _ => error "Invalid connection."
	    | BufferEmpty => losing_connection ()

      fun quit () =
	    (command "QUIT" handle _ => (2,0);      (* ignore respose code *)
	     Tcp.close control_connection handle _ => ())
       
  in
      login ();
      {get = get, cd = cd, ls = ls, quit = quit}
  end  
  handle Dns.Dns_Implementation_Error msg =>
      (B.V.print ("Dns_Implementation_Error (report to Brian): " ^ msg ^ ".\n");
       raise Dns.Dns_Implementation_Error msg)

end

