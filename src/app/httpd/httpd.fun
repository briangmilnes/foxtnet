(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (stone+@cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

		i.	Abstract
		ii.	Table of Contents
		iii.	RCS Log
		1.	functor Httpd
                2.      translation routines
                3.      imported functions
                4.      parsing functions
		5.      HTTP/HTML output
                6.      loggin functions
                7.      internal function process_request
		8.      function install
                9.      function uninstall
                10.     function uptime

		iii.	RCS Log
	
$Log: httpd.fun,v $
# Revision 1.3  1995/03/08  20:22:45  cstone
# version for public access.
#
# Revision 1.1  1995/01/25  22:43:25  cstone
# Initial revision
#
 
		1.	functor Httpd
*)

functor Httpd (structure B : FOX_BASIS
               structure MakeTcpServer : MAKETCPSERVER
               sharing type MakeTcpServer.TcpStack.Tcp.port = FoxWord16.word
               sharing type MakeTcpServer.TcpStack.Tcp.incoming = B.Dyn_Array.T
               sharing type MakeTcpServer.TcpStack.Tcp.outgoing = B.Dyn_Array.T
               val home_dir : string
               val default_document : string
               val log_file : string
               val checkpoint_file : string
               val redirect : bool
               val redirect_prefix: string
               val redirect_server: string
              ) : SERVER =

struct

  structure Pipe = B.Pipe
  structure Tcp  = MakeTcpServer.TcpStack.Tcp
  type server_id = MakeTcpServer.server_id

      (* MIPS  System.Unsafe.CInterface.gettimeofday *)
      (* Alpha System.Unix.gettimeofday *)
  val gettimeofday = System.Unsafe.CInterface.gettimeofday

  val server_starttime = ref (gettimeofday())
  val server_bytesout  = ref (FoxWord64.intToWord 0)
  val server_long_version  = "FoxNet Httpd, Version 1.51:  March 4, 1995"
  val server_short_version = "FoxNet-Httpd/1.51"
  val server_accesses  = ref 0

(*
-----------------------------------------------------------------------
		2.	translation routines

       48 => 0; 57 => 9; 65 => A; 70 => F; 97 => a; 102 => f;
       37 => %; 42 => *; 46 => .; 47 -> /; 13 => CR; 10 => LF;
*)

  fun hexcharcode_to_dec c =
     if (c >= 48 andalso c <= 57) then
 	c - 48
     else if (c >= 65 andalso c <= 70) then
        10 + c - 65
     else if (c >= 97 andalso c <= 102) then
        10 + c - 97
     else
        0

  val CRLF = "\n"

  (* Translate escaped character sequences (% followed by
     a two-hex-digit character code) to a single character. *)
  fun unescape [] = []
    | unescape (37::x::y::xs) =
        (16*(hexcharcode_to_dec x)+(hexcharcode_to_dec y)) :: unescape xs
    | unescape (x::xs) = x :: (unescape xs)

  fun fix_endlines (13::10::xs) = 10 :: (fix_endlines xs)
    | fix_endlines (x::xs)      = x :: (fix_endlines xs)
    | fix_endlines []           = []

  fun plus_to_space []       = []
    | plus_to_space (43::xs) = 32 :: (plus_to_space xs)
    | plus_to_space (x::xs)  = x  :: (plus_to_space xs)

  local
     open System.Unsafe.SysIO
  in
     fun is_file_readable s =
        let val fd = openf (s, O_READ)
            val b  = (ftype (DESC fd) = F_REGULAR) handle _ => false
        in
            closef fd;
            b
        end
           handle _ => false
  end

  fun prefix_of s t =
    let fun loop [] (y::ys) = true
         | loop _  []      = false
         | loop (x::xs) (y::ys) = (x=y) andalso loop xs ys
    in
       loop (explode s) (explode t)
    end

  exception NotFound of string
  fun translate_url ""  = home_dir ^ "/" ^ default_document
    | translate_url "/" = home_dir ^ "/" ^ default_document
    | translate_url url = 
        let val base_url =
                  ((implode o(map chr)o unescape o(map ord)o explode) url)
            val urls  = if (redirect andalso 
                            (prefix_of (home_dir ^ "/") (base_url ^ "/"))) then
                          [base_url]
                        else 
                          [home_dir ^ base_url, home_dir ^ "/" ^ base_url]
            val urls = urls @
                       (map (fn u => u ^ default_document) urls) @
                       (map (fn u => u ^ "/" ^ default_document) urls)
            fun loop []      = raise (NotFound base_url)
              | loop (u::us) = if (is_file_readable u) then
                                  u
                               else
                                  loop us
        in
            loop urls
        end

  (* forbid * and /../ from appearing in filenames *)
  val is_secure =
      let fun secure' (42::xs) = false
            | secure' (47::46::46::47::xs) = false
            | secure' (x::xs) = secure' xs
            | secure' [] = true
      in
         secure' o (map ord) o explode
      end

   (* Split input from a form into key/value string pairs *)
   val split_input =
      let fun splitoff_pair l []       = [rev l]
            | splitoff_pair l (38::xs) = (rev l) :: (splitoff_pair [] xs)
            | splitoff_pair l (x::xs)  = splitoff_pair (x::l) xs

          fun splitoff_key l []       = (rev l, [])
            | splitoff_key l (61::xs) = (rev l, xs)
            | splitoff_key l (x::xs)  = splitoff_key (x::l) xs

          val decode_list = 
                implode o (map chr) o fix_endlines o unescape o plus_to_space
          fun decode_pair (x,y) = (decode_list x, decode_list y)
      in
          ((map decode_pair) o 
           (map (splitoff_key [])) o 
           (splitoff_pair []) o 
           (map ord) o 
           explode)
      end

(*
-----------------------------------------------------------------------
		3.	imported functions
*)

  type imported_fun = string -> (string * string) list-> string
  type 'a env       = (string * 'a) list ref
  val  exp_funs     = ref [] : imported_fun env

  exception HTTPD_Lookup
  fun empty_env () = ref [] : imported_fun env
  fun lookup (env: 'a env) n = 
    let fun lookup' [] n = raise HTTPD_Lookup
          | lookup' ((x,f)::xs) n = if (x = n) then f else lookup' xs n
    in
      lookup' (!env) n
    end
  fun extend env n f = env := (n,f)::(!env)

  fun extend_str env n f = 
    (env := (n, (lookup env n) ^ " " ^ f) :: (!env))
    handle HTTPD_Lookup => env := (n,f)::(!env)

  fun lookup_str env n = (lookup env n) handle HTTPD_Lookup => ""

  fun exec name inputs = (lookup exp_funs name) name inputs

  val install_fn = extend exp_funs

  

(*
-----------------------------------------------------------------------
		4.	parsing functions
*)

  (* extract the extension from a filename *)
  fun extension str =
      let val dot = "."
	  fun ext_list [] ext = ext
	    | ext_list (x::xs) ext = 
	         if (x = dot) then ext else ext_list xs (x::ext)
      in 
	  (implode (ext_list (rev (explode str)) []))
      end

  fun extension_to_mime_type ext =
      (case ext of
	   "ps"   => "application/postscript"
	 | "dvi"  => "application/x-dvi"
	 | "txt"  => "text/plain"
	 | "html" => "text/html"
         | "shtml"=> "text/html"
	 | "gif"  => "image/gif"
	 | "xbm"  => "image/x-xbm"
	 | "jpg"  => "image/jpeg"
	 | "jpeg" => "image/jpeg"
	 | _      => "text/plain")

  val mime_type = extension_to_mime_type o extension
  val html_type = "text/html"

  fun should_parse filename =
     (case (extension filename) of
         "shtml" => true
       | _       => false)

  local 
     structure Link_Parser = Link_Parser(structure V=B.V)
     open Link_Parser
     open Parser
       infix  2 -- ##
       infixr 3 &&
       infix  2 wth suchthat return guard
       infixr 1 ||
     open Parsing_Utils
       infixr 4 cor 
       infixr 4 cand 
       infixr 3 &-&
   in
     fun replace_execs s =
        (case (B.V.String.index ("#",s,0)) of
           NONE => s
         | SOME _ => 
             (case (parse (repeat any
                          && Parser.string (explode "<!--#exec")
                          &-& Parser.string (explode "cmd")
                          &-& (literal "=")
                          &-& string
                          &-& Parser.string (explode "-->")
                          && uptoeol)
                     (Position.markstream (Input.readstring s))) of
                 NONE => s
               | SOME (lst1, (_, (_, (_, (cmd, (_, lst2)))))) =>
                   (replace_execs (implode lst1)) ^
                   ((exec cmd []) handle HTTPD_Lookup => "<???>") ^
                   (implode lst2)))

    fun get_header_info s =
       parse (((repeat (any suchthat (fn v => v <> ":")) wth implode)
              && literal ":"
              &-& (uptoeol wth implode)) wth (fn(x,(_,y)) => (x,y)))
             (Position.markstream (Input.readstring s))

   end
                  

(*
-----------------------------------------------------------------------
		5.	HTTP/HTML output
*)

  fun http_begin_header (number, msg) = 
      let val code = (B.V.Integer.makestring number) ^ " " ^ msg
      in
	  "HTTP/1.0 " ^ code ^ CRLF ^
          "Server: " ^ server_short_version ^ CRLF
      end

  fun http_content mimetype = "Content-type: " ^ mimetype ^ CRLF

  fun http_uri uri = "URI: <" ^ uri ^ ">" ^ CRLF ^
                     "Location: " ^ uri ^ CRLF

  fun http_end_header () = CRLF

  fun html_message title bodytext =
      "<HTML><HEAD><TITLE>" ^ title ^ "</TITLE><P></HEAD>\n" ^
      "<BODY><H1>" ^ title ^ "</H1>\n" ^
      bodytext ^ "<P></BODY></HTML>\n"


(*
-----------------------------------------------------------------------
		6.	request logging
*)

  (* Function to save the number of bytes served *)
  fun checkpoint s =
     let val dest = open_out checkpoint_file
     in
	 output (dest, s^"\n");
	 close_out dest
     end

  (* Function to restore the number of bytes served *)
  fun read_checkpoint () =
     let val src = open_in checkpoint_file
	 val data = input_line src
	 val _ = close_in src

	 fun toWord (nil, a) = a
	   | toWord (d::ds, a) =
	     let val ascii = ord d
	     in
		 if ascii >= ord "0" andalso ascii <= ord "9" then
		     toWord
		     (ds,
		      FoxWord64.+ (FoxWord64.* (FoxWord64.intToWord 10, a),
				   FoxWord64.intToWord (ascii - ord "0")))
		 else
		     a
	     end
     in
	 toWord (explode data, FoxWord64.intToWord 0)
     end
       handle _ => FoxWord64.intToWord 0
	 

  fun write_to_log s =
     let val dest = open_append log_file
     in
         output (dest, s);
         close_out dest;
         B.V.print s
     end
       handle _ => ()

  fun log (log_entry, checkpoint_data) =
     (write_to_log log_entry;
      checkpoint checkpoint_data)

  fun log_header () = write_to_log (server_long_version ^ "\n")

	       
(*
-----------------------------------------------------------------------
		7.	internal function process_request
*)
	   
  fun process_request (connection, producer) =
      let val get_line   = MakeTcpServer.get_line producer 
          val get_string = MakeTcpServer.get_string producer
          val headers = ref [] : string env

	  fun send str =
	      let val n = String.size str
		  val (pkt, sendit) = Tcp.allocate_send(connection, n)
		  val data = B.Dyn_Array.init (System.Unsafe.cast str)
	      in
                  server_bytesout := 
                    FoxWord64.+ (!server_bytesout, FoxWord64.intToWord n);
		  B.Dyn_Array.copy (data, 0, n, pkt, 0);
		  sendit();
                  ()
	      end

          fun send_parsed_file headers filename =
              let val src = open_in filename
                  (* inefficient, but apparently adequate *)
                  fun loop n str =
                      if (end_of_stream src) then
                         if (n <= 0) then
                            ()
                         else
                            send str
                      else 
                          if (n >= 8192) then
                             (send str; loop 0 "")
                          else
                             let val line = input_line src
                             in
                                loop (n + B.V.String.length line)
                                     (str ^ (replace_execs line))
                             end
              in
                  (loop 0 ""; close_in src)
                  handle (Tcp.Send_Failed _) => close_in src
              end

	  fun send_file filename =
	      let val src = open_in filename
		  val bufsize = 8192 (* 8KB *)
		  fun loop () = 
                      let val next = inputc src bufsize
		      in
			  if (next <> "") then (send next; loop()) else ()
		      end
	      in
		  (loop (); close_in src)
                  handle (Tcp.Send_Failed _) => close_in src
	      end

	  fun file_exists filename =
               (close_in (open_in filename); true)
	       handle (Io _) => false
		  
          fun parse_headers () =
              let val line = get_line ()
              in
                  case (get_header_info line) of
                    NONE => ()
                  | SOME(n,f) => (extend headers n f; 
                                  parse_headers())
              end

          fun get_content_length headers =
              B.V.String.string_to_int (lookup_str headers "Content-length")
	  
	  fun close () = Tcp.close connection
                         handle Tcp.Connection_Closed _ => ()

          fun missing_uri_response orig_name decoded_name =
             if (redirect andalso (prefix_of redirect_prefix decoded_name))then
                ((http_begin_header(301, "Moved Permanently")) ^ 
                 (http_content html_type) ^
                 (http_uri (redirect_server ^ orig_name)) ^
                 (http_end_header ()) ^
                 (html_message "301 Moved Permanently" "Moved Permanently."))
             else
                ((http_begin_header(404, "Not Found")) ^
                 (http_content html_type) ^
                 (http_end_header ()) ^
                 (html_message 
                     "404 Not Found"
	             "The server could not find this URI."))

      in
	  (server_accesses := !server_accesses + 1;
           case (B.V.String.tokenize(get_line())) of
	        ["GET", filename'] => 
		  (let val filename = translate_url filename'
		   in
                       log (("GET9 " ^ filename ^ " >>> " ^
			     (Tcp.makestring_key
			      (Tcp.connection_key connection)) ^ 
			     "\n"),
			    FoxMakestring.word64 (!server_bytesout));
                       if (not (is_secure filename')) then
                           send (html_message "403 Forbidden"
                                              "Bad URI accessed.")
		       else if (should_parse filename) then
                           send_parsed_file headers filename
                       else
                           send_file filename
		   end
	             handle NotFound s =>
                        (if (redirect andalso prefix_of redirect_prefix s) then
                           send (html_message "404 Not Found"
				 ("The requested document cannot be accessed" ^
                                  " via this FoxNet server.  However, try " ^
                                  " <A HREF=" ^ redirect_server ^ filename' ^
                                  ">this link</A> to another server<P>"))
                        else
                           send (html_message "404 Not Found"
				  "The server could not find this URI.")))

	     | ["GET", filename', "HTTP/1.0"] =>
		  (let val filename = translate_url filename'
		   in
                       log (("GET  " ^ filename ^ " >>> " ^
			     (Tcp.makestring_key
			      (Tcp.connection_key connection)) ^ 
			     "\n"),
			    FoxMakestring.word64 (!server_bytesout));
                       if (not (is_secure filename)) then
                           send ((http_begin_header(403, "Forbidden")) ^
                                 (http_content html_type) ^
                                 (http_end_header ()) ^
                                 (html_message "403 Forbidden" 
                                     "Bad URI accessed."))
		       else 
			   (send ((http_begin_header(200, "OK")) ^
                                  (http_content (mime_type filename)) ^
                                  (http_end_header ()));
                            if (should_parse filename) then
		               (parse_headers ();
                                send_parsed_file headers filename)
                            else
                               send_file filename)
		   end
                       handle NotFound s => 
                         (send (missing_uri_response filename' s)))

             | ["HEAD", filename', "HTTP/1.0"] =>
		  (let val filename = translate_url filename'
		   in
                       log (("HEAD " ^ filename ^ " >>> " ^
			     (Tcp.makestring_key
			      (Tcp.connection_key connection)) ^ 
			     "\n"),
			    FoxMakestring.word64 (!server_bytesout));
                       if (not (is_secure filename')) then
                           send ((http_begin_header(403, "Forbidden")) ^
                                 (http_content html_type) ^
                                 (http_end_header ()) ^
                                 (html_message "403 Forbidden"
                                     "Bad URI accessed."))
		       else 
			   send ((http_begin_header(200, "OK")) ^
                                 (http_content (mime_type filename)) ^
                                 (http_end_header ()))
		   end
                       handle NotFound s => 
                         (send (missing_uri_response filename' s)))

             | ["POST", filename', "HTTP/1.0"] =>
                   let val string_tl = implode o tl o explode
                       val command = string_tl(translate_url filename')
                       val _       = parse_headers ();
                       val len     = get_content_length headers;
                   in
                       log (("POST " ^ command ^ " >>> " ^
			     (Tcp.makestring_key
			      (Tcp.connection_key connection)) ^ 
			     "\n"),
			    FoxMakestring.word64 (!server_bytesout));
		       let val response = 
                               exec command (split_input (get_string len))
                       in 
		            send ((http_begin_header(200, "OK")) ^
                                  (http_content html_type) ^
                                  (http_end_header ()));
 		            send response
                       end
                         handle HTTPD_Lookup =>
			   send ((http_begin_header (404, "Not Found")) ^
                                 (http_content html_type) ^
                                 (http_end_header ()) ^
                                 (html_message "404 Not Found"
				  "The server could not find this query."))
                   end
              
	     | _ => send ((http_begin_header (501, "Not Implemented")) ^
                          (http_content html_type) ^
                          (http_end_header ()) ^
                          (html_message "501 Not Implemented" "Sorry"));
             close ())
                handle _ => close ()
      end

(*
-----------------------------------------------------------------------
		8.	function install
*)

   fun install () = 
       (server_starttime := gettimeofday ();
        log_header ();
	server_bytesout := read_checkpoint ();
        MakeTcpServer.install (Tcp.Local_Specified 
          {local_port = FoxWord16.intToWord 80})
          process_request NONE)
        

(*
-----------------------------------------------------------------------
		9.	function uninstall
*)

   val uninstall = MakeTcpServer.uninstall


(*
-----------------------------------------------------------------------
		10.	functions for reporting statistics
*)


   (* move these to MakeTcpServer! *)

   fun get_secs (System.Timer.TIME {sec,usec}) = sec

   fun uptime () = 
      get_secs(System.Timer.sub_time 
                 (gettimeofday(),
                  !server_starttime))

   fun bytesout () = !server_bytesout

   fun accesses () = !server_accesses

end
