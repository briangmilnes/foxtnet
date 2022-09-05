(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
        Ken Cline    (Ken.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Support for reading realm information from configuration file.
	Kerberos realm information is typically kept in the file
	`/etc/krb.conf'.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Krb_Realm
	2.	fun is_whitespace
	3.	fun input_string
	4.	fun skip_whitespace
	5.	fun local_realm
	6.	fun contains
	7.	fun kerberos_servers


		iii.	RCS Log
	
$Log: krbrealm.fun,v $
Revision 1.1  1994/08/25  12:13:36  robby
Initial revision

Revision 1.1  1994/07/13  18:49:00  robby
Initial revision

Revision 1.1  94/05/20  02:25:06  robby
This file was written by Ken Cline, but somehow 
I lost the his revision history.
Initial revision

		1.	functor Krb_Realm
 *)


functor Krb_Realm(val config_file : string) : KRB_REALM =
struct

(*
	2.	fun is_whitespace
 *)
  fun is_whitespace "" = true
    | is_whitespace " " = true
    | is_whitespace "\t" = true
    | is_whitespace "\n" = true
    | is_whitespace "\000" = true
    | is_whitespace _ = false

(*
	3.	fun input_string

	Read a string up to, but not including whitespace, newline, or eof.
 *)
  fun input_string f =
    let val test=lookahead f
    in
      if is_whitespace test then ""
      else (input (f,1)) ^ (input_string f)
    end

(*
	4.	fun skip_whitespace
 *)
  fun skip_whitespace f =
      while is_whitespace (lookahead f) do input (f, 1)

(*
	5.	fun local_realm

	Return the local realm name, which is named in the config_file
 *)
  fun local_realm () =
      let
        val line =
            let val config = open_in config_file
            in input_line config before close_in config end
      in
	if line = "" then ""
        else substring (line, 0, size line - 1)
      end


(*
	6.	fun contains

	Test to see if the first string is in the second one.
*) 
  fun contains string1 string2=
    let fun at_beginning (l::ls) (l'::ls')=
      (l=l') andalso (at_beginning ls ls')
	  | at_beginning _ []=false
	  | at_beginning [] _=true
      fun contains_list l (l'::ls')=
	(at_beginning l (l'::ls'))
	orelse (contains_list l ls')
	| contains_list _ []=false
    in
      contains_list (explode string1) (explode string2)
    end

(*
	7.	fun kerberos_servers

	Return the list of hostnames that config_file lists as
        Kerberos servers for the given realm
 *)
  fun kerberos_servers realm =
      let
	val realm=realm
        val config = open_in config_file

        (* skip the first line, which names the local realm *)
        val _ = input_line config

        fun list_servers () =
            let
              val realm' = input_string config
              val _      = skip_whitespace config
              val server = input_string    config
	      val rest   = input_line      config
            in
              if realm = realm' then
                server :: (list_servers ())
              else if not (end_of_stream config) then
                list_servers ()
              else
                []
            end
      in
        (list_servers ())
        before (close_in config)
      end;

end (* functor *)
