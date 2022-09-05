(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Time

		iii.	RCS Log
	
$Log: time.fun,v $
Revision 1.10  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.9  1995/09/17  22:07:07  esb
minor fixes.

Revision 1.8  1995/09/17  21:01:25  cstone
Changed format of makedate_time to RFC 822/RFC 1123 format;
fixed calculation of leap years to account for 100 & 400
year exceptions

Revision 1.7  1995/03/10  03:53:01  esb
adapted to new vendor.sig.

Revision 1.6  1995/01/21  14:38:40  esb
added make_date.

Revision 1.5  1994/07/06  17:48:19  esb
fixed a bug where "quot" was inappropriately used for "div".

Revision 1.4  1994/05/04  01:38:28  esb
minor optimization.

Revision 1.3  94/03/03  16:56:53  esb
added header.


		1.	functor Time
*)

functor Fox_Time (structure Vendor: VENDOR): FOX_TIME = 
 struct
  datatype time = Time of {sec: int, usec: int}

  local

   (* Here we grab a bunch of things from the Definition's
      Initial Basis, so we don't have to worry about shadowing *)
	    
   val intsub: int*int->int = (op-)
   val intadd: int*int->int = (op+)
   val intmul: int*int->int = (op* )
   val intdiv: int*int->int = (op quot)
   val intless: int*int -> bool = (op<)
   val intmore: int*int -> bool = (op>)
   val realadd: real*real->real = (op+)
   val realsub: real*real->real = (op-)
   val realmul: real*real->real = (op* )

   (* here is the conversion of seconds to daytime GMT (time-zone info
      is not easily available). *)

   fun leap_day n = 
     if n mod 4 = 0 then 
       if (n mod 100 = 0) then
	 if (n mod 400 = 0) then 
	   1
         else
           0
       else
	 1 
     else 
       0

   fun year_days n = 365 + leap_day n

   fun month_days (_, 1) = 31
     | month_days (year, 2) = 28 + leap_day year
     | month_days (_, 3) = 31
     | month_days (_, 4) = 30
     | month_days (_, 5) = 31
     | month_days (_, 6) = 30
     | month_days (_, 7) = 31
     | month_days (_, 8) = 31
     | month_days (_, 9) = 30
     | month_days (_,10) = 31
     | month_days (_,11) = 30
     | month_days (_, _) = 31

   fun compute_month (year, days) =
        let fun loop (month, days_left) =
	         if days_left < month_days (year, month) then
		  (month, days_left + 1)
		 else loop (month + 1, days_left - month_days (year, month))
	in loop (1, days)
	end

   fun compute_year days =
        let fun loop (year, days_left) =
	         if days_left <= year_days year then (year, days_left)
		 else loop (year + 1, days_left - year_days year)
	in loop (1970, days)
	end

   fun month_name 1 = "Jan"
     | month_name 2 = "Feb"
     | month_name 3 = "Mar"
     | month_name 4 = "Apr"
     | month_name 5 = "May"
     | month_name 6 = "Jun"
     | month_name 7 = "Jul"
     | month_name 8 = "Aug"
     | month_name 9 = "Sep"
     | month_name 10 = "Oct"
     | month_name 11 = "Nov"
     | month_name _ = "Dec"

   fun weekday_name 0 = "Sun"
     | weekday_name 1 = "Mon"
     | weekday_name 2 = "Tue"
     | weekday_name 3 = "Wed"
     | weekday_name 4 = "Thu"
     | weekday_name 5 = "Fri"
     | weekday_name _ = "Sat"

   fun two_digit n = if n < 10 then "0" ^ Integer.toString n
		     else Integer.toString n

   fun min_digit n = if n < 10 then " " ^ Integer.toString n
		     else Integer.toString n

   fun makedate_time {years, months, days, weekdays, hours, minutes, seconds} =
        weekday_name weekdays ^ ", " ^
	two_digit days ^ " " ^
        month_name months ^ " " ^
	Integer.toString years ^ " " ^
	two_digit hours ^ ":" ^
	two_digit minutes ^ ":" ^
	two_digit seconds ^ " GMT"

  in  (* local *)

   fun split_time total_seconds =
        let val total_minutes = total_seconds div 60
	    val total_hours = total_minutes div 60
	    val total_days = total_hours div 24
	    val (years, days_in_year) = compute_year total_days
	    val (months, days) = compute_month (years, days_in_year)
	    val hours = total_hours - (total_days * 24)
	    val minutes = total_minutes - (total_hours * 60)
	    val seconds = total_seconds - (total_minutes * 60)
	    val weekdays = (total_days + 4) mod 7 (* sunday is weekday 0 *)
	in {years  = years,
	    months = months, 
            days   = days, 
            weekdays = weekdays, 
            hours    = hours, 
            minutes  = minutes, 
            seconds  = seconds}
	end

 (* usec may be negative, so here we must use "div", not quot. *)
   fun norm_time (Time {sec, usec}) =
        let val carry = usec div 1000000
	in Time {sec = sec + carry, usec = usec mod 1000000}
	end

   fun (Time {sec = s1, usec = us1}) - (Time {sec = s2, usec = us2}) =
        norm_time (Time{sec = intsub (s1, s2), usec = intsub (us1, us2)})

   fun (Time {sec = s1, usec = us1}) + (Time {sec = s2, usec = us2}) =
        norm_time (Time{sec = intadd (s1, s2), usec = intadd (us1, us2)})

   fun (Time {sec, usec}) * m =
        norm_time (Time{sec = intmul (sec, m), usec = intmul (usec, m)})

   fun (Time {sec, usec}) div d = 
        Time {sec = intdiv (sec, d),
	      usec = intdiv (intadd (usec, intmul (sec mod d, 1000000)), d)}

   fun (Time {sec = s1, usec = us1}) < (Time {sec = s2, usec = us2}) =
        intless (s1, s2) orelse (s1 = s2 andalso intless (us1, us2))

   fun (Time {sec = s1, usec = us1}) > (Time {sec = s2, usec = us2}) =
        intmore (s1, s2) orelse (s1 = s2 andalso intmore (us1, us2))

   fun time_to_real (Time {sec, usec}) =
        realadd (real sec, (real usec) / 1000000.0);

   fun real_to_time r = 
        let val f = floor r
	in Time {sec = f, usec = floor (realmul (realsub (r, real f),
						 1000000.0))}
	end

   fun makestring (Time {sec, usec}) =
        let val filler = if intmore (0, usec) then ""
	                 else if intless (usec, 10) then "00000"
			 else if intless (usec, 100) then "0000"
			 else if intless (usec, 1000) then "000"
			 else if intless (usec, 10000) then "00"
			 else if intless (usec, 100000) then "0"
			 else ""
	in Integer.toString sec ^ "." ^ filler ^
	   Integer.toString usec
	end

   fun make_date (Time {sec, usec}) =
        makedate_time (split_time sec)

   val zero_time = Time {sec = 0, usec = 0}

   fun time_of_day () =
        let val now = Vendor.Time.gettimeofday ()
	    val sec = Vendor.Time.timesec now
	    val usec = Vendor.Time.timeusec now
	in Time {sec = sec, usec = usec}
	end


  end (* local *)
 end (* struct *)
	    
