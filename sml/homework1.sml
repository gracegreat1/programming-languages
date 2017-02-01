
(* 1. take two dates and evaluates to true or false
   it evaluates to true if the first argument is a date that comes before the second argument.  *)
fun is_older (dates1 : int * int * int, dates2 : int * int * int) =
  if (#1 dates1 < #1 dates2)
  then true
  else if (#1 dates1 > #1 dates2)
  then false
  else
      if (#2 dates1 < #2 dates2)
      then true
      else if (#2 dates1 > #2 dates2)
      then false
      else
	  if (#3 dates1 < #3 dates2)
	  then true
	  else false


(* 2. takes a list of dates and a month and returns how many dates in the list are in the given month *) 	   
fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if (#2 (hd dates) = month)
	   then 1 + number_in_month(tl dates, month)
	   else number_in_month(tl dates, month)
  

(* 3. takes a list of dates and a list of months and returns the number of dates in the list of dates 
   that are in any of the months in the list of months. *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)
	  

(* 4. takes a list of dates and a month and returns a list holding the dates from the argument list of dates
   that are in the month *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if (#2 (hd dates) = month)
  then hd dates :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)


(* 5. takes a list of dates and a list of months and returns a list holding the dates from 
   the argument list of dates that are in any of the months in the list of months *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

							 
(* 6. takes a list of strings and an int n and returns the nth element of the list
   where the head of the list is 1th *)
fun get_nth (strs : string list, n : int) =
  if n = 1 then hd strs
  else get_nth(tl strs, n - 1)



(* 7. takes a date and returns a string of the form January 20, 2013 (for example.) *)
fun date_to_string (date : int * int * int) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August",
		    "September", "October", "November", "December"]
  in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end


(* 8. takes an int called sum, which you can assume is positive, 
   and an int list, which you can assume contains all positive numbers, and returns an int *)
fun number_before_reaching_sum (sum : int, nums : int list) =
  if (hd nums) >= sum
  then 0
  else 1+ number_before_reaching_sum(sum - (hd nums), tl nums)
				 

(* 9. takes a day of year and returns what month that day is in *)
fun what_month (day : int) =
  let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in number_before_reaching_sum(day, days) + 1
  end


(* 10. takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn]
   where m1 is the month of day1, m2 is the month of day1+1,...,and mn is the month of day2. *)
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(* 11. takes a list of dates and evaluates to an (int*int*int) option 
   it evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list*)
fun oldest (dates : (int * int * int) list) =
  if null dates
  then NONE
  else let
      fun oldest_helper (dates : (int * int * int) list) =
	if null (tl dates)
	then hd dates
	else let val older_date = oldest_helper(tl dates)
	     in
		 if is_older(hd dates, older_date)
		 then hd dates
		 else older_date
	     end
  in
      SOME (oldest_helper(dates))
  end
