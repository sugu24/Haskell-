leapyear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4  == 0 = True
  | otherwise = False

nleapyear year =
  if year <= 1800 then 0
  else (if leapyear(year - 1) then 1 else 0) +
       nleapyear(year - 1)

ndays year = total * 365 +  (nleapyear year)
  where total = year - 1800

ndays_month year month =
  if month == 1 then (ndays year)
  else (nmonth !! (month - 2)) + (ndays_month year (month - 1)) +
       (if ((leapyear year) && ((month - 1) == 2)) then 1 else 0)
  where
    nmonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

day_of_week year month = 
  ((ndays_month year month) + 3) `mod` 7

emptys_before_day n day = (n - (length (show day)))

print_day n s = 
  if n == 0 then s ++ ""
    else (print_day (n - 1) (" " ++ s))

empty_before_one_day n s = 
  if n == 0 then s ++ ""
    else (empty_before_one_day (n - 1) ((print_day 4 "") ++ s))

last_print s = 
  if (length s) == 28 then print s
    else (last_print (s ++ "    "))

print_calendar i max_day s
  | (i - 1) == max_day = (last_print s)
  | (length s) == 28 = do
                    print s
                    (print_calendar (i + 1) max_day (print_day n (show i)))
  |  otherwise = (print_calendar (i + 1) max_day (s ++ (print_day n (show i))))
  where
    n = (emptys_before_day 4 i)

cal year month = do
  print " Sun Mon Tue Wed Thu Fri Sat"
  (print_calendar 1 max_day s)
  where
    s = (empty_before_one_day (day_of_week year month) "")
    max_day = (([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !! (month - 1)) + 
               (if ((leapyear year) && (month == 2)) then 1 else 0))