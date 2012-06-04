import Time

donut a b c d e num =
   if num == 0 then
      0
   else
      a b c d e + donut a b c d (e + 17) (num - 1);
      
four a b c d = a b c d;
three b c d = b c d;
two c d = c d;
one d = d + 1;

result = donut four three two one 4 1200;

main = 
   getClockTime >>= \ t1 ->
   print result >>
   getClockTime >>= \ t2 -> 
   let TimeDiff y m d h mn s pd = diffClockTimes t2 t1
       time                     = (fromInteger ((toInteger s) * 10^12 + pd)) / 10^12
   in  print time
