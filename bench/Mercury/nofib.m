module Main where
import Time

nofib n = 
   if n <= 1 then 
      1 
    else 
       nofib (n-1) + nofib (n-2) + 1;

result = 
   nofib 30;

main = 
   getClockTime >>= \ t1 ->
   print result >>
   getClockTime >>= \ t2 -> 
   let TimeDiff y m d h mn s pd = diffClockTime t2 t1
       time                     = (fromInteger ((toInteger s) * 10^12 + pd)) / 10^12
   in  print time
          
