module Main where
import Time

safe q x n s d =
   if n == 0 then
      True
   else
      x /= s `mod` q &&
      x /= s `mod` q + d &&
      x /= s `mod` q - d &&
      safe q x (n-1) (s `div` q) (d+1);

count q x n s =
   if n < q then
      if x < q then
         if safe q x n s 1 then
            count q 0 (n+1) (x+q*s) + count q (x+1) n s
         else
            count q (x+1) n s
      else
         0
   else
      1;

result = count 7 0 0 0;

main = 
   getClockTime >>= \ t1 ->
   print result >>
   getClockTime >>= \ t2 -> 
   let TimeDiff y m d h mn s pd = diffClockTime t2 t1
       time                     = (fromInteger ((toInteger s) * 10^12 + pd)) / 10^12
   in  print time
