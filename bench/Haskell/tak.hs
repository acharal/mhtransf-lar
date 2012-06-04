import Time

tak h x y z =
   if x <= y then
      h x y z
   else
      tak h (tak h (x-1) y z)
            (tak h (y-1) z x)
            (tak h (z-1) x y);

third x y z = z;

result = tak third 24 16 8;

main = 
   getClockTime >>= \ t1 ->
   print result >>
   getClockTime >>= \ t2 -> 
   let TimeDiff y m d h mn s pd = diffClockTimes t2 t1
       time                     = (fromInteger ((toInteger s) * 10^12 + pd)) / 10^12
   in  print time
