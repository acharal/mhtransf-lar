import Time

ack m n = if m == 0 then
                n + 1
          else if n == 0 then
                ack (m-1) 1
          else
                ack (m-1) (ack m (n-1));
                
result = ack 3 9

main = 
   getClockTime >>= \ t1 ->
   print result >>
   getClockTime >>= \ t2 -> 
   let TimeDiff y m d h mn s pd = diffClockTimes t2 t1
       time                     = (fromInteger ((toInteger s) * 10^12 + pd)) / 10^12
   in  print time
          
