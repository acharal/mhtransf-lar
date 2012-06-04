module ack
import StdEnv
Start = result

ack m n =
   if (m == 0)
      (n+1)
      (if (n == 0)
      (ack (m-1) 1)
      (ack (m-1) (ack m (n-1))))


result = ack 3 9 
