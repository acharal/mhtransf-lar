import Time

primes n =
   if n <= 0 then
      2
   else if n == 1 then
      3
   else
      findPrimeMinus (n-2) 1;
      
findPrimeMinus n i =
   if testPrime (6*i-1) 1 then
      if n == 0 then
         6*i-1
      else
         findPrimePlus (n-1) i
   else
      findPrimePlus n i;

findPrimePlus n i =
   if testPrime (6*i+1) 1 then
      if n == 0 then
         6*i+1
      else
         findPrimeMinus (n-1) (i+1)
   else
      findPrimeMinus n (i+1);

testPrime n i =
   if (6*i-1) * (6*i-1) > n then
      True
   else if n `mod` (6*i-1) == 0 then
      False
   else if n `mod` (6*i+1) == 0 then
      False
   else
      testPrime n (i+1);

result = primes 7500;

main = 
   getClockTime >>= \ t1 ->
   print result >>
   getClockTime >>= \ t2 -> 
   let TimeDiff y m d h mn s pd = diffClockTimes t2 t1
       time                     = (fromInteger ((toInteger s) * 10^12 + pd)) / 10^12
   in  print time