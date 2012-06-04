let rec primes n =
      if n <= 0 then
         2
      else if n = 1 then
         3
      else
         findPrimeMinus (n-2) 1
      
and findPrimeMinus n i =
      if testPrime (6*i-1) 1 then
         if n = 0 then
            6*i-1
         else
            findPrimePlus (n-1) i
      else
         findPrimePlus n i

and findPrimePlus n i =
      if testPrime (6*i+1) 1 then
         if n = 0 then
            6*i+1
         else
            findPrimeMinus (n-1) (i+1)
      else
         findPrimeMinus n (i+1)

and testPrime n i =
      if (6*i-1) * (6*i-1) > n then
         true
      else if n % (6*i-1) = 0 then
         false
      else if n % (6*i+1) = 0 then
         false
      else
         testPrime n (i+1)

in

let result = primes 1500
in  "Result: " @ itos result @ "\n"
