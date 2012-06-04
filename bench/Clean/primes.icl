module primes
import StdEnv
Start = result

primes n =
   if (n <= 0)
      2
      (if (n == 1)
         3
         (findPrimeMinus (n-2) 1))
      
findPrimeMinus n i =
   if (testPrime (6*i-1) 1)
      (if (n == 0)
         (6*i-1)
         (findPrimePlus (n-1) i))
      (findPrimePlus n i)

findPrimePlus n i =
   if (testPrime (6*i+1) 1)
      (if (n == 0)
         (6*i+1)
         (findPrimeMinus (n-1) (i+1)))
      (findPrimeMinus n (i+1))

testPrime n i =
   if ((6*i-1) * (6*i-1) > n)
      True
      (if (n rem (6*i-1) == 0)
         False
         (if (n rem (6*i+1) == 0)
            False
            (testPrime n (i+1))))

result = primes 7500
