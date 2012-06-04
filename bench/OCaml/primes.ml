open Unix
open Printf

let rec primes n =
      if n <= 0 then
         2
      else if n == 1 then
         3
      else
         findPrimeMinus (n-2) 1
      
and findPrimeMinus n i =
      if testPrime (6*i-1) 1 then
         if n == 0 then
            6*i-1
         else
            findPrimePlus (n-1) i
      else
         findPrimePlus n i

and findPrimePlus n i =
      if testPrime (6*i+1) 1 then
         if n == 0 then
            6*i+1
         else
            findPrimeMinus (n-1) (i+1)
      else
         findPrimeMinus n (i+1)

and testPrime n i =
      if (6*i-1) * (6*i-1) > n then
         true
      else if n mod (6*i-1) == 0 then
         false
      else if n mod (6*i+1) == 0 then
         false
      else
         testPrime n (i+1)

let result () = primes 1500

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
