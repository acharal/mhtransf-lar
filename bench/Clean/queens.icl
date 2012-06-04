module queens
import StdEnv
Start = result

safe q x n s d =
   if (n == 0)
      True
      (x <> s rem q &&
       x <> s rem q + d &&
       x <> s rem q - d &&
       safe q x (n-1) (s / q) (d+1))

count q x n s =
   if (n < q)
      (if (x < q)
         (if (safe q x n s 1)
            (count q 0 (n+1) (x+q*s) + count q (x+1) n s)
            (count q (x+1) n s))
         0)
      1

result = count 8 0 0 0
