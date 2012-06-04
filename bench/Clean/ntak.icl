module ntak
import StdEnv
Start = result

shuffle h x y z n =
   if (n rem 3 == 0)
      (ntak shuffle h (n+3) (x-1) y z)
      (if (n rem 3 == 1)
         (ntak shuffle h (n+2) (y-1) z x)
         (ntak shuffle h (n+1) (z-1) x y))

ntak f h n x y z =
   if (x <= y)
      (h x y z)
      (ntak f h n (f h x y z n)
                  (f h x y z (n+1))
                  (f h x y z (n+2)))

third x y z = z

result = ntak shuffle third 0 24 16 8
