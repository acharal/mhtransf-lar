module tak
import StdEnv
Start = result

tak h x y z =
   if (x <= y)
      (h x y z)
      (tak h (tak h (x-1) y z)
             (tak h (y-1) z x)
             (tak h (z-1) x y))

third x y z = z

result = tak third 24 16 8
