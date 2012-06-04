module integrate_i
import StdEnv

Start :: Int
Start = result

maxim :: Int
maxim = 10000

integrate g h a b =
   integ g h a b 0 0

integ g h a b i s =
   if (i < b-a)
      (integ g h a b (i+1) (s + g h (a + i)))
      s

trapezoid f x =
   (f(x) + f(x+1)) / 2

simpson13 f x =
   (f(x) + 4 * f(x+1) + f(x+2)) / 6

simpson38 f x =
   ((f(x) + 3 * f(x+1) + 3 * f(x+2) + f(x+3)) * 3) / 24

square x = x * x
inverse x = maxim / x
absolute x = if (x < 0) (~x) x

diff g1 g2 =
   absolute (integrate g1 square 0 maxim - integrate g2 square 0 maxim) +
   absolute (integrate g1 inverse 1 maxim - integrate g2 inverse 1 maxim)

result :: Int
result =
   diff trapezoid simpson13 +
   diff simpson13 simpson38 +
   diff simpson38 trapezoid
