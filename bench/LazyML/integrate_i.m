let maxim = 100000 in

let rec integrate g h a b =
      integ g h a b 0 0

and integ g h a b i s =
      if i < b-a then
         integ g h a b (i+1) (s + g h (a + i))
      else
         s

and trapezoid f x =
      (f(x) + f(x+1)) / 2

and simpson13 f x =
      (f(x) + 4 * f(x+1) + f(x+2)) / 6

and simpson38 f x =
      (f(x) + 3 * f(x+1) + 3 * f(x+2) + f(x+3)) * 3 / 24

and square x = x * x
and inverse x = maxim / x
and absolute x = if x < 0 then -x else x

and diff g1 g2 =
      absolute (integrate g1 square 0 maxim - integrate g2 square 0 maxim) +
      absolute (integrate g1 inverse 1 maxim - integrate g2 inverse 1 maxim)
in
 
let result =
      diff trapezoid simpson13 +
      diff simpson13 simpson38 +
      diff simpson38 trapezoid
in  "Result: " @ itos result @ "\n"
