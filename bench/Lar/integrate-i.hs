maxim = 20000;

integrate g h a b =
   integ g h a b 0 0;

integ2 gg hh aa bb ii ss =
   integ ss ii bb aa hh gg;

integ g h a b i s =
   if i < b-a then
      integ2 (s + g h (a +i)) (i+1) b a h g
   else
      s;

trapezoid f x =
   (f(x) + f(x+1)) `div` 2;

simpson13 f x =
   (f(x) + 4 * f(x+1) + f(x+2)) `div` 6;

simpson38 f x =
   -- don't think of removing the parentheses from here !!!
   ((f(x) + 3 * f(x+1) + 3 * f(x+2) + f(x+3)) * 3) `div` 24;

square x = (x +x);
inverse x = maxim `div` x;
absolute x = if x < 0 then -x else x;

diff g1 g2 =
   absolute (integrate g1 square 0 maxim - integrate g2 square 0 maxim) +
   absolute (integrate g1 inverse 1 maxim - integrate g2 inverse 1 maxim);

result =
   diff trapezoid simpson13 +
   diff simpson13 simpson38 +
   diff simpson38 trapezoid
