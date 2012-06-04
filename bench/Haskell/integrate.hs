integrate g h a b n =
   integ g h a b n 0 0.0;

integ g h a b n i s =
   if i < n then
      integ g h a b n (i+1) (s + g h (a + (b-a) * i/n) (a + (b-a) * (i+1)/n))
   else
      s;

trapezoid f a b =
   (b-a) * (1/2) * (f(a) + f(b));

simpson13 f a b =
   (b-a) * (1/6) * (f(a) + 4 * f(a + (b-a)/2) + f(b));

simpson38 f a b =
   (b-a) * (3/24) * (f(a) + 3 * f(a + (b-a)/3) + 3 * f(b - (b-a)/3) + f(b));

square x = x * x;
inverse x = 1.0 / x;
absolute x = if x < 0.0 then -x else x;

diff g1 g2 n =
   absolute (integrate g1 square 0.0 1.0 n - integrate g2 square 0.0 1.0 n) +
   absolute (integrate g1 inverse 1.0 2.0 n - integrate g2 inverse 1.0 2.0 n);

err n = 
   diff trapezoid simpson13 n +
   diff simpson13 simpson38 n +
   diff simpson38 trapezoid n;

result = err 100000
