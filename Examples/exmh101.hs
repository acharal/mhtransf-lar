result = hfib apply add pred 10;
hfib a f s n = if n <= 1 then n
               else f (hfib a f s (a s n)) (hfib a f s (a s (a s n)));
apply g x = g x;
pred m = m - 1;
add b c = b + c
