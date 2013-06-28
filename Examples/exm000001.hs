shuffle h x y z n =
   if n `mod` 3 == 0 then
      ntak shuffle h (n+3) (x-1) y z
   else if n `mod` 3 == 1 then
      ntak shuffle h (n+2) (y-1) z x
   else
      ntak shuffle h (n+1) (z-1) x y;


third x y z = z

result = shuffle third 0 24 16 8
