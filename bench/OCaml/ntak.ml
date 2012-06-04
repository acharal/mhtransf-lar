open Unix
open Printf

let rec shuffle h x y z n =
      if n mod 3 == 0 then
         ntak shuffle h (n+3) (x-1) y z
      else if n mod 3 == 1 then
         ntak shuffle h (n+2) (y-1) z x
      else
         ntak shuffle h (n+1) (z-1) x y

and ntak f h n x y z =
      if x <= y then
         h x y z
      else
         ntak f h n (f h x y z n)
                    (f h x y z (n+1))
                    (f h x y z (n+2))

and third x y z = z

let result () = ntak shuffle third 0 24 16 8

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
