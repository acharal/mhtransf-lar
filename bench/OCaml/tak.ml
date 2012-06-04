open Unix
open Printf

let rec tak h x y z =
      if x <= y then
         h x y z
      else
         tak h (tak h (x-1) y z)
               (tak h (y-1) z x)
               (tak h (z-1) x y)

and third x y z = z

let result () = tak third 24 16 8

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
