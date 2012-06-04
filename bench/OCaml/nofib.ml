open Unix
open Printf

let rec nofib n =
      if n <= 1 then 1 else nofib (n-1) + nofib (n-2) + 1

let result () =
      nofib 40

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
