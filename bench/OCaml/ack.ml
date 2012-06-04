open Unix
open Printf

let rec ack m n =
      if m == 0 then
         n + 1
      else if n == 0 then
      	 ack (m-1) 1
      else
      	 ack (m-1) (ack m (n-1))

and third x y z = z

let result () = ack 3 9

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
