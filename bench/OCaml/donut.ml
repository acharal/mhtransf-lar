open Unix
open Printf

let rec donut a b c d e num =
      if num == 0 then
         0
      else
         a b c d e + donut a b c d (e + 17) (num - 1)
      
and four a b c d = a b c d
and three b c d = b c d
and two c d = c d
and one d = d + 1

let result () = donut four three two one 4 200

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
