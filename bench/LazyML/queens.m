let rec safe q x n s d =
      if n = 0 then
         true
      else
         x ~= s % q &
         x ~= s % q + d &
         x ~= s %  q - d &
         safe q x (n-1) (s / q) (d+1)

and count q x n s =
      if n < q then
         if x < q then
            if safe q x n s 1 then
               count q 0 (n+1) (x+q*s) + count q (x+1) n s
            else
               count q (x+1) n s
         else
            0
      else
         1

in

let result = count 8 0 0 0
in  "Result: " @ itos result @ "\n"
