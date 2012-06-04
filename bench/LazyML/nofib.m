let rec nofib n =
      if n <= 1 then 1 else nofib (n-1) + nofib (n-2) + 1

in

let result = nofib 30
in  "Result: " @ itos result @ "\n"
