nofib n = if n <= 1 then 1 else nofib (n-1) + nofib (n-2) + 1;
result = nofib 30 
