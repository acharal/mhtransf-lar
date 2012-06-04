donut a b c d e num =
   if num == 0 then
      0
   else
      a b c d e + donut four b c d (e + 17) (num - 1);
      
four a b c d = a b c d;
three b c d = b c d;
two c d = c d;
one d = d + 1;

result = donut four three two one 4 1200
