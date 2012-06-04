module donut
import StdEnv
Start = result

donut a b c d e num =
   if (num == 0)
      0
      (a b c d e + donut a b c d (e + 17) (num - 1))
      
four a b c d = a b c d
three b c d = b c d
two c d = c d
my_one d = d + 1

result = donut four three two my_one 4 1200
