result = repeat 200 fib 15;
fib x = if x <= 1 then 1 else fib(x-1) + fib(x-2);
repeat n f a = repeat_aux n f a 0;
repeat_aux n f a b = if n > 0 then repeat_aux (n-1) f a (b + f a - b) else b
