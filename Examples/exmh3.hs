result = ffac sq 5;
ffac h n = if n <= 1 then 1 else h n * ffac h (n - 1);
sq a = a * a
