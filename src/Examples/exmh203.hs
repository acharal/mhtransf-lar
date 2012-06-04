fun f x = if x == 0 then f 0 + fun f 1 else 0;
one a = a+1;
two b = b+2;
id t = t;
diff g = id (fun g 0);
result = diff one + diff two
