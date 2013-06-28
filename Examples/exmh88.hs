result = qsort (32 : 7 : 42 : 8 : 1 : []);
append l1 l2 = if null l1 then l2 else head l1 : append (tail l1) l2;
filter2 f p l = if null l then [] else if f (head l) p then head l : filter2 f p (tail l) else filter2 f p (tail l);
qsort l = if null l then [] else append (qsort (filter2 lt (head l) (tail l))) (head l : qsort (filter2 ge (head l) (tail l)));
lt x y = x < y;
ge x y = x >= y
