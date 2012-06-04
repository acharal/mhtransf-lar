open Unix
open Printf

let rec identity i = i
and successor i = i + 1
and successor_h f x = f x + 1

and church i f x =
      if i == 0 then
         x
      else
         church (i-1) f (f x)

and church_h i f x z =
      if i == 0 then
         x z
      else
         church_h (i-1) f x (f x z)

and unchurch n = n successor 0

and unchurch_h n = n successor_h identity 0

and c_succ n f x = n f (f x)
   
and c_plus n m f x = n f (m f x)

and c_nonzero n a b f x = if unchurch n == 0 then b f x else a f x

and c_exp n m f x = m n f x

and c_2x0 n f x = c_plus n n f x
and c_2x1 n f x = n f (n f (f x))

and c0 f x = church 0 f x
and c1 f x = c_2x1 c0 f x
and c3 f x = c_2x1 c1 f x
and c6 f x = c_2x0 c3 f x
and c12 f x = c_2x0 c6 f x
and c24 f x = c_2x0 c12 f x
and c48 f x = c_2x0 c24 f x
and c96 f x = c_2x0 c48 f x
and c192 f x = c_2x0 c96 f x
and c385 f x = c_2x1 c192 f x
and c771 f x = c_2x1 c385 f x
and c1543 f x = c_2x1 c771 f x
and c3087 f x = c_2x1 c1543 f x
and c6174 f x = c_2x0 c3087 f x

and c7 f x = c_succ c6 f x
and c3_h f x z = church_h 3 f x z
and c343 f x = c_exp c7 c3_h f x

and c f x = c_nonzero c3 c6174 c343 f x

let result () = unchurch c

let main =
      let t1 = gettimeofday () in
      let it = result () in
      let t2 = gettimeofday () in
      printf "Result:\t%d\n" it;
      printf "Time:\t%0.6f\n" (t2 -. t1)
