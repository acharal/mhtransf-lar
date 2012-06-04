module church
import StdEnv
Start = result

identity i = i
successor i = i + 1
successor_h f x = f x + 1

church i f x =
   if (i == 0)
      x
      (church (i-1) f (f x))

church_h i f x z =
   if (i == 0)
      (x z)
      (church_h (i-1) f x (f x z))

unchurch n = n successor 0

unchurch_h n = n successor_h identity 0

c_succ n f x =
   n f (f x)
   
c_plus n m f x =
   n f (m f x)

c_nonzero n a b f x =
   if (unchurch n == 0) (b f x) (a f x)

c_exp n m f x =
   m n f x

c_2x0 n f x = c_plus n n f x
c_2x1 n f x = n f (n f (f x))

c0 f x = church 0 f x
c1 f x = c_2x1 c0 f x
c3 f x = c_2x1 c1 f x
c6 f x = c_2x0 c3 f x
c12 f x = c_2x0 c6 f x
c24 f x = c_2x0 c12 f x
c48 f x = c_2x0 c24 f x
c96 f x = c_2x0 c48 f x
c192 f x = c_2x0 c96 f x
c385 f x = c_2x1 c192 f x
c771 f x = c_2x1 c385 f x
c1543 f x = c_2x1 c771 f x
c3087 f x = c_2x1 c1543 f x
c6174 f x = c_2x0 c3087 f x

c7 f x = c_succ c6 f x
c3_h f x z = church_h 3 f x z
c343 f x = c_exp c7 c3_h f x

c f x = c_nonzero c3 c6174 c343 f x

result = unchurch c771
