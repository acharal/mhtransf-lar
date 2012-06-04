:- module church.

:- interface.

:- import_module io.
:- import_module int.

:- func identity(int) = int.
:- mode identity(in) = out is det.

:- func successor(int) = int.
:- mode successor(in) = out is det.

:- func successor_h(func(int) = int, int) = int.
:- mode successor_h(in, in) = out is det.

:- func church(int, func(int)=int, int) = int.
:- mode church(in, in, in) = out is det.

:- func church_h(int, func(func(int)=int, int)=int, func(int)=int, int) = int.
:- mode church_h(in, in, in, in) = out is det.

:- func unchurch(func(func(int)=int, int)=int) = int.
:- mode unchurch(in) = out is det.

:- func unchurch_h(func(func(func(int)=int, int)=int,
                        func(int)=int, int) = int) = int.
:- mode unchurch_h(in) = out is det.

:- func c_succ(func(func(int)=int, int)=int, func(int)=int, int) = int.
:- mode c_succ(in, in, in) = out is det.

:- func c_plus(func(func(int)=int, int)=int, func(func(int)=int, int)=int, func(int)=int, int) = int.
:- mode c_plus(in, in, in, in) = out is det.

:- func c_nonzero(func(func(int)=int, int)=int,
                  func(func(int)=int, int)=int,
                  func(func(int)=int, int)=int, func(int) = int, int) = int.
:- mode c_nonzero(in, in, in, in, in) = out is det.

:- func c_exp(func(func(int)=int, int)=int, 
              func(func(func(int)=int, int)=int,
                        func(int)=int, int) = int,
              func(int)=int, int) = int.
:- mode c_exp(in, in, in, in) = out is det.

:- func c_2x0(func(func(int)=int, int)=int, func(int)=int, int) = int.
:- mode c_2x0(in, in, in) = out is det.

:- func c_2x1(func(func(int)=int, int)=int, func(int)=int, int) = int.
:- mode c_2x1(in, in, in) = out is det.

:- func c0(func(int)=int, int) = int.
:- mode c0(in, in) = out is det.

:- func c1(func(int)=int, int) = int.
:- mode c1(in, in) = out is det.

:- func c3(func(int)=int, int) = int.
:- mode c3(in, in) = out is det.

:- func c6(func(int)=int, int) = int.
:- mode c6(in, in) = out is det.

:- func c12(func(int)=int, int) = int.
:- mode c12(in, in) = out is det.

:- func c24(func(int)=int, int) = int.
:- mode c24(in, in) = out is det.

:- func c48(func(int)=int, int) = int.
:- mode c48(in, in) = out is det.

:- func c96(func(int)=int, int) = int.
:- mode c96(in, in) = out is det.

:- func c192(func(int)=int, int) = int.
:- mode c192(in, in) = out is det.

:- func c385(func(int)=int, int) = int.
:- mode c385(in, in) = out is det.

:- func c771(func(int)=int, int) = int.
:- mode c771(in, in) = out is det.

:- func c1543(func(int)=int, int) = int.
:- mode c1543(in, in) = out is det.

:- func c3087(func(int)=int, int) = int.
:- mode c3087(in, in) = out is det.

:- func c6174(func(int)=int, int) = int.
:- mode c6174(in, in) = out is det.

:- func c7(func(int)=int, int) = int.
:- mode c7(in, in) = out is det.

:- func c3_h(func(func(int)=int, int)=int, func(int)=int, int) = int.
:- mode c3_h(in, in, in) = out is det.

:- func c343(func(int)=int, int) = int.
:- mode c343(in, in) = out is det.

:- func c(func(int)=int, int) = int.
:- mode c(in, in) = out is det.

:- func result = int.
:- mode result = out is det.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

identity(I) = I.
successor(I) = I + 1.
successor_h(F, X) = F(X) + 1.

church(I, F, X) = R :- (
   if I = 0 then
      R = X
   else
      R = church(I-1, F, F(X))
).

church_h(I, F, X, Z) = R :- (
   if I = 0 then
      R = X(Z)
   else
      R = church_h(I-1, F, X, F(X, Z))
).

unchurch(N) = N(successor, 0).

unchurch_h(N) = N(successor_h, identity, 0).

c_succ(N, F, X) = N(F, F(X)).
   
c_plus(N, M, F, X) = N(F, M(F, X)).

c_nonzero(N, A, B, F, X) = R :- (
   if unchurch(N) = 0 then
      R = B(F, X)
   else
      R = A(F, X)
).

c_exp(N, M, F, X) = M(N, F, X).

c_2x0(N, F, X) = c_plus(N, N, F, X).
c_2x1(N, F, X) = N(F, N(F, F(X))).

c0(F, X) = church(0, F, X).
c1(F, X) = c_2x1(c0, F, X).
c3(F, X) = c_2x1(c1, F, X).
c6(F, X) = c_2x0(c3, F, X).
c12(F, X) = c_2x0(c6, F, X).
c24(F, X) = c_2x0(c12, F, X).
c48(F, X) = c_2x0(c24, F, X).
c96(F, X) = c_2x0(c48, F, X).
c192(F, X) = c_2x0(c96, F, X).
c385(F, X) = c_2x1(c192, F, X).
c771(F, X) = c_2x1(c385, F, X).
c1543(F, X) = c_2x1(c771, F, X).
c3087(F, X) = c_2x1(c1543, F, X).
c6174(F, X) = c_2x0(c3087, F, X).

c7(F, X) = c_succ(c6, F, X).
c3_h(F, X, Z) = church_h(3, F, X, Z).
c343(F, X) = c_exp(c7, c3_h, F, X).

c(F, X) = c_nonzero(c3, c6174, c343, F, X).

result = unchurch(c).

main -->
	io__write_string("Result: "),
	io__write_int(result),
	io__nl.
