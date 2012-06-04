:- module ntak.

:- interface.

:- import_module io.
:- import_module int.

:- func shuffle(func(int, int, int) = int,
                int, int, int, int) = int.
:- mode shuffle(in, in, in, in, in) = out is det.

:- func ntak(func(func(int, int, int) = int,
                  int, int, int, int) = int,
             func(int, int, int) = int,
             int, int, int, int) = int.
:- mode ntak(in, in, in, in, in, in) = out is det.

:- func third(int, int, int) = int.
:- mode third(in, in, in) = out is det.

:- func result = int.
:- mode result = out is det.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

shuffle(H, X, Y, Z, N) = R :- (
   if N mod 3 = 0 then
      R = ntak(shuffle, H, N+3, X-1, Y, Z)
   else (if N mod 3 = 1 then
      R = ntak(shuffle, H, N+2, Y-1, Z, X)
   else
      R = ntak(shuffle, H, N+1, Z-1, X, Y)
   )
).

ntak(F, H, N, X, Y, Z) = R :- (
   if X =< Y then
      R = H(X, Y, Z)
   else
      R = ntak(F, H, N, F(H, X, Y, Z, N),
                        F(H, X, Y, Z, N+1),
                        F(H, X, Y, Z, N+2))
).

third(X, Y, Z) = Z.

result = ntak(shuffle, third, 0, 24, 16, 8).

main -->
	io__write_string("Result: "),
	io__write_int(result),
	io__nl.
