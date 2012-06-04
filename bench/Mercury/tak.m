:- module tak.

:- interface.

:- import_module io.
:- import_module int.

:- func tak(func(int, int, int) = int,
            int, int, int) = int.
:- mode tak(in, in, in, in) = out is det.

:- func third(int, int, int) = int.
:- mode third(in, in, in) = out is det.

:- func result = int.
:- mode result = out is det.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

tak(H, X, Y, Z) = R :- (
   if X =< Y then
      R = H(X, Y, Z)
   else
      R = tak(H, tak(H, X-1, Y, Z),
                 tak(H, Y-1, Z, X),
                 tak(H, Z-1, X, Y))
).

third(X, Y, Z) = Z.

result = tak(third, 24, 16, 8).

main -->
	io__write_string("Result: "),
	io__write_int(result),
	io__nl.
