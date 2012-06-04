:- module primes.

:- interface.

:- import_module io.
:- import_module int.

:- func primes(int) = int.
:- mode primes(in) = out is det.

:- func findPrimeMinus(int, int) = int.
:- mode findPrimeMinus(in, in) = out is det.

:- func findPrimePlus(int, int) = int.
:- mode findPrimePlus(in, in) = out is det.

:- func testPrime(int, int) = int.
:- mode testPrime(in, in) = out is det.

:- func result = int.
:- mode result = out is det.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- implementation.

primes(N) = P :- (
   if N =< 0 then
      P = 2
   else if N =< 1 then
      P = 3
   else
      P = findPrimeMinus(N-2, 1)
).
      
findPrimeMinus(N, I) = P :- (
   if testPrime(6*I-1, 1) = 1 then (
      if N = 0 then
         P = 6*I-1
      else
         P = findPrimePlus(N-1, I)
   )
   else
      P = findPrimePlus(N, I)
).

findPrimePlus(N, I) = P :- (
   if testPrime(6*I+1, 1) = 1 then (
      if N = 0 then
         P = 6*I+1
      else
         P = findPrimeMinus(N-1, I+1)
   )
   else
      P = findPrimeMinus(N, I+1)
).

testPrime(N, I) = B :- (
   if (6*I-1) * (6*I-1) > N then
      B = 1
   else (if N mod (6*I-1) = 0 then
      B = 0
   else (if N mod (6*I+1) = 0 then
      B = 0
   else
      B = testPrime(N, I+1)
   ))
).

result = primes(50000).

main -->
	io__write_string("Result: "),
	io__write_int(result),
	io__nl.
