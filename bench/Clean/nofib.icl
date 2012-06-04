module nofib
import StdEnv
Start = result

nofib n = if (n <= 1) 1 (nofib (n-1) + nofib (n-2) + 1)
result = nofib 40
