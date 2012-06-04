module Testing where

import Types
import Syntax
import HFtoHI
import HItoHI
import HItoZI
import TypeInfer
import AuxFunGen


-- ------|---------|-----      testing       ----|---------|---------|

p0 = 
   ProgF [
   DefF "result" [] (ConF "+" [FF "apply" [FF "inc" [],
      ConF "8" []],FF "apply" [FF "dec" [],ConF "5" []]]),
   DefF "apply" ["f","x"] (FF "f" [XF "x"]),
   DefF "inc" ["y"] (ConF "+" [XF "y",ConF "1" []]),
   DefF "dec" ["a"] (ConF "-" [XF "a",ConF "1" []])]
e0 =
   [("dec",(T [T [] I] I)),("inc",(T [T [] I] I)),
    ("apply",(T [(T [T [] I] I),(T [] I)] I)),("a",(T [] I)),
    ("y",(T [] I)),("f",(T [T [] I] I)),("x",(T [] I)),
    ("result",T [] I)]

p0_1 =
   fromHFtoHI p0
e0_1 =
   [("dec",(T [T [] I] I)),("inc",(T [T [] I] I)),
    ("apply",(T [(T [T [] I] I),(T [] I)] I)),("inc_y",(T [] I)),
    ("dec_a",(T [] I)),("apply_f",(T [T [] I] I)),
    ("apply_x",(T [] I)),("result",T [] I)]

e0_2 =
   snd3 (fromHItoHI e0_1 p0_1)
p0_2 =
   fst3 (fromHItoHI e0_1 p0_1)
vnL0 =
   trd3 (fromHItoHI e0_1 p0_1)

e0_3 =
   e0_2
p0_3 =
   fromHItoZI p0_2

-- next

p1 =
   ProgF [
   DefF "result" [] (FF "ffac" [FF "sq" [],ConF "5" []]),
   DefF "ffac" ["h","n"] (ConF "if" [ConF "<=" [XF "n",ConF "1" []],
      ConF "1" [],ConF "*" [FF "h" [XF "n"],FF "ffac" [FF "h" [],
      ConF "-" [XF "n",ConF "1" []]]]]),
   DefF "sq" ["a"] (ConF "*" [XF "a",XF "a"])]
e1 =
   [("sq",(T [T [] I] I)),("ffac",(T [(T [T [] I] I),(T [] I)] I)),
    ("n",(T [] I)),("h",(T [T [] I] I)),("a",(T [] I)),
    ("result",T [] I)]

p1_1 =
   fromHFtoHI p1
e1_1 =
   [("sq",(T [T [] I] I)),("ffac",(T [(T [T [] I] I),(T [] I)] I)),
    ("ffac_n",(T [] I)),("ffac_h",(T [T [] I] I)),("sq_a",(T [] I)),
    ("result",T [] I)]

p1_2 =
   fst3 (fromHItoHI e1_1 p1_1)
e1_2 =
   snd3 (fromHItoHI e1_1 p1_1)
vnL1 =
   trd3 (fromHItoHI e1_1 p1_1)

e1_3 =
   e1_2
p1_3 =
   fromHItoZI p1_2

-- next

p2 =
   ProgF [
   DefF "result" [] (ConF "+" [FF "f" [ConF "4" []],
      FF "f" [ConF "5" []]]),
   DefF "f" ["x"] (FF "g" [ConF "+" [XF "x",ConF "1" []]]),
   DefF "g" ["y"] (XF "y")]
e2 =
   [("f",T [T [] I] I),("g",T [T [] I] I),("x",T [] I),("y",T [] I),
    ("result",T [] I)]

p2_1 =
   fromHFtoHI p2
e2_1 =
   [("f",T [T [] I] I),("g",T [T [] I] I),("f_x",T [] I),
    ("g_y",T [] I),("result",T [] I)]

p2_2 =
   fst3 (fromHItoHI e2_1 p2_1)
e2_2 =
   snd3 (fromHItoHI e2_1 p2_1)
vnL2 =
   trd3 (fromHItoHI e2_1 p2_1)

e2_3 =
   e2_2
p2_3 =
   fromHItoZI p2_2

-- next

p3 =
   ProgF [
   DefF "result" [] (FF "third" [FF "twice" [],FF "sq" [],
      ConF "3" []]),
   DefF "third" ["G","f","x"] (FF "G" [FF "f" [],XF "x"]),
   DefF "twice" ["h","y"] (FF "h" [FF "h" [XF "y"]]),
   DefF "sq" ["z"] (ConF "*" [XF "z",XF "z"])]
e3 = 
   [("third",T [T [T [T [] I] I,T [] I] I,T [T [] I] I,T [] I] I),
    ("G",T [T [T [] I] I,T [] I] I),("f",T [T [] I] I),
    ("h",T [T [] I] I),("sq",T [T [] I] I),
    ("twice",T [T [T [] I] I,T [] I] I),("y",T [] I),("z",T [] I),
    ("x",T [] I),("result",T [] I)]

p3_1 =
   fromHFtoHI p3

e3_1 =
   [("third",T [T [T [T [] I] I,T [] I] I,T [T [] I] I,T [] I] I),
    ("third_G",T [T [T [] I] I,T [] I] I),("third_f",T [T [] I] I),
    ("twice_h",T [T [] I] I),("sq",T [T [] I] I),
    ("twice",T [T [T [] I] I,T [] I] I),("twice_y",T [] I),
    ("sq_z",T [] I),("third_x",T [] I),("result",T [] I)]

p3_2 =
   fst3 (fromHItoHI e3_1 p3_1)
e3_2 =
   snd3 (fromHItoHI e3_1 p3_1)
vnL3 =
   trd3 (fromHItoHI e3_1 p3_1)

e3_3 =
   e3_2
p3_3 =
   fromHItoZI p3_2   

-- next

p4 =
   ProgF [
   DefF "result" [] (FF "h" [ConF "2" [],ConF "3" [],
      FF "f" []]),
   DefF "f" ["x","y","z"] (ConF "*" 
      [ConF "+" [XF "z",XF "y"],XF "x"]),
   DefF "h" ["n","m","f"] (ConF "+" [FF "f" [ConF "3" [],
      ConF "+" [XF "n",ConF "1" []],ConF "-" [XF "m",ConF "3" []]],
      FF "f" [ConF "-" [FF "g" [ConF "5" []],ConF "2" []],
      ConF "5" [],ConF "7" []]]),
   DefF "g" ["z"] (XF "z")]
e4 = 
   [("f",T [T [] I,T [] I,T [] I] I),
    ("f",T [T [] I,T [] I,T [] I] I),
    ("h",T [T [] I,T [] I,T [T [] I,T [] I,T [] I] I] I),
    ("g",T [T [] I] I),
    ("x",T [] I),("z",T [] I),("z",T [] I),
    ("n",T [] I),("m",T [] I),("y",T [] I),
    ("result",T [] I)]

p4_1 =
   fromHFtoHI p4

e4_1 =
   [("f",T [T [] I,T [] I,T [] I] I),
    ("h_f",T [T [] I,T [] I,T [] I] I),
    ("h",T [T [] I,T [] I,T [T [] I,T [] I,T [] I] I] I),
    ("g",T [T [] I] I),
    ("f_x",T [] I),("g_z",T [] I),("f_z",T [] I),
    ("h_n",T [] I),("h_m",T [] I),("f_y",T [] I),
    ("result",T [] I)]

p4_2 =
   fst3 (fromHItoHI e4_1 p4_1)
e4_2 =
   snd3 (fromHItoHI e4_1 p4_1)
vnL4 =
   trd3 (fromHItoHI e4_1 p4_1)

e4_3 =
   e4_2
p4_3 =
   fromHItoZI p4_2   

-- next

p5 =
   ProgF [
   DefF "result" [] (FF "four" [FF "three" [],FF "two" [],
      FF "one" [],ConF "4" []]),
   DefF "four" ["a","b","c","d"] (FF "a" 
      [FF "b" [],FF "c" [],XF "d"]),
   DefF "three" ["b","c","d"] (FF "b" [FF "c" [],
      XF "d"]),
   DefF "two" ["c","d"] (FF "c" [XF "d"]),
   DefF "one" ["d"] (ConF "+" [XF "d",ConF "1" []])]

p5_1 =
   fromHFtoHI p5

e5_1 =
   makeVarEnv p5_1

p5_2 =
   fst3 (fromHItoHI e5_1 p5_1)
e5_2 =
   snd3 (fromHItoHI e5_1 p5_1)
vnL5 =
   trd3 (fromHItoHI e5_1 p5_1)

e5_3 =
   e5_2
p5_3 =
   fromHItoZI p5_2   

-- next

p6 =
   ProgF [
   DefF "result" [] (FF "apply" [FF "four" [],ConF "3" []]),
   DefF "four" ["a"] (FF "w" [XF "a"]),
   DefF "apply" ["f","x"] (FF "f" [XF "x"]),
   DefF "w" ["b"] (ConF "*" [FF "sq" [XF "b"],FF "sq" [XF "b"]]),
   DefF "sq" ["c"] (ConF "*" [XF "c",XF "c"])]

p6_1 =
   fromHFtoHI p6

e6_1 =
   makeVarEnv p6_1

p6_2 =
   fst3 (fromHItoHI e6_1 p6_1)
e6_2 =
   snd3 (fromHItoHI e6_1 p6_1)
vnL6 =
   trd3 (fromHItoHI e6_1 p6_1)

e6_3 =
   e6_2
p6_3 =
   fromHItoZI p6_2
   
-- next

p7 =
   ProgF [
   DefF "result" [] (FF "three" [FF "two" [],FF "sq" [],
      ConF "3" []]),
   DefF "h" ["c","d"] (FF "c" [FF "c" [XF "d"]]),
   DefF "three" ["g","f","x"] (FF "g" [FF "f" [],XF "x"]),
   DefF "two" ["f","x"] (FF "h" [FF "f" [],XF "x"]),
   DefF "sq" ["c"] (ConF "*" [XF "c",XF "c"])]

p7_1 =
   fromHFtoHI p7

e7_1 =
   makeVarEnv p7_1

p7_2 =
   fst3 (fromHItoHI e7_1 p7_1)
e7_2 =
   snd3 (fromHItoHI e7_1 p7_1)
vnL7 =
   trd3 (fromHItoHI e7_1 p7_1)

e7_3 =
   e7_2
p7_3 =
   fromHItoZI p7_2   

-- next

p8 =
   ProgF [
   DefF "result" [] (FF "apply" [FF "four" [],ConF "3" []]),
   DefF "apply" ["f","x"] (FF "f" [XF "x"]),
   DefF "four" ["a"] (FF "w" [XF "a"]),
   DefF "w" ["b"] (ConF "*" [FF "z" [XF "b"],
      FF "three" [XF "b",FF "z" [XF "b"]]]),
   DefF "z" ["y"] (ConF "*" [FF "three" [XF "y",XF "y"],XF "y"]),
   DefF "three" ["y","k"] (ConF "+" [FF "m" [XF "y",XF "k"],
      ConF "+" [FF "n" [XF "y",XF "k"],FF "two" [XF "y",XF "k",
      ConF "21" [],ConF "-" [FF "n" [XF "y",XF "k"],
      FF "m" [XF "y",XF "k"]]]]]),
   DefF "m" ["y","k"] (ConF "*" [ConF "4" [],XF "k"]),
   DefF "n" ["y","k"] (ConF "*" [ConF "*" [XF "k",FF "m" 
      [XF "y",XF "k"]],FF "two" [XF "y",XF "k",ConF "5" [],XF "k"]]),
   DefF "two" ["y","k","p","a"] (ConF "+" [XF "p",
      ConF "*" [ConF "2" [],XF "a"]])]

p8_1 =
   fromHFtoHI p8

e8_1 =
   makeVarEnv p8_1

p8_2 =
   fst3 (fromHItoHI e8_1 p8_1)
e8_2 =
   snd3 (fromHItoHI e8_1 p8_1)
vnL8 =
   trd3 (fromHItoHI e8_1 p8_1)

e8_3 =
   e8_2
p8_3 =
   fromHItoZI p8_2

-- next

p9 =
   ProgF [
   DefF "result" [] (FF "f" [ConF "3" [],ConF "2" []]),
   DefF "f" ["n","m"] (FF "g" [ConF "*" [ConF "+" [XF "n",XF "m"],
      ConF "4" []]]),
   DefF "g" ["x"] (ConF "+" [XF "x",ConF "2" []])]

p9_1 =
   fromHFtoHI p9

e9_1 =
   makeVarEnv p9_1

p9_2 =
   fst3 (fromHItoHI e9_1 p9_1)
e9_2 =
   snd3 (fromHItoHI e9_1 p9_1)
vnL9 =
   trd3 (fromHItoHI e9_1 p9_1)

e9_3 =
   e9_2
p9_3 =
   fromHItoZI p9_2

-- next

p10 =
   ProgF [
   DefF "result" [] (FF "qsort" [makelist [ConF "32" [],
--                                           ConF "7" [],
--                                           ConF "42" [],
--                                           ConF "8" [],
                                           ConF "1" [],
--                                           ConF "23" [],
                                           ConF "1" [],
                                           ConF "-1" [],
                                           ConF "-14" [],
--                                           ConF "0" [],
--                                           ConF "41" [],
                                           ConF "1200000" []]]),
   DefF "append" ["l1", "l2"]
      (cond (empty (XF "l1"))
            (XF "l2")
            (cons (car (XF "l1"))
                  (FF "append" [cdr (XF "l1"),XF "l2"]))),
   DefF "filter2" ["f", "p", "l"]
      (cond (empty (XF "l"))
            nil
            (cond (FF "f" [car (XF "l"),XF "p"])
                  (cons (car (XF "l"))
                        (FF "filter2" 
                              [FF "f" [],XF "p",cdr (XF "l")]))
                  (FF "filter2" 
                        [FF "f" [],XF "p",cdr (XF "l")]))),
   DefF "lower" ["x", "y"] ((XF "x") `lt` (XF "y")),
   DefF "greaterEq" ["x", "y"] ((XF "x") `ge` (XF "y")),
   DefF "qsort" ["l"]
      (cond (empty (XF "l"))
            nil
            (FF "append" [
                FF "qsort" [
                   FF "filter2" [
                      FF "lower" [],
                      car (XF "l"),
                      cdr (XF "l")
                      ]
                   ],
                cons (car (XF "l"))
                     (FF "qsort" [
                         FF "filter2" [
                            FF "greaterEq" [],
                            car (XF "l"),
                            cdr (XF "l")
                            ]
                         ])
                ])
        )
    ]

-- next

p11 =
   ProgF [
--   DefF "result" [] (equal (FF "length" [XF "l"]) (numconst 6)),
   DefF "result" [] (XF "l"),   
   DefF "append" ["l1", "l2"]
      (cond (empty (XF "l1"))
            (XF "l2")
            (cons (car (XF "l1"))
                  (FF "append" [cdr (XF "l1"),XF "l2"]))),
   DefF "l" [] (FF "append" [makelist [numconst 32,
                                       numconst 21],
                             makelist [numconst 42,
                                       numconst 8,
                                       numconst 1,
                                       numconst 23]
                             ]),
   DefF "length" ["l"] 
      (cond (empty (XF "l"))
            (numconst 0)
            (plus (numconst 1) (FF "length" [cdr (XF "l")]))
      )
   ]

-- next

p12 =
   ProgF [
   DefF "result" [] (ConF "+" [FF "f" [ConF "3" []],
      FF "g" [ConF "2" []]]),
   DefF "f" ["n"] (XF "n"),
   DefF "g" ["x"] (FF "f" [ConF "3" []])]

p12_1 =
   fromHFtoHI p12

e12_1 =
   makeVarEnv p12_1

p12_2 =
   fst3 (fromHItoHI e12_1 p12_1)
e12_2 =
   snd3 (fromHItoHI e12_1 p12_1)
vnL12 =
   trd3 (fromHItoHI e12_1 p12_1)

e12_3 =
   e12_2
p12_3 =
   fromHItoZI p12_2


-- next

p13 =
   ProgF [
   DefF "result" [] (FF "f" [FF "f" [FF "f" [ConF "3" []]]]),
   DefF "f" ["n"] (ConF "+" [XF "n",ConF "1" []])]

p13_1 =
   fromHFtoHI p13

e13_1 =
   makeVarEnv p13_1

p13_2 =
   fst3 (fromHItoHI e13_1 p13_1)
e13_2 =
   snd3 (fromHItoHI e13_1 p13_1)
vnL13 =
   trd3 (fromHItoHI e13_1 p13_1)

e13_3 =
   e13_2
p13_3 =
   fromHItoZI p13_2


-- next

p14 =
   ProgF [
   DefF "result" [] (ConF "3" [])]

p14_1 =
   fromHFtoHI p14

e14_1 =
   [("result",T [] I)]

p14_2 =
   fst3 (fromHItoHI e14_1 p14_1)
e14_2 =
   snd3 (fromHItoHI e14_1 p14_1)
vnL14 =
   trd3 (fromHItoHI e14_1 p14_1)

e14_3 =
   e14_2
p14_3 =
   fromHItoZI p14_2


-- next

p15 =
   ProgF [
   DefF "result" [] (FF "fib" [numconst 4]),
   DefF "fib" ["n"] (cond (lt (XF "n") (numconst 2)) (numconst 1) 
      (plus (FF "fib" [minus (XF "n") (numconst 1)]) 
            (FF "fib" [minus (XF "n") (numconst 2)])))]

p15_1 =
   fromHFtoHI p15

e15_1 =
   makeVarEnv p15_1

p15_2 =
   fst3 (fromHItoHI e15_1 p15_1)
e15_2 =
   snd3 (fromHItoHI e15_1 p15_1)
vnL15 =
   trd3 (fromHItoHI e15_1 p15_1)

e15_3 =
   e15_2
p15_3 =
   fromHItoZI p15_2

-- next

p16 =
   ProgF [
   DefF "result" [] (FF "donut" [FF "four" [],FF "three" [],
      FF "two" [],FF "one" [],ConF "4" [],numconst 200]),
   DefF "donut" ["a","b","c","d","e","num"] (cond (equal (XF "num") 
      (numconst 0)) (numconst 0) (plus (FF "a" [FF "b" [],FF "c" [],
      FF "d" [],XF "e"]) (FF "donut" [FF "a" [],FF "b" [],FF "c" [],
      FF "d" [],plus (XF "e") (numconst 17),minus (XF "num") 
      (numconst 1)]))),
   DefF "four" ["a","b","c","d"] (FF "a" 
      [FF "b" [],FF "c" [],XF "d"]),
   DefF "three" ["b","c","d"] (FF "b" [FF "c" [],
      XF "d"]),
   DefF "two" ["c","d"] (FF "c" [XF "d"]),
   DefF "one" ["d"] (ConF "+" [XF "d",ConF "1" []])]

p16_1 =
   fromHFtoHI p16

e16_1 =
   makeVarEnv p16_1

p16_2 =
   fst3 (fromHItoHI e16_1 p16_1)
e16_2 =
   snd3 (fromHItoHI e16_1 p16_1)
vnL16 =
   trd3 (fromHItoHI e16_1 p16_1)

e16_3 =
   e16_2
p16_3 =
   fromHItoZI p16_2

-- next

p17 =
   ProgF [
   DefF "result" [] (FF "donut" [FF "four" [],FF "three" [],
      FF "two" [],FF "one" [],ConF "4" [],numconst 200]),
   DefF "donut" ["a","b","c","d","e","num"] (cond (equal (XF "num") 
      (numconst 0)) (numconst 0) (plus (FF "a" [FF "b" [],FF "c" [],
      FF "d" [],XF "e"]) (FF "donut" [FF "a" [],FF "b" [],FF "c" [],
      FF "d" [],XF "e",minus (XF "num") 
      (numconst 1)]))),
   DefF "four" ["a","b","c","d"] (FF "a" 
      [FF "b" [],FF "c" [],XF "d"]),
   DefF "three" ["b","c","d"] (FF "b" [FF "c" [],
      XF "d"]),
   DefF "two" ["c","d"] (FF "c" [XF "d"]),
   DefF "one" ["d"] (ConF "+" [XF "d",ConF "1" []])]

p17_1 =
   fromHFtoHI p17

e17_1 =
   makeVarEnv p17_1

p17_2 =
   fst3 (fromHItoHI e17_1 p17_1)
e17_2 =
   snd3 (fromHItoHI e17_1 p17_1)
vnL17 =
   trd3 (fromHItoHI e17_1 p17_1)

e17_3 =
   e17_2
p17_3 =
   fromHItoZI p17_2

-- next

p18 =
   ProgF [
   DefF "result" [] (FF "donut" [FF "four" [],FF "three" [],
      FF "two" [],FF "fib" [],numconst 12,numconst 200]),
   DefF "donut" ["a","b","c","d","e","num"] (cond (equal (XF "num") 
      (numconst 0)) (numconst 0) (plus (FF "a" [FF "b" [],FF "c" [],
      FF "d" [],XF "e"]) (FF "donut" [FF "a" [],FF "b" [],FF "c" [],
      FF "d" [],XF "e",minus (XF "num") 
      (numconst 1)]))),
   DefF "four" ["a","b","c","d"] (FF "a" 
      [FF "b" [],FF "c" [],XF "d"]),
   DefF "three" ["b","c","d"] (FF "b" [FF "c" [],
      XF "d"]),
   DefF "two" ["c","d"] (FF "c" [XF "d"]),
   DefF "fib" ["n"] (cond (lt (XF "n") (numconst 2)) (numconst 1) 
      (plus (FF "fib" [minus (XF "n") (numconst 1)]) 
            (FF "fib" [minus (XF "n") (numconst 2)])))]

p18_1 =
   fromHFtoHI p18

e18_1 =
   makeVarEnv p18_1

p18_2 =
   fst3 (fromHItoHI e18_1 p18_1)
e18_2 =
   snd3 (fromHItoHI e18_1 p18_1)
vnL18 =
   trd3 (fromHItoHI e18_1 p18_1)

e18_3 =
   e18_2
p18_3 =
   fromHItoZI p18_2


-- ------|---------|---------|---------|---------|---------|---------|