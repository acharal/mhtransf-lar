module Syntax where

import Types

-- ------|---------|-----       HOFL         ----|---------|---------|

data HOFLExpr = XF VName 
	      |	ConF CName [HOFLExpr] 
	      |	FF VName [HOFLExpr] 
	deriving (Eq,Read)

data HOFLDef = DefF VName [VName] HOFLExpr
	deriving (Eq,Read)

data HOFLProg = ProgF [HOFLDef]
	deriving (Eq,Read)


-- pretty printing for HOFL

instance Show HOFLExpr where
   showsPrec p (XF vn) =
      (vn ++)
   showsPrec p (ConF cn el) =
      case cn of
         "if" ->
                showParen (p > 1) (
                    ("if " ++)   . showsPrec 0 (el !! 0) .
                    (" then " ++) . showsPrec 0 (el !! 1) .
                    (" else " ++) . showsPrec 1 (el !! 2)
                )
         c | c == "+" || c == "-" ->
                showParen (p > 5) (
                    showsPrec 6 (el !! 0) .
                    ((" " ++ c ++ " ") ++) . showsPrec 5 (el !! 1)
                )
         "*" ->
                showParen (p > 6) (
                    showsPrec 7 (el !! 0) .
                    (" * " ++) . showsPrec 6 (el !! 1)
                )
         c | c == "div" || c == "mod" ->
                showParen (p > 6) (
                    showsPrec 7 (el !! 0) .
                    ((" `" ++ c ++ "` ") ++) . showsPrec 6 (el !! 1)
                )
         c | c == "==" || c == "/=" || c == "<" || c == ">" ||
                c == "<=" || c == ">=" ->
                showParen (p > 4) (
                    showsPrec 5 (el !! 0) .
                    ((" " ++ c ++ " ") ++) . showsPrec 5 (el !! 1)
                )
         "&&" ->
                showParen (p > 3) (
                    showsPrec 4 (el !! 0) .
                    (" && " ++) . showsPrec 3 (el !! 1)
                )
         "||" ->
                showParen (p > 2) (
                    showsPrec 3 (el !! 0) .
                    (" || " ++) . showsPrec 2 (el !! 1)
                )
         ":" ->
                showParen (p > 1) (
                    showsPrec 0 (el !! 0) . (" : " ++) .
                    showsPrec 1 (el !! 1)
                )
         otherwise ->
                case reads cn :: [(Int, String)] of
                    [(n, "")] ->
                        showsPrec 0 n
                    otherwise ->
                        let showParams [] =
                                id
                            showParams l =
                                ("(" ++) . showSep l . (")" ++)
                            showSep [a] =
                                showsPrec 0 a
                            showSep (a : al) =
                                showsPrec 0 a.(", " ++).showSep al
                        in  (cn ++) . showParams el
   showsPrec p (FF vn ps) =
      let showParams []    = id
          showParams l     = ("(" ++) . showSep l . (")" ++)
          showSep [a]      = showsPrec 0 a
          showSep (a : al) = showsPrec 0 a.(", " ++).showSep al
      in  (vn ++).showParams ps

instance Show HOFLDef where
   showsPrec p (DefF vn ps e) =
      let showParams []    = id
          showParams l     = ("(" ++) . showSep l . (")" ++)
          showSep [a]      = (a ++)
          showSep (a : al) = (a ++) . (", " ++) . showSep al
      in  (vn ++).showParams ps.(" = " ++).showsPrec 0 e.("\n" ++)

instance Show HOFLProg where
   showsPrec p (ProgF ds) =
      foldl (\f -> \d -> f . showsPrec p d) id ds



-- ------|---------|-----       HOIL         ----|---------|---------|

data HOILExpr = XH VName 
	      |	ConH CName [HOILExpr] 
	      |	FH QOp VName [HOILExpr]
	deriving (Eq,Read)

data QOp = NOp | IOp :.: QOp
	deriving (Eq,Read)

data IOp = Call (IIndex,MIndex) 
         | Call' (IIndex,MIndex)
	 | Actuals (IIndex,MIndex)
	deriving (Eq,Read)

data HOILDef = DefH VName [VName] HOILExpr 
	     | DefCaseH VName [VName] MIndex [HOILExpr]
	deriving (Eq,Read)

data HOILProg = ProgH [HOILDef]
	deriving (Eq,Read)



-- pretty printing for HOIL

instance Show IOp where
   showsPrec p (Call (i,m)) =
      ("call[" ++).showsPrec 0 i.(", " ++).
                   showsPrec 0 m.("]" ++)
   showsPrec p (Call' (i,m)) =
      ("call*[" ++).showsPrec 0 i.(", " ++).
                   showsPrec 0 m.("]" ++)
   showsPrec p (Actuals (i,m)) =
      ("actuals[" ++).showsPrec 0 i.(", " ++).
                      showsPrec 0 m.("]" ++)

instance Show QOp where
   showsPrec p NOp = id
   showsPrec p (iOp :.: NOp) =
      showsPrec 0 iOp
   showsPrec p (iOp :.: qOp) =
      showsPrec 0 iOp.(". " ++).showsPrec 0 qOp

instance Show HOILExpr where
   showsPrec p (XH vn) =
      (vn ++)
   showsPrec p (ConH cn el) =
      case cn of
         "if" ->
                showParen (p > 1) (
                    ("if " ++)   . showsPrec 0 (el !! 0) .
                    (" then " ++) . showsPrec 0 (el !! 1) .
                    (" else " ++) . showsPrec 1 (el !! 2)
                )
         c | c == "+" || c == "-" ->
                showParen (p > 5) (
                    showsPrec 6 (el !! 0) .
                    ((" " ++ c ++ " ") ++) . showsPrec 5 (el !! 1)
                )
         "*" ->
                showParen (p > 6) (
                    showsPrec 7 (el !! 0) .
                    (" * " ++) . showsPrec 6 (el !! 1)
                )
         c | c == "div" || c == "mod" ->
                showParen (p > 6) (
                    showsPrec 7 (el !! 0) .
                    ((" `" ++ c ++ "` ") ++) . showsPrec 6 (el !! 1)
                )
         c | c == "==" || c == "/=" || c == "<" || c == ">" ||
                c == "<=" || c == ">=" ->
                showParen (p > 4) (
                    showsPrec 5 (el !! 0) .
                    ((" " ++ c ++ " ") ++) . showsPrec 5 (el !! 1)
                )
         "&&" ->
                showParen (p > 3) (
                    showsPrec 4 (el !! 0) .
                    (" && " ++) . showsPrec 3 (el !! 1)
                )
         "||" ->
                showParen (p > 2) (
                    showsPrec 3 (el !! 0) .
                    (" || " ++) . showsPrec 2 (el !! 1)
                )
         ":" ->
                showParen (p > 1) (
                    showsPrec 0 (el !! 0) . (" : " ++) .
                    showsPrec 1 (el !! 1)
                )
         otherwise ->
                case reads cn :: [(Int, String)] of
                    [(n, "")] ->
                        showsPrec 0 n
                    otherwise ->
                        let showParams [] =
                                id
                            showParams l =
                                ("(" ++) . showSep l . (")" ++)
                            showSep [a] =
                                showsPrec 0 a
                            showSep (a : al) =
                                showsPrec 0 a.(", " ++).showSep al
                        in  (cn ++) . showParams el
   showsPrec p (FH NOp vn ps) =
      let showParams []    = id
          showParams l     = ("(" ++) . showSep l . (")" ++)
          showSep [a]      = showsPrec 0 a
          showSep (a : al) = showsPrec 0 a.(", " ++).showSep al
      in  (vn ++).showParams ps
   showsPrec p (FH qOp vn ps) =
      let showParams []    = id
          showParams l     = (" (" ++) . showSep l . (")" ++)
          showSep [a]      = showsPrec 0 a
          showSep (a : al) = showsPrec 0 a.(", " ++).showSep al
      in  showsPrec 0 qOp.(" (" ++).(vn ++).(")" ++).
          showParams ps

instance Show HOILDef where
   showsPrec p (DefH vn ps e) =
      let showParams []    = id
          showParams l     = ("(" ++) . showSep l . (")" ++)
          showSep [a]      = (a ++)
          showSep (a : al) = (a ++) . (", " ++) . showSep al
      in  (vn ++).showParams ps.(" = " ++).showsPrec 0 e.("\n" ++)
   showsPrec p (DefCaseH vn ps m es) =
      let showParams []     = id
          showParams l      = ("(" ++) . showSep l . (")" ++)
          showSep [a]       = (a ++)
          showSep (a : al)  = (a ++) . (", " ++) . showSep al
          showEs []         = id
          showEs l          = (" (" ++) . showSepE l . (")" ++)
          showSepE [a]      = showsPrec 0 a
          showSepE (a : al) = showsPrec 0 a.(", " ++).showSepE al
      in  (vn ++).showParams ps.(" = case " ++).showsPrec 0 m.
                  showEs es.("\n" ++)

instance Show HOILProg where
   showsPrec p (ProgH ds) =
      foldl (\f -> \d -> f . showsPrec p d) id ds



-- ------|---------|-----       ZOIL         ----|---------|---------|

data ZOILExpr = XZ VName 
	      |	ConZ CName [ZOILExpr] 
	      |	FZ QOp ZOILExpr 
	deriving (Eq,Read)

data ZOILDef = DefZ VName ZOILExpr 
	     | DefCaseZ VName MIndex [ZOILExpr]
	deriving (Eq,Read)

data ZOILProg = ProgZ [ZOILDef]
	deriving (Eq,Read)



-- pretty printing for ZOIL

instance Show ZOILExpr where
   showsPrec p (XZ vn) =
      (vn ++)
   showsPrec p (ConZ cn el) =
      case cn of
         "if" ->
                showParen (p > 1) (
                    ("if " ++)   . showsPrec 0 (el !! 0) .
                    (" then " ++) . showsPrec 0 (el !! 1) .
                    (" else " ++) . showsPrec 1 (el !! 2)
                )
         c | c == "+" || c == "-" ->
                showParen (p > 5) (
                    showsPrec 6 (el !! 0) .
                    ((" " ++ c ++ " ") ++) . showsPrec 5 (el !! 1)
                )
         "*" ->
                showParen (p > 6) (
                    showsPrec 7 (el !! 0) .
                    (" * " ++) . showsPrec 6 (el !! 1)
                )
         c | c == "div" || c == "mod" ->
                showParen (p > 6) (
                    showsPrec 7 (el !! 0) .
                    ((" `" ++ c ++ "` ") ++) . showsPrec 6 (el !! 1)
                )
         c | c == "==" || c == "/=" || c == "<" || c == ">" ||
                c == "<=" || c == ">=" ->
                showParen (p > 4) (
                    showsPrec 5 (el !! 0) .
                    ((" " ++ c ++ " ") ++) . showsPrec 5 (el !! 1)
                )
         "&&" ->
                showParen (p > 3) (
                    showsPrec 4 (el !! 0) .
                    (" && " ++) . showsPrec 3 (el !! 1)
                )
         "||" ->
                showParen (p > 2) (
                    showsPrec 3 (el !! 0) .
                    (" || " ++) . showsPrec 2 (el !! 1)
                )
         ":" ->
                showParen (p > 1) (
                    showsPrec 0 (el !! 0) . (" : " ++) .
                    showsPrec 1 (el !! 1)
                )
         otherwise ->
                case reads cn :: [(Int, String)] of
                    [(n, "")] ->
                        showsPrec 0 n
                    otherwise ->
                        let showParams [] =
                                id
                            showParams l =
                                ("(" ++) . showSep l . (")" ++)
                            showSep [a] =
                                showsPrec 0 a
                            showSep (a : al) =
                                showsPrec 0 a.(", " ++).showSep al
                        in  (cn ++) . showParams el
   showsPrec p (FZ NOp e) =
      showsPrec 0 e
   showsPrec p (FZ qOp e) =
      showsPrec 0 qOp.(" (" ++).showsPrec 0 e.(")" ++)

instance Show ZOILDef where
   showsPrec p (DefZ vn e) =
      (vn ++).(" = " ++).showsPrec 0 e.("\n" ++)
   showsPrec p (DefCaseZ vn m es) =
      let showParams []    = id
          showParams l     = (" (" ++) . showSep l . (")" ++)
          showSep [a]      = showsPrec 0 a
          showSep (a : al) = showsPrec 0 a.(", " ++).showSep al
      in  (vn ++).(" = case " ++).showsPrec 0 m.
                  showParams es.("\n" ++)

instance Show ZOILProg where
   showsPrec p (ProgZ ds) =
      foldl (\f -> \d -> f . showsPrec p d) id ds



-- Syntactic sugar

numconst n   = ConF (show n) []
cond e e1 e2 = ConF "if" [e, e1, e2]
plus e1 e2   = ConF "+" [e1, e2]
minus e1 e2  = ConF "-" [e1, e2]
times e1 e2  = ConF "*" [e1, e2]
equal e1 e2  = ConF "==" [e1, e2]
nequal e1 e2 = ConF "/=" [e1, e2]
lt e1 e2     = ConF "<" [e1, e2]
gt e1 e2     = ConF ">" [e1, e2]
le e1 e2     = ConF "<=" [e1, e2]
ge e1 e2     = ConF ">=" [e1, e2]
nil          = ConF "[]" []
empty e      = ConF "null" [e]
cons e1 e2   = ConF ":" [e1, e2]
car e        = ConF "head" [e]
cdr e        = ConF "tail" [e]
makelist []    = nil
makelist (e:l) = cons e (makelist l)

-- ------|---------|---------|---------|---------|---------|---------|
