module Types where


-- ------|---------|-----   auxiliary types  ----|---------|---------|

type VName = String
type CName = String

type MIndex = Int
--type IIndex = Int
type IIndex = (Int, VName)

type Order = Int
--type Label = Int
type Label = IIndex
--type Tag = Int
type Tag = IIndex

type Flag = Bool

-- ------|---------|-----    typechecking    ----|---------|---------|

-- ------|---------|-----       ground       ----|---------|---------|

data Ground = I | B | L Ground
	deriving (Eq,Read)

-- pretty printing

instance Show Ground where
   showsPrec p I =
      ("Int" ++) 
   showsPrec p B =
      ("Bool" ++)
   showsPrec p (L ground) =
      ("[" ++).showsPrec 0 ground.("]" ++)


-- ------|---------|-----          type      ----|---------|---------|

data Type = T [Type] Ground
	deriving (Eq,Read)

-- pretty printing

instance Show Type where
   showsPrec p (T [] ground)  = showsPrec 0 ground
   showsPrec p (T ptl ground) =
      let showParTypes []  = id
          showParTypes [a] = showsPrec 0 a
          showParTypes l   = ("(" ++).showSep l.(")" ++)
          showSep [a]      = showsPrec 0 a
          showSep (a : al) = showsPrec 0 a.(", " ++).showSep al
      in  showParTypes ptl.(" -> " ++).showsPrec 0 ground


-- ------|---------|-----   auxiliary types  ----|---------|---------|

type VarEnv = [(VName,Type)]

data Value = VI Int | VB Bool | VL [Value]
   deriving (Eq,Read)

-- pretty printing

instance Show Value where
   showsPrec p (VI int)  = showsPrec 0 int
   showsPrec p (VB bool) = showsPrec 0 bool
   showsPrec p (VL l)    = showsPrec 0 l

-- ------|---------|---------|---------|---------|---------|---------|
