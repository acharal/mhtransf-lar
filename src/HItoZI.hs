module HItoZI where

import Types
import Syntax


-- ------|---------|-----   transformation  ----|---------|---------|

fromHItoZIe :: HOILExpr -> ZOILExpr
fromHItoZIe (XH vn) = XZ vn
fromHItoZIe (ConH cn exprs) = ConZ cn (map fromHItoZIe exprs)
fromHItoZIe (FH NOp vn []) = XZ vn
fromHItoZIe (FH qOp vn emptyListOrConst)
   | vn == "const" = 
        FZ qOp (fromHItoZIe (takeSingleElem emptyListOrConst))
   | otherwise     = FZ qOp (XZ vn)
     where 
     takeSingleElem [elem] = elem

fromHItoZId :: HOILDef -> ZOILDef
fromHItoZId (DefH vn _ expr) = DefZ vn (fromHItoZIe expr)
fromHItoZId (DefCaseH vn _ mInd exprs) =
   DefCaseZ vn mInd (map fromHItoZIe exprs)

fromHItoZI :: HOILProg -> ZOILProg
fromHItoZI (ProgH defs) = ProgZ (map fromHItoZId defs)

-- ------|---------|---------|---------|---------|---------|---------|