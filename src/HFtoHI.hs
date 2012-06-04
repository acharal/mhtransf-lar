module HFtoHI where

import Types
import Syntax


-- ------|---------|-----  unique var names  ----|---------|---------|

uniqueNamesE :: VName -> [VName] -> HOFLExpr -> HOFLExpr
uniqueNamesE vn1 ps nvar@(XF vn2) 
   | elem vn2 ps = 
      XF (vn1 ++ "_" ++ vn2)
   | otherwise   = 
      nvar
uniqueNamesE vn ps (ConF cn exprs) =
   ConF cn (map (uniqueNamesE vn ps) exprs)
uniqueNamesE vn1 ps (FF vn2 exprs)
   | elem vn2 ps =
      FF (vn1 ++ "_" ++ vn2) (map (uniqueNamesE vn1 ps) exprs)
   | otherwise   =
      FF vn2 (map (uniqueNamesE vn1 ps) exprs)

uniqueNamesD :: HOFLDef -> HOFLDef
uniqueNamesD (DefF vn ps expr) = 
   DefF vn (map ((vn ++).("_" ++)) ps) (uniqueNamesE vn ps expr)

uniqueNames :: HOFLProg -> HOFLProg
uniqueNames (ProgF defs) = 
   ProgF (map uniqueNamesD defs)


-- ------|---------|-----   transformation   ----|---------|---------|

fromHFtoHIe :: HOFLExpr -> HOILExpr
fromHFtoHIe (XF vn) =
   XH vn
fromHFtoHIe (ConF cn exprs) =
   ConH cn (map fromHFtoHIe exprs)
fromHFtoHIe (FF vn exprs) =
   FH NOp vn (map fromHFtoHIe exprs)

fromHFtoHId :: HOFLDef -> HOILDef
fromHFtoHId (DefF vn ps expr) = 
   DefH vn ps (fromHFtoHIe expr)

fromHFtoHI :: HOFLProg -> HOILProg
fromHFtoHI prog = 
   let (ProgF defs) = uniqueNames prog
   in  ProgH (map fromHFtoHId defs)

-- ------|---------|---------|---------|---------|---------|---------|
