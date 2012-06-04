module AuxFunTrans where

import Data.List
import Data.Maybe
import Types
import Syntax
import AuxFunGen
import AuxFunEnv


-- ------|---------   transformation auxiliaries   --------|---------|


-- ------|---------|-----     environment    ----|---------|---------|

--adjusting environment

addNewTEnv :: VarEnv -> HOILProg -> Order -> VarEnv
addNewTEnv env prog ordE =
   env ++ 
   (concat.concat) [[[(np,nt) 
		     | x@(np,nt) <- formAssoc env f] 
		    | f <- (snd.unzip)(high env prog var ordE)] 
		   | var <- (fst.unzip) env, orderV env var == ordE]
      
adjustEnv :: VarEnv -> HOILProg -> Order -> VarEnv
adjustEnv env prog ordE =
   let test = ((==ordE).order.snd)
   in  mapIf test (remvHighs ordE) (addNewTEnv env prog ordE)
      where 
      mapIf _ _ [] 		= []
      mapIf test f (e:es)	
	 | test e		= (f e):mapIf test f es
	 | otherwise		= e:mapIf test f es
      remvHighs ordE tup@(vn,T ps g) =
         (vn,T (filter ((< (ordE - 1)).order) ps) g)


-- ------|---------|- manipulating Q operators --|---------|---------|

--inverse of Q Operator

fromQOptoList :: QOp -> [(Char,(IIndex,MIndex))] 
fromQOptoList NOp = []
fromQOptoList (Call (i,m) :.: restQOp) = 
   ('c',(i,m)):(fromQOptoList restQOp)
fromQOptoList (Call' (i,m) :.: restQOp) = 
   ('q',(i,m)):(fromQOptoList restQOp)
fromQOptoList (Actuals (i,m) :.: restQOp) = 
   ('a',(i,m)):(fromQOptoList restQOp)

fromListtoQOpR :: [(Char,(IIndex,MIndex))] ->QOp
fromListtoQOpR [] = NOp
fromListtoQOpR (('c',(i,m)):restList) = 
   Actuals (i,m) :.: (fromListtoQOpR restList)
fromListtoQOpR (('q',(i,m)):restList) = 
   Actuals (i,m) :.: (fromListtoQOpR restList)
fromListtoQOpR (('a',(i,m)):restList) = 
   Call' (i,m) :.: (fromListtoQOpR restList)


invQOp :: QOp -> QOp
invQOp = fromListtoQOpR.reverse.fromQOptoList


--concatenation of Q Operators

fromListtoQOp :: [(Char,(IIndex,MIndex))] ->QOp
fromListtoQOp [] = NOp
fromListtoQOp (('a',(i,m)):restList) = 
   Actuals (i,m) :.: (fromListtoQOp restList)
fromListtoQOp (('c',(i,m)):restList) = 
   Call (i,m) :.: (fromListtoQOp restList)
fromListtoQOp (('q',(i,m)):restList) = 
   Call' (i,m) :.: (fromListtoQOp restList)


concQOp :: QOp -> QOp -> QOp
concQOp q1 q2 = 
   fromListtoQOp ((fromQOptoList q1) ++ (fromQOptoList q2))


-- ------|---------|-----     functions      ----|---------|---------|

--calls function

callsE :: [(VName,[HOILExpr])] -> HOILExpr -> [(VName,[HOILExpr])]     
callsE ascs (XH _) = ascs
callsE ascs (ConH _ es)	= concatMap (callsE []) es `conc` ascs
callsE ascs fcall@(FH _ vn es) = 
   concatMap (callsE []) es `conc` ([(vn,[fcall])] `conc` ascs)

callsD :: HOILDef -> [(VName,[HOILExpr])]
callsD (DefH _ _ expr) = callsE [] expr
callsD (DefCaseH _ _ _ exprs) = concatMap (callsE []) exprs

callsP :: HOILProg -> [(VName,[HOILExpr])]
callsP (ProgH defs) = foldl conc [] (reverse (map callsD defs))

calls :: HOILProg -> VName -> [HOILExpr] 
calls prog vname = lkUp vname (callsP prog)


--label function

label :: HOILProg -> VName -> [(Label,HOILExpr)] 
label prog vname = let callList = nub (calls prog vname)
		   in  zip (zip [0..(length callList - 1)] (repeat vname)) callList


--low function

-- ------|---------|-----     auxiliary      ----|---------|---------|

--search definition

searchDef :: VName -> HOILProg -> Maybe HOILDef
searchDef _  (ProgH []) = Nothing
searchDef vn1 (ProgH (def@(DefH vn2 _ _):restdefs)) 
   | vn1 == vn2		= Just def
   | otherwise 		= searchDef vn1 (ProgH restdefs)
searchDef vn1  (ProgH (_:restdefs)) = 
   searchDef vn1 (ProgH restdefs)


searchDefCase :: VName -> HOILProg -> Maybe HOILDef
searchDefCase _  (ProgH []) = Nothing
searchDefCase vn1 (ProgH (def@(DefCaseH vn2 _ _ _):restdefs)) 
   | vn1 == vn2		= Just def
   | otherwise 		= searchDefCase vn1 (ProgH restdefs)
searchDefCase vn1  (ProgH (_:restdefs)) = 
   searchDefCase vn1 (ProgH restdefs)


searchD :: VName -> HOILProg -> Maybe HOILDef
searchD vn prog = 
   if (sDef == Nothing) then 
      (if (sDefCase == Nothing) then Nothing
        else sDefCase)
    else sDef
      where
      sDef	= searchDef vn prog
      sDefCase	= searchDefCase vn prog


-- ------|---------|----- end of auxilliary  ----|---------|---------|

lowAssoc :: VarEnv -> HOILProg -> VName -> Order -> [(Int,VName)]
lowAssoc env prog vname ordE =
   let Just def				= searchD vname prog
       takeForms (DefH _ fms _) 	= fms
       takeForms (DefCaseH _ fms _ _)	= fms
       formals 	 			= takeForms def
       inds				= [0..((length formals) - 1)]
       posForms				= zip inds formals
       ordV				= orderV env vname
       helpTest				= ((ordE - 1) >)
       test   				= helpTest.(orderV env).snd
   in  if (ordV == ordE) then filter test posForms
        else posForms			
		
low :: VarEnv -> HOILProg -> VName -> Order -> [Int]
low env prog vname ordE = (fst.unzip)(lowAssoc env prog vname ordE)


--high function

high :: VarEnv -> HOILProg -> VName -> Order -> [(Int,VName)]
high env prog vname ordE = 
 case searchD vname prog of
  Nothing -> error ("Could not find: (" ++ vname ++ ") in:\n" ++ show prog)
  otherwise ->
   let Just def				= searchD vname prog
       takeForms (DefH _ fms _) 	= fms
       takeForms (DefCaseH _ fms _ _)	= fms
       formals  			= takeForms def
       inds			   	= [0..((length formals) - 1)]
       posForms				= zip inds formals
       ordV				= orderV env vname
       helpTest				= ((ordE - 1) ==)
       test 				= helpTest.(orderV env).snd
   in  if (ordV == ordE) then filter test posForms
	else []

-- ------|---------|---------|---------|---------|---------|---------|
