module HItoHI where

import Data.List
import Types
import Syntax
import AuxFunGen
import AuxFunEnv
import AuxFunTrans


-- ------|--------- pre-transformation auxiliaries --------|---------|

--exProc function: processing expressions

exProc :: VarEnv -> HOILProg -> Order -> HOILExpr -> HOILExpr   
exProc _ _ _ nvar@(XH vname) = nvar
exProc env prog ordE (ConH cname es) = 
   ConH cname (map (exProc env prog ordE) es)
exProc env prog ordE fun@(FH qOp vname es)
   | ordV == ordE = 
      FH (qOp `concQOp` (Call (lab,(ordE - 1)) :.: NOp)) vname hes
   | otherwise    = 
      FH qOp vname (map (exProc env prog ordE) es)
         where
         ordV = orderV env vname
         labs = label prog vname
         lab  = lkUpSureS fun labs
         lows = low env prog vname ordE
         hes  = [exProc env prog ordE (es !! l) | l <- lows]
         

--elimHigh function: eliminating the highest-order formals

-- ------|---------|-----      auxiliary     ----|---------|---------|

elimHighD :: VarEnv -> HOILProg -> Order -> HOILDef -> HOILDef
elimHighD env prog ordE (DefH vname params expr) =
   DefH vname newParams (exProc env prog ordE expr)
      where
      lows      = low env prog vname ordE
      newParams = [params !! l | l <- lows]

elimHighD env prog ordE (DefCaseH vname params mInd exprs) =
   DefCaseH vname newParams mInd (map (exProc env prog ordE) exprs)
      where
      lows      = low env prog vname ordE
      newParams = [params !! l | l <- lows]

elimHighHelp :: VarEnv -> HOILProg -> Order -> HOILProg -> HOILProg
elimHighHelp env prog ordE (ProgH defs) = 
   ProgH (map (elimHighD env prog ordE) defs)


-- ------|---------|-----  end of auxiliary  ----|---------|---------|

elimHigh :: VarEnv -> HOILProg -> Order -> HOILProg 
elimHigh env prog ordE = elimHighHelp env prog ordE prog



--inv function

inv ::  VarEnv -> HOILProg -> VName -> VName -> Order -> 
        (Label,HOILExpr) -> (QOp,HOILExpr)  
inv env prog f x ordE lclf =
   let Just def                  = searchD f prog
       tkPs (DefH _ fms _)       = fms
       tkPs (DefCaseH _ fms _ _) = fms
       ps                        = tkPs def
       Just jindx                = findIndex (x ==) ps
       iindx                     = fst lclf
       iexpr                     = snd lclf
       takeExprs (FH _ _ exps)   = exps
       takeQOp (FH qOper _ _)    = qOper
       jexpr                     = (takeExprs iexpr) !! jindx
       jprocexpr                 = exProc env prog ordE jexpr
       actuals                   = Actuals (iindx,(ordE - 1))
       inverqop                  = invQOp (takeQOp iexpr)
       qOpH                      = actuals :.: inverqop
   in  (qOpH,jprocexpr)



--params function

params :: VarEnv -> HOILProg -> VName -> VName -> Order ->
          [(QOp,HOILExpr)]
params env prog f x ordE = 
   let lclfs = label prog f
       invsH = inv env prog f x ordE
   in  map invsH lclfs

--createNew function

-- ------|---------|-----     auxiliary      ----|---------|---------|

createNewF :: VarEnv -> HOILProg -> Order -> VName -> [HOILDef]
createNewF env prog ordE vn =
   let ordV                     = orderV env vn
       ordHighs                 = high env prog vn ordE 
       highs                    = (snd.unzip) ordHighs
       takeVName (XH vn)        = vn
       takeVName (ConH _ _)     = "const"
       takeVName (FH _ vn _)    = vn
       takeQOp (FH qOp _ _)     = qOp
       takeQOp (ConH _ _)       = NOp
       takeQOp (XH _)           = NOp
       isConH (XH _)            = False
       isConH (ConH _ _)        = True
       isConH (FH _ _ _)        = False
   in  if (ordV == ordE) then
         [DefCaseH x (form env x) (ordE - 1) 
          [FH (concQOp qOp (takeQOp expr)) (takeVName expr) 
           ([XH f | f <- form env x] ++ [expr | isConH expr])
          | tup@(qOp, expr) <- params env prog vn x ordE]
         | x <- highs]
       else
         []

createNewD :: VarEnv -> HOILProg -> Order -> HOILDef -> [HOILDef]
createNewD env prog ordE (DefH vn _ _) =
   createNewF env prog ordE vn
createNewD env prog ordE (DefCaseH vn _ _ _) =
   createNewF env prog ordE vn

createNewHelp ::  VarEnv -> HOILProg -> Order -> HOILProg -> [HOILDef]
createNewHelp env prog ordE (ProgH defs) =
   concatMap (createNewD env prog ordE) defs

-- ------|---------|-----  end of auxiliary  ----|---------|---------|

createNew :: VarEnv -> HOILProg -> Order -> [HOILDef]
createNew env prog ordE = nub (createNewHelp env prog ordE prog)


-- ------|----- end of pre-transformation auxiliaries  ----|---------|


-- ------|---------|-----   transformation   ----|---------|---------|

-- looking for the dimensionality dependency of a variable
mkDimL :: VarEnv -> [HOILDef] -> [(VName,Order)]
mkDimL env defs = 
   let processD (DefH vname params _) =
          processF vname params
       processD (DefCaseH vname params _ _) =
          processF vname params
       processF vname params =
          let t@(T tparams _) = lkUpSure vname env
              ordF            = order t
          in  if ordF == 0 then
                 [(vname, 0)]
              else
                 concatMap (processP ordF) (zip params tparams)
       processP ordF (vname, t) =
          if order t == 0 then
             [(vname, ordF)]
          else
             concatMap (processP ordF) (formAssocRaw vname t)
   in  concatMap processD defs
              

step :: VarEnv -> HOILProg -> Order -> (HOILProg,VarEnv)
step env prog@(ProgH defs) ordE =
   let newdefs          = createNew env prog ordE
       ProgH procdefs   = elimHigh env prog ordE
       newenv           = adjustEnv env prog ordE
   in  ((ProgH (procdefs ++ newdefs)),newenv)


steps :: VarEnv -> HOILProg -> Order -> [(HOILProg,VarEnv)]
steps env prog ordE
   | ordE == 0  = []
   | otherwise  = stp:(steps newenv newprog neword)
      where
      stp       = step env prog ordE 
      newprog   = fst stp
      newenv    = snd stp
      neword    = ordE - 1


transformHItoHI :: VarEnv -> HOILProg -> [HOILProg]
transformHItoHI env prog@(ProgH defs) =
   let ordE        = orderE env
   in  if (ordE == 0) then [prog]
        else map fst (steps env prog ordE)


fromHItoHI :: VarEnv -> HOILProg -> (HOILProg,VarEnv,[(VName,Int)])
fromHItoHI env prog@(ProgH defs) =
   let ordE         = orderE env
       (prog',env') = last (steps env prog ordE)
       dimL         = mkDimL env defs
   in  if ordE == 0 then 
         (prog,env,dimL)
       else
         (prog',env',dimL)

-- ------|---------|---------|---------|---------|---------|---------|
