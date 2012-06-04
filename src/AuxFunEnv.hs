module AuxFunEnv where

import Types
import AuxFunGen

-- ------|---------|-----    environment     ----|---------|---------|

-- ------|---------|-----   order of types   ----|---------|---------|

order :: Type -> Order
order (T [] g) = 0
order (T ts g) = 1 + maximum (map order ts)

orderV :: VarEnv -> VName -> Order
orderV env vname = order (lkUpSure vname env)

orderE :: VarEnv -> Order
orderE env = let tps 	= snd (unzip env)
		 ords 	= map order tps
	     in  maximum ords



-- ------|---------|-----   form function    ----|---------|---------|

formAssocRaw :: VName -> Type -> [(VName,Type)]
formAssocRaw vname t@(T tparams _) =
   let labeling    = (vname ++).("_" ++).show
       numofparams = length tparams
       newnames  	 = map labeling [0..(numofparams - 1)]
   in  zip newnames tparams

formAssoc :: VarEnv -> VName -> [(VName,Type)]
formAssoc env vname = formAssocRaw vname (lkUpSure vname env)


form :: VarEnv -> VName -> [VName]
form env vname =
   let tp	 		 = lkUpSure vname env
       takeParams (T paraml _)   = paraml
       numParams 		 = (length.takeParams) tp
   in  map ((vname ++).("_" ++).show) [0..(numParams - 1)]


-- ------|---------|-----   pretty printer   ----|---------|---------|

showEnv :: VarEnv -> String
showEnv env =
   let aux [] = id
       aux ((vname, tau) : l) =
          (vname ++) . (" :: " ++) . showsPrec 0 tau . ("\n" ++) .
          aux l
   in  aux env ""

-- ------|---------|---------|---------|---------|---------|---------|
