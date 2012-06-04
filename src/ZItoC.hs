{-*****************************************************************************
 *  CVS version:
 *    $Id: ZItoC.hs,v 1.5 2004/09/20 11:57:10 nickie Exp $
 ******************************************************************************
 *
 *  Haskell file : ZItoC.hs
 *  Project      : HOFL to ZOIL to C compiler
 *  Version      : 1.0 alpha
 *  Description  : ZOIL to C translation
-}


module ZItoC where

import Syntax
import Types
import qualified ZItoCl

 
-- ------|---------|-----  making the files  ----|---------|---------|
   
makeC :: Order -> Bool -> VarEnv -> ZOILProg -> [(VName,Order)] ->
	 Int -> Int -> Int -> Int -> Bool -> String
makeC ord trace env prog dimL hcTSize hcHSize whTSize whHSize global =
   let mainConstruct            =
	   if global then {-ZItoCg.mainConstruct-}
              error "no global now"
            else ZItoCl.mainConstruct 
       cprog                    =
          mainConstruct trace env prog ord dimL
             hcTSize hcHSize whTSize whHSize ""
   in cprog

-- ------|---------|---------|---------|---------|---------|---------|
