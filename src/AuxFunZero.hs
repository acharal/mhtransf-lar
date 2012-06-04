module AuxFunZero where

import Types
import Syntax


-- ------|---------|---- auxiliaries for ZOIL ---|---------|---------|

--search definition

searchDefZ :: VName -> ZOILProg -> Maybe ZOILDef
searchDefZ _  (ProgZ []) = Nothing
searchDefZ vn1 (ProgZ (def@(DefZ vn2 _):restdefs)) 
   | vn1 == vn2		= Just def
   | otherwise 		= searchDefZ vn1 (ProgZ restdefs)
searchDefZ vn1  (ProgZ (_:restdefs)) = 
   searchDefZ vn1 (ProgZ restdefs)


searchDefCaseZ :: VName -> ZOILProg -> Maybe ZOILDef
searchDefCaseZ _  (ProgZ []) = Nothing
searchDefCaseZ vn1 (ProgZ (def@(DefCaseZ vn2 _ _):restdefs)) 
   | vn1 == vn2		= Just def
   | otherwise 		= searchDefCaseZ vn1 (ProgZ restdefs)
searchDefCaseZ vn1  (ProgZ (_:restdefs)) = 
   searchDefCaseZ vn1 (ProgZ restdefs)


searchZD :: VName -> ZOILProg -> Maybe ZOILDef
searchZD vn prog = 
   if (sDef == Nothing) then 
      if (sDefCase == Nothing) then Nothing
       else sDefCase
    else sDef
      where
      sDef	= searchDefZ vn prog
      sDefCase	= searchDefCaseZ vn prog


-- ------|---------|---------|---------|---------|---------|---------|