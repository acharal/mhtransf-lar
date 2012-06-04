module ZItoCl where 

import Types
import Syntax
import AuxFunGen (lkUpSure)
import AuxFunTrans (concQOp)
import Data.Char


-- ------|---------|-----        ZItoC       ----|---------|---------|

-- ------|---------|- declarations of functions -|---------|---------|

newCFunDec :: ZOILDef -> ShowS
newCFunDec (DefZ vn _) =
   ("value eval_" ++).(vn ++).("(World w);\n" ++)
newCFunDec (DefCaseZ vn _ _) =
   ("value eval_" ++).(vn ++).("(World w);\n" ++)

newCFunDecs :: ZOILProg -> ShowS
newCFunDecs (ProgZ defs) =
   foldl (.) id (map newCFunDec defs)


-- ------|---------|-- definitions of functions -|---------|---------|

--auxiliaries

retValue :: Order -> QOp -> ZOILExpr -> ShowS
retValue ord NOp (XZ vn) = 
   ("eval_" ++).(vn ++).("(w)"++)
retValue ord qOp (XZ vn) = 
   ("eval_" ++).(vn ++).("((World) {&w,{" ++).
      showsQLists qOp lists.("}})" ++)
      where
      lists                                =
         [("w.l[" ++).shows n.("]" ++) | n <- [0..(ord - 1)]]
      insertCommas [l]                     = [l]
      insertCommas (l:ls)                  = 
         (l:("," ++):(insertCommas ls))
      showsQLists NOp ls                   = 
         foldl1 (.) (insertCommas ls)
      showsQLists (Call (i,m):.:qOp) ls    = 
         showsQLists qOp 
            ((take m ls) ++ 
             (("call(" ++).shows (fst i).("," ++).(ls !! m).(")" ++)):
             (drop (m + 1) ls))
      showsQLists (Call' (i,m):.:qOp) ls    = 
         showsQLists qOp 
            ((take m ls) ++ 
             (("call(" ++).shows (fst i).("," ++).(ls !! m).(")" ++)):
             (drop (m + 1) ls))
      showsQLists (Actuals (i,m):.:qOp) ls =          
         showsQLists qOp 
            ((take m ls) ++ 
             (("actuals(" ++).shows (fst i).("," ++).(ls !! m).(")" ++)):
             (drop (m + 1) ls))
retValue ord qOp (ConZ cn es) = 
   case cn of
      "if" ->
            let test = retValue ord qOp (es !! 0)
            in  test.(" ? " ++).retValue ord qOp (es !! 1).(" : " ++).
                   retValue ord qOp (es !! 2)
      c | c == "-u" ->
            ("(-" ++).(" " ++).retValue ord qOp (es !! 0).(")" ++)
      c | c == "+" || c == "-" || c == "*" || c == "/" ||
          c == "&&" || c == "||" ->
            ("(" ++).retValue ord qOp (es !! 0).(" " ++).(cn ++).
               (" " ++).retValue ord qOp (es !! 1).(")" ++)
      c | c == "div" ->
            ("(" ++).retValue ord qOp (es !! 0).(" " ++).("/" ++).
               (" " ++).retValue ord qOp (es !! 1).(")" ++)
      c | c == "mod" ->
            ("(" ++).retValue ord qOp (es !! 0).(" " ++).("%" ++).
               (" " ++).retValue ord qOp (es !! 1).(")" ++)
      c | c == "True" ->
            ("true" ++)
      c | c == "False" ->
            ("false" ++)
      c | c == "==" || c == "<" || c == ">" || 
          c == "<=" || c == ">=" ->
            ("(" ++).retValue ord qOp (es !! 0).(" " ++).(cn ++).
               (" " ++).retValue ord qOp (es !! 1).(")" ++)
      "/=" ->
            ("(" ++).retValue ord qOp (es !! 0).(" " ++).("!=" ++).
               (" " ++).retValue ord qOp (es !! 1).(")" ++)
      otherwise ->
            case reads cn :: [(Int, String)] of
               [(n, "")] -> shows n
               otherwise -> error ("Unknown constant: " ++ cn)
retValue ord NOp (FZ qOp e) = 
   retValue ord qOp e
retValue ord qOp1 (FZ qOp2 e) = 
   retValue ord (concQOp qOp1 qOp2) e



isNum :: String -> Bool
isNum []      = True
isNum (c:str) = let ascii = ord c
                in if ascii >= 48 && ascii <= 57 then
                      case str of 
                         [] -> True
                         _  -> (isNum str)
                   else
                      False

isfresh str = 
   let aux1 b [] x = ([],x)
       aux1 b (c:str) res= if b == c then
                              (res, str)
                           else
                              aux1 b str (c:res)
       splitAtChar c str = aux1 c str []
       (s1,_) = splitAtChar '_' (reverse str)
   in case s1 of
         [] -> False
         _ -> if (isNum s1) then True else False
                              



newCFunDef :: Bool -> VarEnv -> Order -> [(VName,Order)] -> ZOILDef -> ShowS
newCFunDef trace env ord dimL (DefZ vn e)
   | elem vn ((fst.unzip) dimL) && (vn /= "result") =
      ("value eval_" ++).(vn ++).("(World w)\n" ++).
      ("{\n" ++).
      ("\tvalue val;\n" ++).
      ("\ttagWH hV;\n\n" ++).
      (if trace then
         ("\tprintf(\"BEGIN\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\"\\n\");\n\n" ++)
       else
         id).
      ("\tif (entriesHC >= HCGCLIMIT) gcollectHC(&w);\n" ++).
      ("\tif (seekWH(ID_" ++).(vn ++).("," ++).
         shows (lkUpSure vn dimL).(",&w,&hV)) {\n" ++).
      ("\t\tval = getWH(hV);\n\n" ++).
      (if trace then
         ("\t\tprintf(\"FOUND\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\t\tprintWorld(&w);\n" ++).
         ("\t\tprintf(\" = %d\\n\", val);\n\n" ++)
       else
         id).
      ("\t\treturn val;\n" ++).
      ("\t}\n\n" ++).
      ("\tval = " ++).retValue ord NOp e.(";\n" ++).
      ("\tputWH(hV,val);\n\n" ++).
      (if trace then
         ("\tprintf(\"END\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\" = %d\\n\", val);\n\n" ++)
       else
         id).
      ("\treturn val;\n\n" ++).
      ("}\n\n" ++)
   | otherwise   =
      ("value eval_" ++).(vn ++).("(World w)\n" ++).
      ("{\n" ++).
      ("\tvalue val;\n\n" ++).
      (if trace then
         ("\tprintf(\"BEGIN\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\"\\n\");\n\n" ++)
       else
         id).
      ("\tif (entriesHC >= HCGCLIMIT) gcollectHC(&w);\n" ++).
      ("\tval = " ++).retValue ord NOp e.(";\n\n" ++).
      (if trace then
         ("\tprintf(\"END\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\" = %d\\n\", val);\n\n" ++)
       else
         id).
      ("\treturn val;\n\n" ++).
      ("}\n\n" ++)

newCFunDef trace env ord dimL (DefCaseZ vn m es)
   | elem vn ((fst.unzip) dimL) && (vn /= "result") =
      ("value eval_" ++).(vn ++).("(World w)\n" ++).
      ("{\n" ++).
      ("\tvalue val;\n" ++).
      ("\ttagWH hV;\n\n" ++).
      (if trace then
         ("\tprintf(\"BEGIN\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\"\\n\");\n\n" ++)
       else
         id).
      (if m == 0  && (isfresh vn) then ("#ifdef STORE_HIGHER\n"++) else id).
      ("\tif (entriesHC >= HCGCLIMIT) gcollectHC(&w);\n" ++).
      ("\tif (seekWH(ID_" ++).(vn ++).("," ++).
         shows (lkUpSure vn dimL).(",&w,&hV)) {\n" ++).
      ("\t\tval = getWH(hV);\n\n" ++).
      (if trace then
         ("\t\tprintf(\"FOUND\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\t\tprintWorld(&w);\n" ++).
         ("\t\tprintf(\" = %d\\n\", val);\n\n" ++)
       else
         id).
      ("\t\treturn val;\n" ++).
      ("\t}\n\n" ++).
      (if m == 0 && (isfresh vn) then ("#endif\n"++) else id).
      ("\tswitch (cases(w.l[" ++).shows m.("])){\n" ++).
      showsCases (length es). 
      ("\tdefault:\n" ++).
      ("\t\tfprintf(stderr,\"Internal error:" ++).
         (" unpredicted case\\n\");\n" ++).
      ("\t\texit(-1);\n\n" ++).
      ("\t}\n\n" ++).
      (if m ==  0 && (isfresh vn) then ("#ifdef STORE_HIGHER\n"++) else id).
      ("\tputWH(hV,val);\n\n" ++).
      (if m == 0 && (isfresh vn)  then ("#endif\n"++) else id).
      (if trace then
         ("\tprintf(\"END\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\" = %d\\n\", val);\n\n" ++)
       else
         id).
      ("\treturn val;\n\n" ++).
      ("}\n\n" ++)
   | otherwise =
      ("value eval_" ++).(vn ++).("(World w)\n" ++).
      ("{\n" ++).
      ("\tvalue val;\n\n" ++).
      (if trace then
         ("\tprintf(\"BEGIN\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\"\\n\");\n\n" ++)
       else
         id).
      ("\tif (entriesHC >= HCGCLIMIT) gcollectHC(&w);\n" ++).
      ("\tswitch (cases(w.l[" ++).shows m.("])){\n" ++).
      showsCases (length es). 
      ("\tdefault:\n" ++).
      ("\t\tfprintf(stderr,\"Internal error:" ++).
         (" unpredicted case\\n\");\n" ++).
      ("\t\texit(-1);\n\n" ++).
      ("\t}\n\n" ++).
      (if trace then
         ("\tprintf(\"END\\t" ++).
         (vn ++).
         (" @ \");\n" ++).
         ("\tprintWorld(&w);\n" ++).
         ("\tprintf(\" = %d\\n\", val);\n\n" ++)
       else
         id).
      ("\treturn val;\n\n" ++).   
      ("}\n\n" ++)   
         where  
         showsCases 0         = id
         showsCases num       = 
            showsCases (num - 1).
            ("\tcase " ++).shows (num - 1).(":\n" ++).
            ("\t\tval = " ++).retValue ord NOp (es !! (num - 1)).
               (";\n" ++).
            ("\t\tbreak;\n" ++)


newCFunDefs :: Bool -> VarEnv -> Order -> [(VName,Order)] -> ZOILProg -> ShowS
newCFunDefs trace env ord dimL (ProgZ defs) =
   foldl (.) id (map (newCFunDef trace env ord dimL) defs)



-- ------|--------  constructing the main.c file  ---------|---------|

mainConstruct :: Bool -> VarEnv -> ZOILProg -> Order -> [(VName,Order)] -> 
                    Int -> Int -> Int -> Int -> ShowS
mainConstruct trace env prog ord dimL hcTSize hcHSize whTSize whHSize
   | ord /= 0  =
      ("/* Header files */\n\n" ++).
      ("#include <stdio.h>\n" ++).
      ("#include <stdlib.h>\n" ++).
      ("#include <time.h>\n\n" ++).
      ("#include \"hashcons.h\"\n" ++).
      ("#include \"warehouse.h\"\n\n\n" ++).
      ("/* Definitions of macros for identifiers stored" ++).
         (" in the warehouse */\n\n" ++).
      showsIdsDefs ((fst.unzip) dimL) 0.("\n\n" ++).
      ("/* Definition of the world */\n\n" ++).
      ("const dimension MAXDIM = " ++).shows ord.(";\n\n" ++).
      ("struct world_tag {\n" ++).
      ("\tstruct world_tag *prev;\n" ++).
      ("\tlist l[" ++).shows ord.("];\t\t" ++).
         ("/* equal to MAXDIM */\n" ++).
      ("};\n\n\n" ++).
      ("/* Implementation parameters */\n\n" ++).
      ("/* the hash table size */\n" ++).
      ("const tagHC HCTABLESIZE = 1 << " ++).shows hcTSize.(";\n" ++).
      ("/* the heap size for the hash consing table */\n" ++).
      ("const tagHC HCHEAPSIZE = 1 << " ++).shows hcHSize.(";\n" ++).
      ("/* the hash table size */\n" ++).
      ("const tagWH WHTABLESIZE = 1 << " ++).shows whTSize.(";\n" ++).
      ("/* the heap size for the warehouse */\n" ++).
      ("const tagWH WHHEAPSIZE = 1 << " ++).shows whHSize.(";\n" ++).
         ("\n\n" ++).
      ("/* Function prototypes */\n\n" ++).
      newCFunDecs prog.("\n\n" ++).
      ("/* Main program */\n\n" ++).
      ("int main()\n" ++).
      ("{\n" ++).
      ("\tclock_t t1,t2;\n" ++).
      ("\tvalue result;\n\n" ++).
      ("\tinitializeWH();\n" ++).
      ("\tinitializeHC();\n\n" ++).
      ("\tatexit(statsWH);\n" ++).
      ("\tatexit(statsHC);\n\n" ++).
      ("\tt1 = clock();\n" ++).
      ("\tresult = eval_result((World) {NULL,{" ++).
         showsInitLists ord.("}});\n" ++).
      ("\tt2 = clock();\n\n" ++).
      showsPrintRes.
      ("\tprintf(\"c time = %.3f sec\\n\"" ++).
         (",((double)(t2 - t1)/CLOCKS_PER_SEC));\n\n" ++).
      ("\treturn 0;\n" ++).
      ("\n}\n\n\n" ++).
      ("/* Function definitions */\n\n" ++).
      newCFunDefs trace env ord dimL prog 
   | otherwise = error "Zero-order input!"
         where
         showsIdsDefs [] _                = id
         showsIdsDefs (zOrdV:zOrdVs) indx 
            | zOrdV /= "result" = 
                ("#define ID_" ++).(zOrdV ++).(" ((identifier) " ++).
                   shows indx.(")\n" ++).
                showsIdsDefs zOrdVs (indx + 1)
            | otherwise =
	        showsIdsDefs zOrdVs (indx + 1)
         showsInitLists 1  = ("0" ++)
         showsInitLists i  = ("0," ++).showsInitLists (i - 1)
         showsPrintRes
            | rt == T [] I = 
               ("\tprintf(\"result = %d\\n\",result);\n" ++) 
            | rt == T [] B =
               ("\tprintf(\"result = \");\n" ++).
               ("\tres == 0\n" ++).("\t? printf(\"False\\n\")\n" ++).
               ("\t: printf(\"True\\n\");\n" ++) 
               where
               rt = lkUpSure "result" env
