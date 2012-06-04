module LAR where

import Types
import Syntax
import AuxFunEnv

type Dim = Int

data LARProg  = LARProg [LARBlock]

data LARBlock = Func VName LARStm [(VName, Dim)]
              | Var VName VName LARStm [(VName, Dim)]
              deriving Show

data LARStm   = LARStm [LAROp] LARExpr deriving Show

data LAROp    = LARAct Dim [(VName)] | LARForw Dim [(VName)] deriving Show

data LARExpr  = LARCall VName [(VName, Dim)]
              | LARC VName [LARExpr]
              deriving Show


instance Show LARProg where
     showsPrec p (LARProg bls) = foldl (.) id (map (\x -> (showsPrec 0 x).("\n"++) ) bls)


isPrefix [] _ = True
isPrefix (x:xs) (y:ys) = if x == y then (isPrefix xs ys) else False
isPrefix _ _ = False

unq [] = []
unq (x:xs) = if x `elem` xs then (unq xs) else x:(unq xs)



fromZOILtoLAR :: ZOILProg -> VarEnv -> [(VName, [(VName,Type)])]-> LARProg
fromZOILtoLAR (ProgZ ds) env env'
    = let formals2 :: VName -> [(VName, Dim)]
          formals  :: VName -> [VName]
          formalsOfDim :: VName -> Dim -> [VName]
--          formals2 x = [ (y, (order t)) | (y, t) <- env, (isPrefix (x++"_") y ) ] {-??-}
          formals2 x = case (filter ((==x).fst) env') of
                                     [] -> []
                                     (_,ls):[] -> (map (\(y,t) -> (y, order t)) ls)
          formals x = map fst (formals2 x)
          formalsOfDim x d = map fst (filter ((==d).snd) (formals2 x))
          
          mkBlocks :: ZOILDef -> [LARBlock]
          mkBlocks (DefZ x exp) = [Func x (mkStm exp) (formals2 x)]
          mkBlocks (DefCaseZ n idx exps) = (map (\(i,e)->(Var (n++"__"++(show i)) n (mkStm e) (formals2 n)) ) (zip [0..] exps))
          
          mkStm :: ZOILExpr -> LARStm
          mkStm (FZ q exp) = let ops = (mkOp.rmCalls) q
                                 calls = (getCalls q)
                             in case calls of
                                   NOp -> LARStm ops (mkExp exp)
                                   _ -> LARStm ops (mkExp (FZ calls exp))

          mkStm x = LARStm [] (mkExp x)

          mkExp :: ZOILExpr -> LARExpr
          mkExp (XZ n)         = LARCall n []
          mkExp (ConZ c exprs) = LARC c (map mkExp exprs)
          mkExp (FZ q exp)     = let calls = (getCallList q)
                                     args z = [ (x ++"__"++(show i), m) | (i,m) <- calls , (x,n) <- (formals2 z), m == n]
                                 in case exp of 
                                      XZ n -> LARCall n (args n)
                                      otherwise -> undefined

          mkOp :: QOp -> [LAROp]
          mkOp NOp = []
          mkOp (Call' ((i,v),m):.: qs) = (LARForw m j):(mkOp qs)
                                     where j = [ x ++"__"++(show i) | (x,n) <- (formals2 v), m == n]
          mkOp (Actuals (_,m) :.: qs) = (LARAct m []):(mkOp qs)
          mkOp _ = undefined

          rmCalls :: QOp -> QOp
          rmCalls (q :.: qs) = case q of 
                         Actuals (i,m) -> q:.:(rmCalls qs)
                         Call' (i,m)   -> q:.:(rmCalls qs)
                         Call (i,m) -> NOp
          rmCalls NOp = NOp

          getCalls :: QOp -> QOp
          getCalls qs@(Call (i,m) :.: q) = qs
          getCalls (_:.:q) = (getCalls q)
          getCalls _ = NOp
          
          getCallList ((Call ((i,_),m)) :.: qs) = (i,m):(getCallList qs)
          getCallList NOp = []
          getCallList x = undefined

      in LARProg (foldl (++) [] (map mkBlocks ds))



makeC :: LARProg -> Order ->ShowS
makeC (LARProg ds) ord = let definitions = let proto (Func n stm bind) = ("FUNC("++).(n++).(");\n"++)
                                               proto (Var  n _ stm bind) = ("VAR("++).(n++).(");\n"++)
                                           in foldl (.) id (map (\x -> (proto x) ) ds)
                                           
                             argdefs  = let    proto (Func n stm bind) = foldl (.) id (map (\(n,x)->proto2 n x) (labeling bind))
                                               proto (Var  n _ stm bind) = foldl (.) id (map (\(n,x)->proto3 n x) (labeling bind))
                                               proto2 n (x,d) = ("#define "++).(x++).(if d == 0 then  ("("++).world.(") GETARG("++).(shows n).(","++).(shows d).(")\n"++)
                                                                                      else            (" ARG("++).(shows n).(","++).(shows d).(")\n"++))
                                               proto3 n (x,d) = ("#define "++).(x++).(if d == 0 then  ("("++).world.(") CALLARG("++).(shows n).(","++).(shows d).(")\n"++)
                                                                                      else            (" ARG("++).(shows n).(","++).(shows d).(")\n"++))
                                           in foldl (.) id (map (\x -> (proto x) ) ds)
                                           where labeling [] = []
                                                 labeling x = let sublist d = filter ((==d).snd) x
                                                              in foldl (++) [] [ zip [0..] x | x <- [ sublist d | d <- [0..maxdim] ] ]
                                                              where maxdim = maximum (map snd x)
                                                 
                                                 world = (foldl (.) id (insComm [ ("T"++).(shows n) | n <- [0..(ord-1)] ]))
                             maxdim = ord - 1
                             mainprog = foldl (.) id (map (\x -> (mkCBlock x).("\n"++) ) ds)
                             epilogue = id
                             prologue = ("#include <stdio.h>\n#include <stdlib.h>\n#include <time.h>\n"++).
                                        ("#define True 1\n#define False 0\n"++).
                                        ("#define VAR(x)        FUNC(x)\n"++).
                                        ("#define ARG(x,n)      T ## n->args[x]\n"++).
                                        ("#define VAL(x,n)      T ## n->vals[x]\n"++).
                                        ("#define SETVARS2(m,n) LarVal vals[m]; T ## n->vals = vals\n"++).
                                        ("#define SETVARS(m)    SETVARS2(m,0)\n"++).
                                        ("#define GETARG(n,x)   ({ if(ARG(n,x)){ VAL(n,x)=CALLARG(n,x); ARG(n,x)=0; }; VAL(n,x); })\n"++).
                                        ("#define CALLARG(n,x)  CALL(ARG(n,x))\n"++).
                                        ("#define CTX(n)        T ## n\n"++).
                                        ("#define AR(n, ...)    &(T_){ CTX(n), (LarArg[]){ __VA_ARGS__ }, 0 }\n"++).
                                        ("#define CALL2(d, a...)  CTX(d) = AR(d, a)\n"++).
                                        ("#define ACTUAL(n)     T ## n = T ## n->prev\n"++).
                                        ("#define FUNC(x)       LarVal x("++).(foldl (.) id (insComm [ ("TP_ T"++).(shows n) | n <- [0..(ord-1)] ])).(")\n"++).
                                        ("#define CALL(x)       x("++).(foldl (.) id (insComm [ ("T"++).(shows n) | n <- [0..(ord-1)] ])).(")\n"++).
                                        ("\n\ntypedef struct T_* TP_;\n"++).
                                        ("typedef int LarVal;\n"++).
                                        ("typedef LarVal (*LarArg)("++).(foldl (.) id (insComm [ ("TP_"++) | n <- [0..(ord-1)] ])).(");\n"++).
                                        ("typedef struct T_ {\n\t TP_ prev;\n\t LarArg* args;\n\t LarVal* vals;\n}T_;\n\n\n"++).
                                        ("\n #define SAVEHIGHER\n"++)

                             mainfunc = ("int main(int argc, char* argv[]){\n"++).
                                        ("clock_t t1, t2;\nregister LarVal res;\n"++).
                                        ("t1 = clock();\n"++).
                                        ("res = result("++).(foldl (.) id emptyARgs.)(");\n"++).
                                        ("t2 = clock();\n printf(\"%d\\n\", res);\nprintf(\"c time = %.10f sec\\n\", ((double)(t2 - t1)/CLOCKS_PER_SEC));\nreturn 0;\n}\n\n\n"++)

                             emptyARgs = insComm [ ("0"++) | d <- [0..maxdim] ]
                         in prologue.definitions.argdefs.mainfunc.mainprog.epilogue
                         where 
                               mkCBlock :: LARBlock -> ShowS
                               mkCBlock x@(Func n stm bind) = ("FUNC("++).(n++).("){\n"++).(mkblkprolog x).(mkCStm stm bind).("}\n"++)
                               mkCBlock x@(Var  n x' stm bind) = ("VAR("++).(n++).("){\n "++).
                                                                 (if b then  ("#ifdef SAVEHIGHER\n LarArg* Saveto = &"++).(x'++).(";\n#endif\n"++)
                                                                  else (""++)
                                                                 ).(mkblkprolog x).(mkAStm stm bind b).("}\n"++)
                                                                 where b = case bind of 
                                                                               _:_ -> True
                                                                               _   -> False
                                                                        
                                            
                               mkblkprolog (Func n stm bind) = let count = length (filter ((0==).snd) bind)
                                                               in if count > 0 then 
                                                                      ("SETVARS("++).(shows count).(");\n"++)
                                                                  else
                                                                      (""++)
                               mkblkprolog _ = (""++)

                               mkOps :: [LAROp] -> ShowS
                               mkOps x = foldl (.) id (map mkOp x)
                                       where --mkOp (LARForw m i) = ("CTX("++).(shows m).(") = AR("++).(shows m).(", FORWARD"++).(shows m).(shows i).(");\n"++)
                                             mkOp (LARForw m i) = ("CALL2("++).(shows m).(","++).(foldl (.) id (insComm (map (++) i))).(");\n"++)
                                             mkOp (LARAct m _) = ("ACTUAL("++).(shows m).(");\n"++)

                               mkCStm :: LARStm -> [(VName, Dim)] -> ShowS
                               mkCStm (LARStm ops exp) bind = (mkOps ops).
                                                              ("return "++).(mkCExp exp bind).(";\n"++)


                               mkAStm (LARStm ops exp) bind  b =  (mkOps ops).
                                                                  ("LarVal res = "++).(mkCExp exp bind).(";\n"++).
                                                                  (if b then
                                                                        case exp of
                                                                             LARCall n _ -> ("#ifdef SAVEHIGHER\n*Saveto = "++).(n++).(";\n#endif\n"++)
                                                                             _ -> (""++)
                                                                   else (""++)).
                                                                  ("return res;\n"++)

                               mkForws :: (Dim -> ShowS) -> [(VName,Dim)] ->ShowS
                               mkForws f ((_,0):xs) = mkForws f xs
                               mkForws f ((_,d):xs) = (f d).(mkForws f xs)
                               mkForws f _ = (""++)
                           
                               mkCExp :: LARExpr -> [(VName, Dim)] -> ShowS
                               mkCExp (LARC c exps) args = case c of
                                                    "if" -> ("("++).(mkCExp (exps !! 0) args).(")?"++).
                                                                  ("("++).(mkCExp (exps !! 1) args).("):"++).
                                                                  ("("++).(mkCExp (exps !! 2) args).(")"++)
                                                    "-"   -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "+"   -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "*"   -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "/"   -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "=="  -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "<="  -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    ">"   -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "<"   -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "=>"  -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "&&"  -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "||"  -> ("("++).(mkCExp (exps !! 0) args).(c++).(mkCExp (exps !! 1) args).(")"++)
                                                    "/="  -> ("("++).(mkCExp (exps !! 0) args).("!="++).(mkCExp (exps !! 1) args).(")"++)
                                                    "mod"  -> ("("++).(mkCExp (exps !! 0) args).("%"++).(mkCExp (exps !! 1) args).(")"++)
                                                    "div"  -> ("("++).(mkCExp (exps !! 0) args).("/"++).(mkCExp (exps !! 1) args).(")"++)
                                                    "-u"  -> ("(-"++).(mkCExp (exps !! 0) args).(")"++)
                                                    "+u"  -> (mkCExp (exps !! 0) args)
                                                    otherwise -> (c++)

                               mkCExp (LARCall n acts) args = (n++).("("++).(mkActs acts).(")"++)
                           
                               mkActs :: [(VName, Dim)] -> ShowS
                               mkActs args = foldl (.) id (insComm (map (mkDim args) [0..(ord-1)]))
                                           where mkDim :: [(VName, Dim)] -> Dim -> ShowS
                                                 mkDim args d = let vars = map fst (filter ((==d).snd) args)
                                                                in  case vars of 
                                                                         [] -> ("CTX("++).(shows d).(")"++)
                                                                         _  -> ("AR("++).(shows d).(","++).(foldl (.) id (insComm (map (++) vars))).(")"++)
--                                                                         _  -> ("AR("++).(foldl (.) id (reverse (map (++) vars))).(")"++)
                               insComm :: [ShowS] -> [ShowS]
                               insComm [x] = [x]
                               insComm (x:xs)  = (x.(","++)):(insComm xs)
