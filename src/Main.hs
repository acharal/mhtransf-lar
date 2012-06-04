module Main where

import System.Environment

import AuxFunGen
import AuxFunEnv
import Types
import Syntax
import HFtoHI
import HItoHI
import HItoZI
import TypeInfer
import LAR
import ZItoCl
import ZItoC
import qualified LARN

-- ------|---------|-----        Main        ----|---------|---------|

data Action =
     ANone      -- do absolutely nothing
   | APrintHOFL -- print the HOFL program
   | ACheck     -- check the validity of the HOFL program
   | APrintHOIL -- preprocess and print the HOIL program
   | APrintEnv  -- preprocess and print the HOIL environment
   | APrintZOIL -- transform and print the ZOIL program
   | APrintLAR  -- transform and print the ZOIL (LAR) program
   | APrintLARN -- transform and print the ZOIL (LARN) program
   | ATransform -- transform and print all programs
   | AEvaluate    -- transform and evaluate the ZOIL program
   | AEvalAll     -- transform and evaluate the ZOIL program
   | ADepGraph    -- transform and print the dependency graph
   | ACompile     -- transform and compile the ZOIL program to C
   | ACompileLAR  -- transform and compile the ZOIL program to C using LAR
   | ACompileWH   -- transform and compile the ZOIL program to C using WH
   | ADims

data State = State {
      stAction  :: Action,        -- what to do
      stInput   :: Maybe String,  -- where to read the input
      stHcTSize :: Int,           -- log2 of size of HC hash table
      stHcHSize :: Int,           -- log2 of size of HC heap
      stWhTSize :: Int,           -- log2 of size of WH hash table
      stWhHSize :: Int,           -- log2 of size of WH heap
      stGlobal  :: Bool,          -- global or local world?
      stUseWh   :: Bool,          -- use the warehouse?
      stUseDep  :: Bool,          -- use dependency analysis?
      stTrace   :: Bool,          -- trace evaluation?
      stGraph   :: Maybe [Int]    -- list of dimensions to restrict
   }

usage :: IO ()
usage = do putStr "Usage: mhtransf <options>\n\n"
           putStr "Note:\n"
           putStr "-----\n"
           putStr "For the time being, it reads its input from stdin\n"
           putStr "and produces its output to stdout\n\n"
           putStr "Options:\n"
           putStr "--------\n"
           putStr "  -h    : print this usage message\n"
           putStr "  -p0   : print the HOFL program\n"
           putStr "  -s    : check the validity of the HOFL program\n"
           putStr "  -p1   : preprocess and print the HOIL program\n"
           putStr "  -e    : preprocess and print the HOIL environment\n"
           putStr "  -p    : transform and print the ZOIL program\n"
           putStr "  -plar : transform and print the ZOIL (LAR) program\n"
           putStr "  -plarn: transform and print the ZOIL (LARN) program\n"
           putStr "  -r    : transform and evaluate the ZOIL program\n"
           putStr "  -t    : transform and print all steps\n"
           putStr "  -tr   : transform and evaluate all steps\n"
           putStr "  -d dl : transform and print the dependency graph\n"
           putStr "          where 'dl' is an (optional) list of dimensions\n"
           putStr "          to restrict the graph to, e.g. [1,2,3]\n"
           putStr "  -c    : transform and compile the ZOIL program to C\n"
           putStr "  -clar : transform and compile the ZOIL program to C using LAR model\n"
           putStr "  -cwh  : transform and compile the ZOIL program to C using Warehouse\n"
           putStr "  -v    : C program (or -r) will trace the evaluation\n"
           putStr "  -g    : use a global world in the compiled C program\n"
           putStr "  -nw   : do not use the warehouse\n"
           putStr "  -nd   : do not use dependency analysis\n"
           putStr "  -wt n : set the size of the warehouse table to 2^n\n"
           putStr "  -wh n : set the size of the warehouse heap to 2^n\n"
           putStr "  -ht n : set the size of the hashconsing table to 2^n\n"
           putStr "  -hh n : set the size of the hashconsing heap to 2^n\n"

processArgs :: [String] -> IO State
processArgs args =
    let aux [] st                = return st
        aux ("-p0"    : args) st = aux args st{stAction = APrintHOFL}
        aux ("-p1"    : args) st = aux args st{stAction = APrintHOIL}
        aux ("-s"     : args) st = aux args st{stAction = ACheck}
        aux ("-e"     : args) st = aux args st{stAction = APrintEnv}
        aux ("-p"     : args) st = aux args st{stAction = APrintZOIL}
        aux ("-plar"  : args) st = aux args st{stAction = APrintLAR}
        aux ("-plarn" : args) st = aux args st{stAction = APrintLARN}
        aux ("-t"     : args) st = aux args st{stAction = ATransform}
        aux ("-r"     : args) st = aux args st{stAction = AEvaluate}
        aux ("-tr"    : args) st = aux args st{stAction = AEvalAll}
	aux ("-dims"  : args) st = aux args st{stAction = ADims}
        aux ("-d"     : args) st =
           let st' = st{stAction = ADepGraph}
           in  case args of
                  [] ->
                     aux args st'
                  (arg : args') ->
                     let rml :: [([Int], String)]
                         rml = reads arg
                     in  case rml of
                            [(ml, "")] ->
                               let st'' = st'{stGraph = Just ml}
                               in  aux args' st''
                            otherwise ->
                               aux args st'
        aux ("-clar"  : args) st = aux args st{stAction = ACompileLAR}
        aux ("-cwh": args) st = aux args st{stAction = ACompileWH}
        aux ("-c"     : args) st = aux args st{stAction = ACompileLAR}        
        aux ("-g"  : args) st = aux args st{stGlobal = True}
        aux ("-nw" : args) st = aux args st{stUseWh = False}
        aux ("-nd" : args) st = aux args st{stUseDep = False}
        aux ("-v"  : args) st = aux args st{stTrace = True}
        aux ("-wt" : arg : args) st =
            let nml :: [(Int, String)]
                nml = reads arg
            in  case nml of
                   [(n, "")] -> aux args st{stWhTSize = n}
                   otherwise -> usage >> return st{stAction = ANone}
        aux ("-wh" : arg : args) st =
            let nml :: [(Int, String)]
                nml = reads arg
            in  case nml of
                   [(n, "")] -> aux args st{stWhHSize = n}
                   otherwise -> usage >> return st{stAction = ANone}
        aux ("-ht" : arg : args) st =
            let nml :: [(Int, String)]
                nml = reads arg
            in  case nml of
                   [(n, "")] -> aux args st{stHcTSize = n}
                   otherwise -> usage >> return st{stAction = ANone}
        aux ("-hh" : arg : args) st =
            let nml :: [(Int, String)]
                nml = reads arg
            in  case nml of
                   [(n, "")] -> aux args st{stHcHSize = n}
                   otherwise -> usage >> return st{stAction = ANone}
        aux args st           = usage >> return st{stAction = ANone}
        defaultState :: State
        defaultState = State {
           stAction  = ACompileLAR,
           stInput   = Nothing,
           stHcTSize = 16,
           stHcHSize = 16,
           stWhTSize = 16,
           stWhHSize = 16,
           stGlobal  = False,
           stUseWh   = True,
           stUseDep  = True,
           stTrace   = False,
           stGraph   = Nothing
        }
    in  aux args defaultState

allSteps :: [HOILProg] -> IO ()
allSteps pl =
    let line =
            do putStr (take 70 (repeat '-'))
               putStr "\n"
        aux n [] = return ()
        aux n (p : pl) =
            do line
               putStr "STEP: "
               print n
               line
               print p
               aux (n+1) pl
    in  do aux 0 pl
           line


--bindList :: HOFLProg -> VarEnv -> [(VName,[(Int, [VName])])]
bindList p venv = [ (x, (formals x)) | (x,_) <- venv ]
        where formals v = let ProgF defs = p
                              lkup v ((DefF v' x _):ds) = if v == v' then x else (lkup v ds)
                              lkup v [] = []
                          in (lkup v defs)
                             


main :: IO ()
main =
    do args  <- getArgs
       state <- processArgs args
       text  <- getContents
       let p0 = read text :: HOFLProg
       let p1 = fromHFtoHI p0
       e1 <- makeVarEnv p1
       let ord = orderE e1
       let pl = p1 : transformHItoHI e1 p1
       let (p2, e2, dimL) = fromHItoHI e1 p1
       let dimL' =
              if stUseWh state then
                 if stUseDep state then
                    dimL
                 else
                    map (\(vn, dim) -> (vn, ord)) dimL
              else
                 []
       let p3 = fromHItoZI p2
       let e3 = e2
       let p4 = fromZOILtoLAR p3 e1 (makeMyEnv p1 e1)
       let p5 = LARN.translate p4
       case stAction state of
           ANone ->
               return ()
           APrintHOFL ->
               print p0
           ACheck ->
               isValidHOIL p1 e1 >>= \ok ->
               if ok then
                  putStr "Valid program of order " >>
                  print (orderE e1)
               else
                  return ()
           APrintHOIL ->
               print p1
           APrintEnv ->
               putStr (showEnv e1)
           ATransform ->
               allSteps pl
           AEvaluate ->
               LARN.evaluator (stTrace state) ord p5 >>= \val ->
               print val
           AEvalAll ->
               putStr "Cannot do that yet!!!\n" >>
               print dimL'
           APrintZOIL ->
               isValidHOIL p1 e1 >>= \ok ->
               if ok then
                  print p3
               else
                  return ()
           APrintLAR ->
               isValidHOIL p1 e1 >>= \ok ->
               if ok then
                  print p4
               else
                  return ()
           APrintLARN ->
               isValidHOIL p1 e1 >>= \ok ->
               if ok then
                  let f = foldl (.) id
                            (map (\x -> (showsPrec 0 x) . ("\n"++)) p5)
                  in  putStr (f "")
               else
                  return ()
           ADepGraph ->
               print "Dep graph here"
           ACompileLAR ->
               isValidHOIL p1 e1 >>= \ok ->
               if ok then
                  putStr (LAR.makeC p4 ord "")
               else
                  return ()
           ACompileWH ->
               isValidHOIL p1 e1 >>= \ok ->
               if ok then
                  putStr (ZItoC.makeC ord (stTrace state) e3 p3 dimL'
                                          (stHcTSize state) (stHcHSize state)
                                          (stWhTSize state) (stWhHSize state)
                                          (stGlobal state))
               else
                  return ()
           ADims ->
              isValidHOIL p1 e1 >>= \ok ->
              if ok then 
                 return ()
              else
                 return ()

-- ------|---------|---------|---------|---------|---------|---------|

makeMyEnv :: HOILProg -> VarEnv -> [(VName, [(VName, Type)])]
makeMyEnv prog env = let l = [ (x, (zip xs t')) | (x,xs) <- (toList prog), (y, (T t' _)) <- env , x == y]
                         mkFormals :: (VName, Type) -> (VName, [(VName, Type)])
                         mkFormals (x, (T ts@(_:_) _)) = (x,  zip (newformals x) ts )
                                                       where newformals x =  map (\(x,i)-> x++"_"++(show i)) (zip (repeat x) [0..])
                         mkFormals (x, (T [] _)) = (x, [])
                         
                         prodFormals :: (VName, Type) -> [(VName, [(VName, Type)])]
                         prodFormals a@(x, T [] _) = []
                         prodFormals a@(x, T _ _)  = let b@(_,ys) = (mkFormals a)
                                                     in b:(foldl (++) [] [ prodFormals z | z <- ys ])
                         l' = (foldl (++) [] (map snd l))
                     in l++(foldl (++) [] [ prodFormals x | x <- l'])
                   where toList :: HOILProg -> [(VName, [VName])]
                         toList (ProgH ds) = map (\(DefH n ns _)-> (n,ns)) ds
