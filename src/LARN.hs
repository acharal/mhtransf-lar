module LARN where

import AuxFunZero
import Types
import LAR

-- import Debug.Trace
trace msg x = x

-- Syntax

-- type Dim = Int -- imported from LAR
type Pos = Int

type LARNProg    = [LARNDef]
type LARNDef     = (VName, LARNBody)
type LARNBody    = (Maybe (Dim, Pos), LARNActuals, LARNExpr)
type LARNActuals = [Dim]
data LARNExpr    = LARNconst CName [LARNExpr]
                 | LARNvararg LARNCalls LARNVar
  deriving Show
type LARNCalls   = [(Dim, VName)]
data LARNVar     = LARNvar VName
                 | LARNarg Dim Pos
  deriving Show


-- Semantic domains

type World  = [NTag]
type NTag   = [ActRec]
type ActRec = [Arg]
data Arg    = ArgValue Value
            | ArgFunc VName


-- Semantic function
evalE :: Bool -> LARNProg -> LARNExpr -> World -> IO (Value, World)

evalE trace prog (LARNconst "if" [e, e1, e2]) w = 
   evalE trace prog e w >>= \(VB test, w') -> 
   if test then
      evalE trace prog e1 w'
   else
      evalE trace prog e2 w'
evalE trace prog (LARNconst "-u" [e]) w = 
   evalE trace prog e w >>= \(VI i1, w') ->
   return (VI (- i1), w')
evalE trace prog (LARNconst "+" [e1, e2]) w = 
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VI (i1 + i2), w2)
evalE trace prog (LARNconst "-" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VI (i1 - i2), w2)
evalE trace prog (LARNconst "*" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VI (i1 * i2), w2)
evalE trace prog (LARNconst "/" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VI (i1 `div` i2), w2)
evalE trace prog (LARNconst "div" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VI (i1 `div` i2), w2)
evalE trace prog (LARNconst "mod" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VI (i1 `mod` i2), w2)
evalE trace prog (LARNconst "==" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VB (i1 == i2), w2)
evalE trace prog (LARNconst "/=" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VB (i1 /= i2), w2)
evalE trace prog (LARNconst "<" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VB (i1 < i2), w2)
evalE trace prog (LARNconst ">" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VB (i1 > i2), w2)
evalE trace prog (LARNconst "<=" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VB (i1 <= i2), w2)
evalE trace prog (LARNconst ">=" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VI i1, w1) ->
   evalE trace prog e2 w1 >>= \(VI i2, w2) ->
   return (VB (i1 >= i2), w2)
evalE trace prog (LARNconst "&&" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VB b1, w1) ->
   evalE trace prog e2 w1 >>= \(VB b2, w2) ->
   return (VB (b1 && b2), w2)
evalE trace prog (LARNconst "||" [e1, e2]) w =
   evalE trace prog e1 w >>= \(VB b1, w1) ->
   evalE trace prog e2 w1 >>= \(VB b2, w2) ->
   return (VB (b1 && b2), w2)
evalE trace _ (LARNconst "[]" _) w =
   return (VL [], w)
evalE trace prog (LARNconst "null" [e]) w =
   evalE trace prog e w >>= \(VL l, w') ->
   return (VB (null l), w')
evalE trace prog (LARNconst ":" [e1, e2]) w =
   evalE trace prog e1 w >>= \(val, w1) ->
   evalE trace prog e2 w1 >>= \(VL l, w2) ->
   return (VL (val:l), w2)
evalE trace prog (LARNconst "head" [e]) w =
   evalE trace prog e w >>= \(VL l, w') ->
   if null l then
      error "head []"
   else
      return (head l, w')
evalE trace prog (LARNconst "tail" [e]) w =
   evalE trace prog e w >>= \(VL l, w') ->
   if null l then
      error "tail []"
   else
      return (VL (tail l), w')
evalE trace prog (LARNconst "True" []) w =
   return (VB True, w)
evalE trace prog (LARNconst "False" []) w =
   return (VB False, w)
evalE trace _ (LARNconst cn []) w 
   | cn /= "[]"  = return (VI (read cn::Int), w)
evalE trace prog (LARNvararg gamma z) w =
   let (w_gamma, inv) = semCalls gamma w
   in  evalV trace prog z w_gamma >>= \(v, w_gamma') ->
       return (v, inv w_gamma')
            
searchVar vn [] =
   error ("Definition for " ++ vn ++ " not found")
searchVar vn ((vd, bd) : p) =
   if vd == vn then
      bd
   else
      searchVar vn p

evalV :: Bool -> LARNProg -> LARNVar -> World -> IO (Value, World)
evalV trace prog (LARNvar vn) w =
   let bd = searchVar vn prog
   in  evalB trace prog bd w
evalV trace prog (LARNarg m n) w =
   case head (w !! m) !! n of
      ArgValue v ->
         return (v, w)
      ArgFunc vn ->
         let bd = searchVar vn prog
         in  evalB trace prog bd w

evalB :: Bool -> LARNProg -> LARNBody -> World -> IO (Value, World)
evalB trace prog (save, alpha, e) w =
   let (w_alpha, inv) = semActuals alpha w
   in  evalE trace prog e w_alpha >>= \(v, w_alpha') ->
       let w' = inv w_alpha'
       in  case save of
              Nothing ->
                 return (v, w')
              Just (d, p) ->
                 if d == 0 then
                    return (v, update d p (ArgValue v) w')
                 else
                    let getName (LARNvararg _ (LARNvar vn)) =
                           Just (ArgFunc vn)
                        getName (LARNvararg _ (LARNarg m n)) =
                           Just (head (w_alpha' !! m) !! n)
                        getName _ =
                           Nothing
                    in  case getName e of
                           Nothing ->
                              return (v, w')
                           Just arg ->
                              return (v, update d p arg w')

update :: Dim -> Pos -> Arg -> World -> World
update dim pos arg w =
   let (l1, ((a : t) : l2)) = splitAt dim w
       (a1, (_ : a2)) = splitAt pos a
       a' = a1 ++ (arg : a2)
   in  l1 ++ ((a' : t) : l2)

call :: Dim -> ActRec -> World -> World
call dim a w =
   let (l1, (t : l2)) = splitAt dim w
   in  l1 ++ ((a : t) : l2)

actual :: Dim -> World -> (ActRec, World)
actual dim w =
   let (l1, ((a : t) : l2)) = splitAt dim w
   in  (a, l1 ++ (t : l2))

semCalls :: LARNCalls -> World -> (World, World -> World)
semCalls [] w = (w, id)
semCalls calls w =
   let split dim [] =
          [(dim, [])]
       split dim (call@(d, vn) : calls) =
          let ((_, h) : t) = split d calls
          in  if d == dim then
                 ((dim, call : h) : t)
              else
                 (dim, []) : ((d, call : h) : t)
       makeActRec [] = []
       makeActRec ((_, vn) : calls) = ArgFunc vn : makeActRec calls
       extend [] w inv = (w, inv)
       extend ((dim, calls) : rest) w inv =
          let actrec = makeActRec calls
              w' = call dim actrec w
              inv' = snd . actual dim . inv
          in  extend rest w' inv'
       dl = split (fst (head calls)) calls
   in  extend dl w id

semActuals :: LARNActuals -> World -> (World, World -> World)
semActuals actuals w =
   let shrink [] w inv = (w, inv)
       shrink (dim : rest) w inv =
          let (actrec, w') = actual dim w
              inv' = call dim actrec . inv
          in  shrink rest w' inv'
   in  shrink actuals w id

evaluator :: Bool -> Order -> LARNProg -> IO Value
evaluator trace ord prog =
   let w = replicate ord ([] :: NTag)
   in  evalV trace prog (LARNvar "result") w >>= \(val, w') ->
       return val


-- Translation LAR -> LARN

cutLast str =
   let aux res "" =
          res >>= \n ->
          return (n, "")
       aux res (c : str) =
          let doit =
                 case (res, c) of
                    (_, '_')     -> aux (Just 0) str
                    (Nothing, _) -> aux Nothing str
                    (Just n, _)  -> aux (Just (n+1)) str
          in  doit >>= \(n, res) ->
              let res' = if n < 0 then c : res else res
              in  return (n-1, res')
   in  case aux Nothing str of
          Just (n, res) -> res
          Nothing       -> str

cutSubscript str =
   let aux res "" = res
       aux res ('_' : '_' : str) = res
       aux res (c : str) = aux (res ++ [c]) str
   in  aux "" str

translate :: LARProg -> LARNProg
translate (LARProg bll) =
   let walk [] v = Nothing
       walk (block : bll) v =
          let find [] _ _ = Nothing
              find ((vp, d) : rest) p d' =
                 let p' = if d == d' then p+1 else 0
                 in  trace ("Eq: " ++ vp ++ " and " ++ v) (
                     if vp == v then
                        Just (d, p')
                     else
                        find rest p' d
                     )
              aux v vn ctx bll =
                 if v == vn then
                    Nothing
                 else
                    case find ctx 0 (-1) of
                       Nothing -> walk bll v
                       res     -> res
          in  case block of
                 Func vn _ ctx  -> aux v vn ctx bll
                 Var vn _ _ ctx -> aux v vn ctx bll
       ctx = walk bll
   in  map (translateDef ctx) bll
translateDef ctx (Func vn stm _) =
   (vn, translateBody ctx vn stm)
translateDef ctx (Var vn vpar stm _) =
   (vn, translateBody ctx vn stm)
translateBody ctx vn (LARStm ops e) =
   let transOps [] =
          ([], [])
       transOps (LARAct d _ : ops) =
          let (a, c) = transOps ops
          in  (d : a, c)
       transOps (LARForw d vnl : ops) =
          let (a, c) = transOps ops
          in  (a, map (\vn -> (d, vn)) vnl ++ c)
       forward [] e = e
       forward calls (LARNvararg calls' z) = LARNvararg (calls ++ calls') z
       forward _ (LARNconst _ _) = error "LARN internal: forward to const"
       (a, c) = transOps ops
       e' = translateExpr ctx e
   in  (ctx (cutSubscript vn), a, forward c e')
translateExpr ctx (LARCall vn vdl) =
   let calls = map (\(vn, d) -> (d, vn)) vdl
       z = case ctx vn of
              Nothing     -> LARNvar vn
              Just (d, p) -> LARNarg d p
   in  LARNvararg calls z
translateExpr ctx (LARC vn el) =
   LARNconst vn (map (translateExpr ctx) el)
