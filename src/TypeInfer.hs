module TypeInfer where

import Data.Char

import Types
import Syntax


-- Type inference

type STVar = Int

data SType =
        STvar STVar
     |  STint
     |  STbool
     |  STreal
     |  STlist SType
     |  STfunc SType SType
  deriving Eq

instance Show SType where
    showsPrec p STint = ("Int" ++)
    showsPrec p STbool = ("Bool" ++)
    showsPrec p STreal = ("Real" ++)
    showsPrec p (STlist t) = ("[" ++) . showsPrec 0 t . ("]" ++)
    showsPrec p (STvar v) = showsPrec 0 v
    showsPrec p (STfunc t1 t2) =
        showParen (p > 0) (
            showsPrec 1 t1 . (" -> " ++) . showsPrec 0 t2
        )

mhRealType :: SType -> Type
mhRealType STint = T [] I
mhRealType STbool = T [] B
mhRealType STreal = -- T [] R
	 error "cannot do reals yet!!!"
mhRealType (STlist t) =
    case mhRealType t of
        T [] g    -> T [] (L g)
	otherwise -> error "list elements cannot be functions"
mhRealType (STfunc t1 t2) =
    case mhRealType t2 of
        T tl g -> T (mhRealType t1 : tl) g
mhRealType (STvar v) = error ("variable " ++ show v ++
                              " not generalized")

freeST :: SType -> [STVar]
freeST (STvar v) = [v]
freeST STint = []
freeST STbool = []
freeST STreal = []
freeST (STlist t) = freeST t
freeST (STfunc t1 t2) =
    let l1 = freeST t1
        l2 = freeST t2
    in  l1 ++ filter (\v -> not (elem v l1)) l2

type STEnv = [(VName, SType)]
type STSub = [(STVar, SType)]
type STMstate = (Int, STEnv, STSub)

showSTEnv :: STEnv -> String
showSTEnv gamma =
   let aux [] = id
       aux ((vname, t) : l) =
          (vname ++) . (" :: " ++) . showsPrec 0 t . ("\n" ++) .
          aux l
   in  aux gamma ""

unbind :: VName -> STEnv -> STEnv
unbind v = filter (\(v', t) -> v /= v')

type STD a = IO a

extractSTD :: STD a -> IO a
extractSTD = id

stdPrint :: String -> STD ()
stdPrint = putStr

newtype STM a = STM (STMstate -> STD (STMstate, Maybe a))

instance Monad STM where
    return v    = STM (\st -> return (st, return v))
    STM x >>= f = STM (\st -> x st >>= \(st', m) ->
                              case m of
                                   Just v ->
                                       let STM y = f v
                                       in  y st'
                                   Nothing ->
                                       return (st', Nothing))

messageST :: String -> STM ()
messageST msg =
    STM (\st -> stdPrint msg >> return (st, return ()))

debugST :: String -> String -> STM ()
debugST cls msg =
    if {- DEBUG FLAG IS: -} False then
        let adjust n str = take n (str ++ repeat ' ')
        in  messageST (">> " ++ adjust 8 (take 7 cls ++ ":") ++ 
                       " " ++ msg)
    else
        return ()

liftST :: Maybe a -> STM a
liftST m = STM (\st -> return (st, m))

extractST :: STM a -> IO a
extractST (STM x) =
    let st0 = (1, [], [])
        std = x st0 >>= \(st', m) ->
              case m of
                  Just v  -> return v
                  Nothing -> error "type inference failed"
    in  extractSTD std

getEnvST :: STM STEnv
getEnvST =
    STM (\st@(n, gamma, sigma) ->
         return (st, return gamma))

setEnvST :: STEnv -> STM ()
setEnvST gamma' =
    STM (\(n, gamma, sigma) ->
         return ((n, gamma', sigma), return ()))

getSubST :: STM STSub
getSubST =
    STM (\st@(n, gamma, sigma) ->
         return (st, return sigma))

setSubST :: STSub -> STM ()
setSubST sigma' =
    STM (\(n, gamma, sigma) ->
         return ((n, gamma, sigma'), return ()))

freshST :: STM SType
freshST =
    STM (\(n, gamma, sigma) ->
         return ((n+1, gamma, sigma), return (STvar n)))

unify :: SType -> SType -> STM ()
unify t t' =
    let unify_ t@(STvar v) t'@(STvar v') =
            if v == v' then
                debugST "test" ("type match succeeded for " ++
                                 show t ++ "\n")
            else
                getSubST >>= \sigma ->
                case (lookup v sigma, lookup v' sigma) of
                    (Just tr, Just tr') ->
                        unify tr tr'
                    (Just tr, Nothing) ->
                        unify tr t'
                    (Nothing, Just tr') ->
                        unify t tr'
                    (Nothing, Nothing) ->
                        debugST "set" (show v ++
                                       " := " ++ show t' ++ "\n") >>
                        setSubST ((v, t') : sigma)
        unify_ (STvar v) t =
            getSubST >>= \sigma ->
            case lookup v sigma of
                Just t' ->
                    unify t' t
                Nothing ->
                    debugST "set" (show v++" := "++show t++"\n") >>
                    setSubST ((v, t) : sigma)
        unify_ t tv@(STvar v) =
            unify tv t
        unify_ (STfunc t1 t2) (STfunc t1' t2') =
            unify t1 t1' >>
            unify t2 t2'
	unify_ (STlist t) (STlist t') =
	    unify t t'
        unify_ t1 t2 =
            if t1 == t2 then
                debugST "test" ("type match succeeded for " ++ 
                                 show t1 ++ "\n")
            else
                messageST ("Unification failed:\n" ++
                    "   t1 = " ++ show t1 ++ "\n" ++
                    "   t2 = " ++ show t2 ++ "\n") >>
                liftST Nothing
    in  debugST "unify" (show t ++ " with " ++ show t' ++ "\n") >>
        unify_ t t'

unifyL :: [SType] -> [SType] -> STM ()
unifyL [] [] =
    return ()
unifyL (t : tl) (t' : tl') =
    unify t t' >>
    unifyL tl tl'
unifyL tl tl' =
    messageST ("Unification failed:\n" ++
               "   tl1 = " ++ show tl ++ "\n" ++
               "   tl2 = " ++ show tl' ++ "\n") >>
    liftST Nothing

finalize :: SType -> STM SType
finalize t@(STvar v) =
    getSubST >>= \sigma ->
    case lookup v sigma of
        Just t' -> finalize t'
        Nothing -> return t
finalize (STfunc t1 t2) =
    finalize t1 >>= \t1' ->
    finalize t2 >>= \t2' ->
    return (STfunc t1' t2')
finalize (STlist t) =
    finalize t >>= \t' ->
    return (STlist t')
finalize t = return t

generalize :: SType -> STM SType
generalize t =
    let aux t [] =
            return ()
        aux t (v : vl) =
            getSubST >>= \sigma ->
            debugST "ground" (show v ++ "\n") >>
            setSubST ((v, STint) : sigma) >>
            aux t vl
    in  finalize t >>= \t' ->
        aux t' (freeST t') >>
        finalize t'
        -- return t'

generalizeEnv :: STEnv -> STM STEnv
generalizeEnv [] =
    return []
generalizeEnv ((v, t) : vtl) =
    generalize t >>= \t' ->
    generalizeEnv vtl >>= \vtl' ->
    return ((v, t') : vtl')

inferE :: HOILExpr -> STM SType
inferE e =
    let inferE_ (XH v) =
            getEnvST >>= \gamma ->
            case lookup v gamma of
               Just t  ->
                  return t
               Nothing -> 
                  messageST ("undefined variable " ++ v ++ "\n") >>
                  liftST Nothing
        inferE_ (ConH c el) =
	    let theta "+u"    = return (STint, [STint])
	        theta "-u"    = return (STint, [STint])
		theta "if"    = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (t, [STbool, t, t])
		theta "+"     = return (STint, [STint, STint])
		theta "-"     = return (STint, [STint, STint])
		theta "*"     = return (STint, [STint, STint])
		theta "/"     = return (STint, [STint, STint])
		theta "div"   = return (STint, [STint, STint])
		theta "mod"   = return (STint, [STint, STint])
		theta "=="    = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (STbool, [t, t])
		theta "/="    = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (STbool, [t, t])
		theta "<"     = return (STbool, [STint, STint])
		theta ">"     = return (STbool, [STint, STint])
		theta "<="    = return (STbool, [STint, STint])
		theta ">="    = return (STbool, [STint, STint])
		theta "&&"    = return (STbool, [STbool, STbool])
		theta "||"    = return (STbool, [STbool, STbool])
		theta "[]"    = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (STlist t, [])
		theta "null"  = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (STbool, [STlist t])
		theta ":"     = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (STlist t, [t, STlist t])
		theta "head"  = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (t, [STlist t])
		theta "tail"  = freshST >>= \t ->
                                debugST "new" (show t ++ "\n") >>
		                return (STlist t, [STlist t])
		theta "True"  = return (STbool, [])
		theta "False" = return (STbool, [])
		theta c =
		    case (reads c :: [(Int, String)]) of
		        [(n, "")] -> return (STint, [])
			otherwise -> error("undefined constant " ++ c)
	    in  theta c >>= \(t, tl) ->
	        inferEL el >>= \tl' ->
	        unifyL tl tl' >>
		return t
        inferE_ (FH op v el) =
	    let makeFunc [] tr = tr
	        makeFunc (t : tl) tr = STfunc t (makeFunc tl tr)
	    in  getEnvST >>= \gamma ->
	        case lookup v gamma of
	           Just t ->
                      freshST >>= \t' ->
                      debugST "new" (show t' ++ "\n") >>
                      inferEL el >>= \tl -> 
                      unify (makeFunc tl t') t >>
                      return t'
                   Nothing ->
                      messageST ("undefined function " ++ v ++ "\n") >>
                      liftST Nothing
    in  debugST "goal" (show e ++ "\n") >>
        inferE_ e >>= \t ->
        debugST "infer" (show e ++ " : " ++ show t ++ "\n") >>
        return t

inferEL :: [HOILExpr] -> STM [SType]
inferEL [] =
    return []
inferEL (e : el) =
    inferE e >>= \t ->
    inferEL el >>= \tl ->
    return (t : tl)

inferDL :: [HOILDef] -> STM ()
inferDL dl =
    let prepare [] =
            return []
        prepare (DefH v vl e : dl) =
            freshST >>= \t ->
            debugST "new" (show t ++ "\n") >>
            getEnvST >>= \gamma ->
            debugST "var+" (show v ++ " : " ++ show t ++ "\n") >>
            setEnvST ((v, t) : gamma) >>
            prepare dl >>= \l ->
            return (t : l)
	prepare _ =
	    error "cannot type infer an intensional program"
        process [] [] =
            return ()
        process (DefH v vl e : dl) (t : tl) =
            let formals [] =
                    freshST >>= \t ->
                    debugST "new" (show t ++ "\n") >>
                    return (t, t)
                formals (v : vl) =
                    freshST >>= \t ->
                    debugST "new" (show t ++ "\n") >>
                    getEnvST >>= \gamma ->
                    debugST "var+" (show v ++ " : " ++ show t ++ "\n") >>
                    setEnvST ((v, t) : gamma) >>
                    formals vl >>= \(tf, tr) ->
                    return (STfunc t tf, tr)
                unformals [] =
                    return ()
                unformals (v : vl) =
                    getEnvST >>= \gamma ->
                    debugST "var-" (show v ++ "\n") >>
                    setEnvST (unbind v gamma) >>
                    unformals vl
            in  formals vl >>= \(tf, tr) ->
                unify t tf >>
                inferE e >>= \te ->
                unify te tr >>
                unformals vl >>
                process dl tl
    in  prepare dl >>= \tl ->
        process dl tl

mhTypeInfer :: HOILProg -> IO VarEnv
mhTypeInfer (ProgH dl) =
    extractST (
        inferDL dl >>
        getEnvST >>= \gamma ->
        generalizeEnv gamma >>= \gamma' ->
        debugST "result" ("\n" ++ showSTEnv gamma' ++ "\n") >>
        return (map (\(v, t) -> (v, mhRealType t)) gamma')
    )


-- Closure of type environments (adding types of parameters)

getArgType :: Type -> Int -> Type
getArgType tt@(T tl t) n =
    let aux (t : tl) 1 = t
        aux (t : tl) m = aux tl (m-1)
	aux [] m = error ("getArgType\n" ++
                          "   type " ++ show tt ++ " has no " ++
		  	  show n ++ " argument")
    in  aux tl n

formals :: HOILProg -> VName -> [VName]
formals (ProgH dl) ov =
    let aux [] = error ("formals\n" ++
                        "   definition of " ++ ov ++ " not found")
        aux (DefH vd vl e : dl) =
            if ov == vd then vl else aux dl
	aux _ =
	    error "cannot type infer an intensional program"
    in  aux dl

closeEnv :: HOILProg -> VarEnv -> VarEnv
closeEnv p xl =
    let top [] rho yl = par yl rho
        top ((v, t) : xl) rho yl =
            let vl = formals p v
                zl = map (\(v, i) -> (v, getArgType t i))
                         (zip vl (enumFrom 1))
            in  top xl ((v, t) : rho) (yl ++ zl)
        par [] rho = rho
        par ((v, t) : xl) rho = par xl ((v, t) : rho)
    in  top xl [] []


-- Calculate the environment

makeVarEnv :: HOILProg -> IO VarEnv
makeVarEnv p =
   mhTypeInfer p >>= \env ->
   return (closeEnv p env)


-- Check if the original HOIL program is valid

arity :: Type -> Int
arity (T tl g) = length tl

isValidHOIL :: HOILProg -> VarEnv -> IO Bool
isValidHOIL (ProgH dl) xl =
    let aux [] flag =
            return flag
        aux (DefH v vl e : dl) flag =
	    case lookup v xl of
	        Just t ->
		    if length vl == arity t then
		        chk e flag >>= \flag' ->
	                aux dl flag'
                    else
		        putStr ("function " ++ v ++
                                " does not produce a ground result\n") >>
			chk e False >>
			aux dl False
		Nothing ->
		    error "internal error in calculating environment"
        aux _ flag =
	    error "cannot type infer an intensional program"
	chk (XH v) flag =
	    return flag
	chk (ConH c el) flag =
	    chkL el flag
	chk (FH op v el) flag =
	    case lookup v xl of
	        Just t ->
		    if length el == arity t then
		        chkL el flag
                    else
		        putStr ("partial application: " ++ v ++
                                " " ++ show el ++ "\n") >>
			chkL el False
		Nothing ->
		    error "internal error in calculating environment"
        chkL [] flag =
	    return flag
	chkL (e : el) flag =
	    chk e flag >>= \flag' ->
	    chkL el flag'
    in  aux dl True

-- ------|---------|---------|---------|---------|---------|---------|
