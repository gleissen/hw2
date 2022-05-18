{-# LANGUAGE UnicodeSyntax #-}
module Horn.Logic.Clauses where
import           Data.List   (intercalate)
import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.Maybe
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Control.Monad.State.Strict
import           Debug.Trace

data Exp =   Var String
            | Num Integer
            | Plus [Exp]
            | Minus [Exp]
            | Times [Exp]
            deriving (Eq,Ord)

data Base =   Tr
            | Fl
            | Eq Exp Exp
            | Geq Exp Exp
            | Leq Exp Exp
            | Neg Base
            | And [Base]
            | Or  [Base]
            | Implies Base Base
            deriving (Eq,Ord)

type Name = String
type Var = Exp
data Query = Query { name :: Name, vars :: [Var]} deriving (Eq, Ord)

-- These are regular Horn clauses where the head is a query
data Horn a =  Horn { hd    :: Query
               ,      bd    :: [Query]
               ,      base  :: Base
               ,      annot :: a
               } deriving (Eq, Ord)


-- These are Horn clauses where the head is a formula (bound).
-- We allow a formula in the body and a formula in the head.
-- That is, this represents a clause q1(x1,..) /\ q2(x2,..) /\ bbase -> bound.

data Bound  = Bound { queries  :: [Query] 
               ,      bbase    :: Base
               ,      bound    :: Base
              } deriving (Eq, Ord)


-- Solutions map each predicate names to a disjunction (set) of base formulas.
-- Additionally, they contain a list of variables used in the solution formula.
type Solution = Map Name ([Var], Set Base)

------------------
-- pretty printing
------------------
instance Show Base where
  show (Eq e1 e2)      = (show e1) ++ "=" ++ (show e2)
  show (Geq e1 e2)     = (show e1) ++ "≥" ++ (show e2)
  show (Leq e1 e2)     = (show e1) ++ "≤" ++ (show e2)
  show (Neg e)         = "¬" ++ (show e)
  show (And [])       = "True"
  show (And es)        = "(" ++ intercalate "∧" (map show es) ++ ")"
  show (Or es)         = intercalate "∨" (map show es)
  show (Implies e1 e2) = "(" ++ (show e1) ++ "⇒" ++ (show e2) ++ ")"
  show (Tr)          = "True"
  show (Fl)         = "False"

instance Show Exp where
  show (Var s)    = s
  show (Num n)    = (show n)
  show (Plus es)  = intercalate "+" (map show es)
  show (Minus es) = intercalate "-" (map show es)
  show (Times es) = intercalate "*" (map show es)

instance Show (Horn a) where
  show h = case (length (bd h)) of
    0         -> (show $ hd h) ++ " :- " ++ show (base h) ++ "."
    otherwise ->  (show $ hd h) ++ " :- " ++ bd_ ++ " ∧ " ++ show (base h) ++ "."
    where
      bd_ = intercalate "∧" (map show $ bd h)

instance Show (Bound) where
  show b = (show $ bound b) ++ " :- " ++ bd
    where 
      bd = intercalate "∧" (map show $ queries b)

instance Show Query where
  show p = (name p) ++ "(" ++ vars_ ++ ")"
    where
      vars_ = intercalate "," (map show $ vars p)


-- Helper functions
data NState = N { nxv :: Int} deriving (Show)
type NM = State NState 

-------------------------------------------------------------------
initN :: NState
-------------------------------------------------------------------
initN = N {nxv = 0}

-------------------------------------------------------------------
nxVar :: NM Var
-------------------------------------------------------------------
nxVar = do
        st <- get 
        let new = (nxv st) + 1
        put N{nxv = new}
        return $ Var $ "x" ++ (show new)

-------------------------------------
normalizeVar :: Exp -> NM (Var, Base)
-------------------------------------
normalizeVar e = error "TODO: FILL THIS IN" 

-----------------------------------------------
normalizeQuery :: Query -> NM (Query, [Base])
-----------------------------------------------
normalizeQuery p = error "TODO: FILL THIS IN" 

-----------------------------------------------
normalizeHorn :: Horn a -> NM (Horn a)
-----------------------------------------------
normalizeHorn h = error "TODO: FILL THIS IN" 

-----------------------------------------------
normalize :: Horn a -> Horn a
-----------------------------------------------
normalize h = fst $ runState (normalizeHorn h) initN

-----------------------------------------------
normalizeBoundM :: Bound -> NM (Bound)
-----------------------------------------------
normalizeBoundM b = error "TODO: FILL THIS IN" 

-----------------------------------------------
normalizeBound :: Bound -> Bound
-----------------------------------------------
normalizeBound b = fst $ runState (normalizeBoundM b) initN

-------------------------------------
getQueryNames :: Horn a -> Set Name
-------------------------------------
getQueryNames h = Set.fromList $ map name ([hd h] ++ (bd h))

-------------------------------------------------
dependsOn :: Name -> Horn a -> Bool
-------------------------------------------------
dependsOn p h = or $ map (((==) p).name) (bd h)

------------------------
isBase :: Horn a -> Bool
------------------------
isBase h = (bd h) == []

------------------------------------------
pluginHorn :: Solution -> Horn a -> Base
------------------------------------------
pluginHorn sol h = Implies (And $ [base h] ++ body) head
  where
    body = map (plugin sol) (bd h)
    head = plugin sol (hd h)

------------------------------------------
pluginBound :: Solution -> Bound -> Base
------------------------------------------
pluginBound sol b = Implies (And ([bbase b] ++ body)) (bound b)
  where 
    body = map (plugin sol) (queries b)


------------------------------------------
plugin :: Solution -> Query -> Base
------------------------------------------
plugin sol (Query p vs) =  substVars vs solVs pSol
  where pSol = Or $ map (substVars vs solVs) sts
        s = fromJust $ Map.lookup p sol 
        sts = Set.toList $ snd s
        solVs = fst s

------------------------------------
get_vars_horn :: Horn a -> Set Exp
------------------------------------
get_vars_horn h = Set.unions [Set.fromList $ vars $ hd h, get_vars $ base h]


---------------------------
get_vars :: Base -> Set Exp
---------------------------
get_vars (Eq e1 e2)      = Set.union  (get_vars_exp e1) (get_vars_exp e2)
get_vars (Geq e1 e2)     = Set.union (get_vars_exp e1) (get_vars_exp e2)
get_vars (Leq e1 e2)     = Set.union (get_vars_exp e1) (get_vars_exp e2)
get_vars (Neg e)         = get_vars e
get_vars (And es)        = Set.unions $ map get_vars es
get_vars (Or es)         = Set.unions $ map get_vars es
get_vars (Implies e1 e2) = Set.union (get_vars e1) (get_vars e2)
get_vars (Tr)            = Set.empty
get_vars (Fl)            = Set.empty

------------------------------
get_vars_exp :: Exp -> Set Exp
------------------------------
get_vars_exp (Var s)    = Set.singleton (Var s)
get_vars_exp (Num n)    = Set.empty
get_vars_exp (Plus es)  = Set.unions $ map get_vars_exp es
get_vars_exp (Minus es) = Set.unions $ map get_vars_exp es
get_vars_exp (Times es) = Set.unions $ map get_vars_exp es

----------------------------------------
substVars :: [Var] -> [Var] -> Base ->  Base
----------------------------------------
substVars vs' vs phi = foldl ((flip.uncurry) subst) phi (zip vs' vs)

-- subst phi y x = phi[y/x]
-------------------------------------
subst :: Exp -> Var -> Base ->  Base
-------------------------------------
subst  y x (Eq e1 e2)      =  Eq  (subst_exp y x e1) (subst_exp y x e2)
subst  y x (Geq e1 e2)     =  Geq  (subst_exp y x e1) (subst_exp y x e2)
subst  y x (Leq e1 e2)     =  Leq  (subst_exp y x e1) (subst_exp y x e2)
subst  y x (Neg e)         =  Neg (subst y x e)
subst  y x (And es)        =  And $ map (subst y x) es
subst  y x (Or es)         =  Or  $ map (subst y x) es
subst  y x (Implies e1 e2) =  Implies (subst y x e1) (subst y x e2)
subst  y x (Tr)          =  Tr
subst  y x (Fl)          =  Fl

---------------------------------------
subst_exp :: Exp -> Var -> Exp ->  Exp
---------------------------------------
subst_exp e (Var x) (Var x')
      | x==x'            = e
      | otherwise        = Var x'
subst_exp y x (Num n)    = Num n
subst_exp y x (Plus es)  = Plus $ map (subst_exp y x) es
subst_exp y x (Minus es) = Minus $ map (subst_exp y x) es
subst_exp y x (Times es) = Times $ map (subst_exp y x) es


-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
    let nh = normalize h
    putStr $ "original : " ++ (show h) ++ "\n"
    putStr $ "normalized : " ++ (show nh) ++ "\n"
    return ()
  where 
    h = Horn{hd = Query {name = "p", vars = [Num 1, Plus [(Var "x"),(Num 1)]]}, bd = [Query {name = "p", vars = [(Var "x"), (Var "y")]}], base = Tr, annot=[]}
