{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}

module Horn.Nano.Nano where

import qualified Horn.Logic.Clauses as L
import           Language.ECMAScript3.Syntax 
import           Language.ECMAScript3.PrettyPrint
import           Text.PrettyPrint.ANSI.Leijen       (text,(<+>))
import           Language.ECMAScript3.Parser        (parseFromFile)
import           Data.Maybe                         
import           Text.Parsec                    
import qualified Data.Set    as Set
import           Data.Set    (Set)
import           Data.Generics                      (Data)
import           Data.Typeable                      (Typeable)

data Exp = Var String
         | Num Integer
         | Plus Exp Exp
         | Minus Exp Exp 
         | Times Exp Exp deriving (Show,Eq,Ord)

data BExp = Bool Bool
          | And BExp BExp
          | Or BExp BExp
          | Lte Exp Exp
          | Gte Exp Exp 
          | Eq Exp Exp 
          | Neq Exp Exp deriving (Show)

data Stmt = Skip
          | Assign String Exp
          | Seq Stmt Stmt
          | SeqList [Stmt]
          | If BExp Stmt Stmt
          | While [L.Base] BExp Stmt 
          | Assume L.Base
          | Assert L.Base 
          | Pred [L.Base] deriving (Show)


-------------------------------------
getVarsExp :: Exp -> Set L.Var
-------------------------------------
getVarsExp (Var s) = Set.singleton $ L.Var s

getVarsExp (Num _) = Set.empty

getVarsExp (Plus e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2

getVarsExp (Minus e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2

getVarsExp (Times e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2

-------------------------------------
getVarsBExp :: BExp -> Set L.Var
-------------------------------------

getVarsBExp (Bool _) = Set.empty

getVarsBExp (And e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsBExp e1
    vs2 = getVarsBExp e2

getVarsBExp (Or e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsBExp e1
    vs2 = getVarsBExp e2    

getVarsBExp (Lte e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2   

getVarsBExp (Gte e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2   

getVarsBExp (Eq e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2   

getVarsBExp (Neq e1 e2) = Set.union vs1 vs2
  where
    vs1 = getVarsExp e1
    vs2 = getVarsExp e2   

-------------------------------------
getVars :: Stmt -> Set L.Var
-------------------------------------

getVars Skip = Set.empty

getVars (Assign x e) = Set.union vs1 vs2
    where
      vs1 = Set.singleton $ L.Var x
      vs2 = getVarsExp e

getVars (Seq s1 s2) = Set.union vs1 vs2
    where
      vs1 = getVars s1
      vs2 = getVars s2

getVars (SeqList ss) = Set.unions $ map getVars ss

getVars (If e s1 s2) = Set.unions [vse, vs1,vs2]
    where
      vse = getVarsBExp e
      vs1 = getVars s1
      vs2 = getVars s2

getVars (While b e s) = Set.unions [vsb, vse, vss]
    where
      vsb = Set.unions $ map L.get_vars b
      vse = getVarsBExp e
      vss = getVars s

getVars (Assume b) = L.get_vars b

getVars (Assert b) = L.get_vars b

-------------------------------------
expToBase :: Exp -> L.Exp
-------------------------------------              
expToBase (Var s) = L.Var s

expToBase (Num n) = L.Num n

expToBase (Plus e1 e2) = L.Plus [b1,b2]
    where 
        b1 = expToBase e1 
        b2 = expToBase e2

expToBase (Minus e1 e2) = L.Minus [b1,b2]
    where 
        b1 = expToBase e1 
        b2 = expToBase e2 

expToBase (Times e1 e2) = L.Times [b1,b2]
    where 
        b1 = expToBase e1
        b2 = expToBase e2 

-------------------------------------
bexpToBase :: BExp -> L.Base
-------------------------------------
bexpToBase (Bool b) 
        | b==True   = L.Or  []
        | b==False  = L.And []

bexpToBase (And e1 e2) = L.And [b1,b2]
    where 
        b1 = bexpToBase e1
        b2 = bexpToBase e2 
    
bexpToBase (Or e1 e2) = L.Or [b1,b2]
    where 
        b1 = bexpToBase e1
        b2 = bexpToBase e2

bexpToBase (Lte e1 e2) = L.Leq b1 b2
    where 
        b1 = expToBase e1
        b2 = expToBase e2 

bexpToBase (Gte e1 e2) = L.Geq b1 b2
    where 
        b1 = expToBase e1
        b2 = expToBase e2

bexpToBase (Eq e1 e2) = L.Eq b1 b2
    where 
        b1 = expToBase e1
        b2 = expToBase e2

bexpToBase (Neq e1 e2) = L.Neg $ L.Eq b1 b2
    where 
        b1 = expToBase e1
        b2 = expToBase e2



---------------------------------------------------------------------
-- | Parsing ESC/JAVA
---------------------------------------------------------------------

-- | `isNano` is a predicate that describes the **syntactic subset** 
--   of ECMAScript3 that comprises `Nano`.

class IsNano a where 
  isNano :: a -> Bool

instance IsNano InfixOp where
  isNano OpLAnd = True -- @&&@
  isNano OpLOr  = True -- @||@
  isNano OpLEq   = True -- @<@
  isNano OpGEq   = True -- @>@
  isNano OpSub  = True -- @-@
  isNano OpAdd  = True -- @+@
  isNano OpMul  = True -- @*@
  isNano OpEq   = True -- @==@
  isNano OpNEq  = True -- @!=@
  isNano _      = False

instance IsNano (LValue a) where 
  isNano (LVar _ _) = True
  isNano _          = False

instance IsNano AssignOp where
  isNano OpAssign = True
  isNano x        = error $ show $ text  "Not a Nano AssignOp!" <+> prettyPrint x

instance IsNano (Expression a) where 
  isNano (BoolLit _ _)         = True
  isNano (IntLit _ _)          = True
  isNano (VarRef _ _)          = True
  isNano (InfixExpr _ o e1 e2) = isNano o && isNano e1 && isNano e2
  isNano e                     = error $ show $ text "Not Nano Expression!" <+> prettyPrint e 

instance IsNano (Statement a) where
  isNano (EmptyStmt _)         = True                   -- skip
  isNano s
    | isSpecification s        = True
  isNano (ExprStmt _ e)        = isNanoExprStatement e  -- x = e
  isNano (BlockStmt _ ss)      = isNano ss              -- sequence
  isNano (IfSingleStmt _ b s)  = isNano b && isNano s   
  isNano (IfStmt _ b s1 s2)    = isNano b && isNano s1 && isNano s2
  isNano (WhileStmt _ b s)     = isNano b && isNano s
  isNano (VarDeclStmt _ ds)    = all isNano ds 
  isNano e                     = error $ show $ text "Not a Nano Statement!" <+> prettyPrint e

instance IsNano a => IsNano (Maybe a) where 
  isNano (Just x) = isNano x
  isNano Nothing  = True

instance IsNano [Statement a] where 
  isNano = all isNano

isNanoExprStatement :: Expression a -> Bool
isNanoExprStatement (AssignExpr _ o lv e) = isNano o && isNano lv && isNano e 
isNanoExprStatement e                     = error $ show $ text "Not Nano ExprStmt!" <+> prettyPrint e

instance IsNano (VarDecl a) where
  isNano (VarDecl _ _ (Just e)) = isNano e
  isNano (VarDecl _ _ Nothing)  = True

------------------------------------------
toNanoBexp :: Expression a -> BExp
------------------------------------------
toNanoBexp (BoolLit _ b) = Bool b

toNanoBexp (InfixExpr _ OpLAnd e1 e2) = And bexp1 bexp2
  where 
    bexp1 = toNanoBexp e1 
    bexp2 = toNanoBexp e2 

toNanoBexp (InfixExpr _ OpLOr e1 e2) = Or bexp1 bexp2
  where 
    bexp1 = toNanoBexp e1 
    bexp2 = toNanoBexp e2

toNanoBexp (InfixExpr _ OpLEq e1 e2) = Lte exp1 exp2
  where 
    exp1 = toNanoExp e1 
    exp2 = toNanoExp e2

toNanoBexp (InfixExpr _ OpGEq e1 e2) = Gte exp1 exp2
  where 
    exp1 = toNanoExp e1 
    exp2 = toNanoExp e2

toNanoBexp (InfixExpr _ OpEq e1 e2) = Eq exp1 exp2
  where 
    exp1 = toNanoExp e1 
    exp2 = toNanoExp e2

toNanoBexp (InfixExpr _ OpNEq e1 e2) = Neq exp1 exp2
  where 
    exp1 = toNanoExp e1 
    exp2 = toNanoExp e2

------------------------------------------
toNanoExp :: Expression a -> Exp
------------------------------------------
toNanoExp (IntLit _ n)  = Num $ toInteger n

toNanoExp (VarRef _ (Id _ s))  = Var s

toNanoExp (InfixExpr _ OpAdd e1 e2)  = Plus e1n e2n
  where
    e1n = toNanoExp e1
    e2n = toNanoExp e2

toNanoExp (InfixExpr _ OpSub e1 e2)  = Minus e1n e2n
  where
    e1n = toNanoExp e1
    e2n = toNanoExp e2

toNanoExp (InfixExpr _ OpMul e1 e2)  = Times e1n e2n
  where
    e1n = toNanoExp e1
    e2n = toNanoExp e2

------------------------------------------
toNanoVarDcl :: VarDecl a -> Stmt
------------------------------------------
toNanoVarDcl (VarDecl _ (Id _ s) (Just e)) = Assign s en 
  where
    en = toNanoExp e 

------------------------------------------
toNanoStmt :: Statement a -> Stmt
------------------------------------------
toNanoStmt (EmptyStmt _) = Skip

toNanoStmt (ExprStmt _ (AssignExpr _ OpAssign (LVar _ s) e)) = Assign s en 
  where
    en = toNanoExp e 

toNanoStmt (VarDeclStmt _ ss) = SeqList ssn
  where
    ssn = map toNanoVarDcl ss

toNanoStmt (BlockStmt _ ss) = SeqList ssn
  where
    ssn = map toNanoStmt ss  

toNanoStmt (IfSingleStmt _ b s) = If bn sn Skip
  where 
    bn = toNanoBexp b
    sn = toNanoStmt s

toNanoStmt (IfStmt _ b s1 s2) = If bn s1n s2n 
  where 
    bn = toNanoBexp b
    s1n = toNanoStmt s1
    s2n = toNanoStmt s2

toNanoStmt (WhileStmt _ b s) = While ps bn sn
  where
    bn = toNanoBexp b
    sn = toNanoStmt s
    ps = getPredicates s

toNanoStmt s 
  | isAssume s = Assume $ fromJust $ getAssume s
  | isAssert s = Assert $ fromJust $ getAssert s
  | isPred s    = Skip


---------------------------------------------------------------------
mkNano :: [Statement SourcePos] -> Maybe [Stmt]
---------------------------------------------------------------------
mkNano smts =  fmap concat $ sequence $ map mkNanoFun smts 

---------------------------------------------------------------------
mkNanoFun :: Statement a -> Maybe [Stmt]
---------------------------------------------------------------------
mkNanoFun (FunctionStmt l f xs body) 
  | all isNano body = Just $ nano
  | otherwise       = Nothing
  where 
    nano = map toNanoStmt body

mkNanoFun s             = error $ show $ text "Conversion error" <+> prettyPrint s

-----------------------------------------------------------------------------------
-- | Helpers for extracting specifications from @ECMAScript3@ @Statement@ 
-----------------------------------------------------------------------------------

-- Ideally, a la JML, we'd modify the parser to take in annotations for 
-- 
--   * assert(p)
--   * assume(p)
--   * invariant(p) 
--
-- For now, we hack them with function calls.

isSpecification :: Statement a -> Bool
isSpecification s  = not $ null $ catMaybes $ ($ s) <$> specs 
  where 
    specs          = [getAssert, getAssume, getInv, getPred]

getInvariant :: Statement a -> L.Base
getInvariant = getSpec getInv . flattenStmt

flattenStmt (BlockStmt _ ss) = concatMap flattenStmt ss
flattenStmt s                = [s]

getPredicates :: Statement a -> [L.Base]
getPredicates stmt = catMaybes $ map getPred (flattenStmt stmt)

getAssume    :: Statement a -> Maybe L.Base
getAssume    = getStatementPred "assume"

isAssume     :: Statement a -> Bool 
isAssume     =  isJust . getAssume

getAssert    :: Statement a -> Maybe L.Base 
getAssert    = getStatementPred "assert"

isAssert     :: Statement a -> Bool 
isAssert     =  isJust . getAssert

isPred     :: Statement a -> Bool 
isPred     =  isJust . getPred

getPred    :: Statement a -> Maybe L.Base 
getPred    = getStatementPred "pred"

-- -- getRequires  = getStatementPred "requires"
-- -- getEnsures   = getStatementPred "ensures"
getInv       = getStatementPred "invariant"

isInv     :: Statement a -> Bool 
isInv     =  isJust . getInv

getStatementPred :: String -> Statement a -> Maybe L.Base
getStatementPred name (ExprStmt _ (CallExpr _ (VarRef _ (Id _ f)) [p]))
  | name == f = Just $ bexpToBase $ toNanoBexp p
getStatementPred _ _ = Nothing 

getSpec   :: (Statement a -> Maybe L.Base) -> [Statement a] -> L.Base
getSpec g stmts =  L.And $ catMaybes $ map g stmts


---------------------------------------------------------------------
-- | Command Line Configuration Options
---------------------------------------------------------------------

data Config = Config { 
    files   :: [FilePath]     -- ^ source files to check
  } deriving (Data, Typeable, Show, Eq)

---------------------------------------------------------------------
-- | Top-level Parser 
---------------------------------------------------------------------

parseNanoFromFile :: String -> IO [Stmt]
parseNanoFromFile f = 
  do  s <- fmap unJavaScript $ parseFromFile f
      return $ fromMaybe err (mkNano s)
  where
      err  = error $ show $ text "Invalid Input File"

-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
    --s <- parseJavaScriptFromFile "tests/pos/inc0.js"
    --putStr $ "Parsed the file :  " ++ (show s)
    stmt <- parseNanoFromFile "tests/pos/while5.js"
    putStr $ "Parsed the file :  " ++ (show stmt)
    --putStrLn "Parsed the file : "
    -- TODO
    
    
