 {-# LANGUAGE OverloadedStrings #-}
module Horn.HornVCs.HornVCs where

import           Control.Monad.State.Strict
import qualified Horn.Nano.Nano as Nano
import qualified Horn.Logic.Clauses as Logic
import qualified Horn.Bindings.Z3 as Z3
import           Debug.Trace
import           Control.Monad.Trans.Class  (lift)
import           Data.Foldable (foldrM)
import           Rainbow
import           Data.Maybe
import qualified Horn.Monad as Monad
import           Horn.Fixpoint.Fixpoint as Fix
import qualified Data.Set    as Set
import           Data.Set    (Set)
import Data.Function ((&))

data VCState = VCS { vc :: [Logic.Horn [Logic.Base]], nx :: Int, chk :: [Logic.Bound] } deriving (Show)
type VCM = StateT VCState IO

-----------------------------------------------------------------------------------
subst :: Logic.Exp -> Logic.Var -> Logic.Query -> Logic.Query
-----------------------------------------------------------------------------------
subst e x query = error "TODO: FILL THIS IN" 

-----------------------------------------------------------------------------------
generateStmtVC :: Nano.Stmt -> Logic.Query -> VCM Logic.Query
-----------------------------------------------------------------------------------
generateStmtVC Nano.Skip post = error "TODO: FILL THIS IN" 

generateStmtVC (Nano.Assign x e) post = error "TODO: FILL THIS IN" 

generateStmtVC (Nano.Seq s1 s2) post = error "TODO: FILL THIS IN" 

generateStmtVC (Nano.SeqList stms) post = error "TODO: FILL THIS IN" 

generateStmtVC (Nano.If b s1 s2) post = error "TODO: FILL THIS IN" 

generateStmtVC (Nano.While ps b s) post = error "TODO: FILL THIS IN" 
-- addVC $ Logic.Horn {Logic.hd = ??, Logic.base=??, Logic.bd = ??, Logic.annot=ps}

generateStmtVC (Nano.Assert phi) post = error "TODO: FILL THIS IN" 

generateStmtVC (Nano.Assume phi) post = error "TODO: FILL THIS IN" 


-----------------------------------------------------------------------------------
getHornVCs :: Nano.Stmt -> IO ([Logic.Horn [Logic.Base]], [Logic.Bound]) 
-----------------------------------------------------------------------------------
getHornVCs s = do
        let vs = Set.toList $ Nano.getVars s
        let post = Logic.Query { Logic.name = "post", Logic.vars = vs}
        res <- runStateT (generateStmtVC s post) initState
        let lower = Logic.Horn {Logic.hd = fst res, Logic.base = Logic.Tr, Logic.bd = [], Logic.annot=[]}
        let horn = lower : (vc $ snd res)
        let bounds = chk $ snd res
        return (horn, bounds)
        
-------------------------------------------------------------------
isValid :: Logic.Base -> IO Bool
-------------------------------------------------------------------
isValid pre = do
         b <- Z3.implies (Logic.Tr) pre
         return b

-------------------------------------------------------------------
initState ::  VCState
-------------------------------------------------------------------
initState = VCS [] 1 []

-------------------------------------------------------------------
getVCs :: VCM [Logic.Horn [Logic.Base]]
-------------------------------------------------------------------
getVCs = do 
        st <- get
        return (vc st)

-------------------------------------------------------------------
addVC :: Logic.Horn [Logic.Base] -> VCM ()
-------------------------------------------------------------------
addVC c = do
        st <- get
        let vcs = c:(vc st)
        put VCS {vc = vcs, nx = nx st, chk = chk st}

-------------------------------------------------------------------
addBound :: Logic.Bound -> VCM ()
-------------------------------------------------------------------
addBound b = do
        st <- get
        let bounds = b:(chk st)
        put VCS {vc = vc st, nx = nx st, chk = bounds}

-------------------------------------------------------------------
nxQuery :: VCM ()
-------------------------------------------------------------------
nxQuery = do
        st <- get 
        let new = (nx st) + 1
        put VCS{vc = vc st, nx = new, chk = chk st}

-------------------------------------------------------------------
freshQuery :: [Logic.Var] -> VCM (Logic.Query)
-------------------------------------------------------------------
freshQuery vs = do
        st <- get
        let nm = "p" ++ (show $ nx st)
        nxQuery
        return Logic.Query{Logic.name = nm, Logic.vars = vs}


------------------------------------------------------
getQueries :: [Logic.Horn [Logic.Base]] -> Set Logic.Base
------------------------------------------------------
getQueries hs = Set.unions $ map (Set.fromList . Logic.annot) hs


-------------------------------------------
verifyFile :: FilePath ->  IO (Bool)
-------------------------------------------
verifyFile f = do
        stmts <- Nano.parseNanoFromFile f
        let prog = Nano.SeqList stmts
        putStr $ "Checking the file : " ++ (show f) -- ++ "\n" ++ (show stmts)
        let vs = Set.toList $ Nano.getVars prog
        --putStr $ "\nVars are:" ++ (show  vs)
        res <- getHornVCs prog
        --putStr $ "\nUnnormalized clauses " ++ (show (fst res)) 
        let norm = map Logic.normalize (fst res)
        let normBounds = map Logic.normalizeBound (snd res)
        --putStr $ "\nNormalized clauses " ++ (show norm) 
        --putStr $ "\nNormalized bounds " ++ (show normBounds) 
        let queries = Set.toList $ getQueries (fst res)
        --putStr $ "\nSolving clauses: " ++ (show norm) ++ "\n with queries: " ++ (show queries)
        sol <- evalStateT (Fix.solve norm queries vs) Monad.initState  
        --putStrLn $ "Solution: Bounds" ++ (show sol) 
        let phi = Logic.And $ map (Logic.pluginBound sol) normBounds
        --putStrLn $ "Checking: " ++ (show phi)
        let psi = Logic.And $ map (Logic.pluginHorn sol) norm
        res1 <- isValid phi
        --putStrLn $ "result: " ++ (show res1)
        --putStrLn $ "Checking, is this a solution" ++ (show psi)
        res2 <- isValid psi
        --putStrLn $ "result: " ++ (show res2)
        let res = res1 && res2
        printResult res
        --res <- checkVCs prog Logic.Tr Logic.Tr 
        --printResult res
        return res

-------------------------------------------
printResult :: Bool -> IO()
-------------------------------------------
printResult True = do
        putStr $ "Verification: "
        putChunkLn $ "passed"   & fore green

printResult False = do
        putStr $ "Verification: "
        putChunkLn $ "failed"  & fore red

-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
    --res <- verifyFile "tests/pos/max.js"  
    res <- getHornVCs (Nano.SeqList [init,pgm,post])
    --runStateT (generateStmtVC (Nano.SeqList [init,pgm]) post) initState
    putStr $ "Horn clauses: " ++ (show $ fst res) ++ "\n" 
    putStr $ "Bounds clauses: " ++ (show $ snd res) ++ "\n" 
    let norm = map Logic.normalize (fst res)
    putStr $ "Normalized clauses: " ++ (show $ norm) ++ "\n" 
    let normBounds = map Logic.normalizeBound (snd res)
    putStr $ "Normalized clauses: " ++ (show $ normBounds) ++ "\n"         
    putStr $ "Solving Normalized Clauses"
    sol <- evalStateT (Fix.solve norm preds [Logic.Var "x"]) Monad.initState     
    putStr $ "Solution" ++ (show sol)
    return ()
        where
        init = Nano.Assign "x" (Nano.Num 0)
        pgm = Nano.While preds (Nano.Lte (Nano.Var "x") (Nano.Num 5)) (Nano.Assign "x" (Nano.Plus (Nano.Var "x") (Nano.Num 1)))
        --pgm = Nano.Assume $ Logic.Geq (Logic.Var "x") (Logic.Num 0)
        post = Nano.Assert $ Logic.Eq (Logic.Var "x") (Logic.Num 6)
        preds = [(Logic.Leq (Logic.Var "x") (Logic.Num 6))]
    --return ()
    --where

      --a2 = Nano.Assign "x"  (Nano.Plus (Nano.Var "y") (Nano.Num 1))
      --a1 = Nano.Assign "y"  (Nano.Var "z")
      --pgm = Nano.SeqList [a1,a2]
      --post = Logic.Geq (Logic.Var "x") (Logic.Num 1)
      -- {True} if y<=0 then x:=1 else x:=y {x > 0}
      -- cond = Nano.Lte (Nano.Var "y") (Nano.Num 0)
      -- s1 = Nano.Assign "x" (Nano.Num 1)
      -- s2 = Nano.Assign "x" (Nano.Var "y")
      -- pgm = Nano.If cond s1 s2
      -- post = Logic.Geq (Logic.Var "x") (Logic.Num 0) 
      ---{x=0} while (I:=x=<6) x=<5 x=x+1 {x=6}
      --cond = Nano.Lte (Nano.Var "x") (Nano.Num 5)
      --s = Nano.Assign "x" (Nano.Plus (Nano.Var "x") (Nano.Num 1))
      --inv = Logic.Leq (Logic.Var "x") (Logic.Num 6)
      --pgm = Nano.While inv cond s 
      --post = Logic.Eq (Logic.Var "x") (Logic.Num 6) 
      --init = (Logic.Eq (Logic.Var "x") (Logic.Num 0))
