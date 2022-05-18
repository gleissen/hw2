module Horn.Fixpoint.Fixpoint where

import           Control.Monad              (filterM, foldM)
import           Control.Monad.State.Strict (evalStateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Debug.Trace
import qualified Horn.Bindings.Z3           as Z3
import           Horn.Logic.Clauses
import           Horn.Monad


data WorkItem = Item {state :: Base, query :: Name} deriving (Show,Eq,Ord)
type WorkSet  = Set WorkItem
type Predicate = Base

-- helper
---------------------------
pick :: WorkSet -> WorkItem
---------------------------
pick ws = head $ Set.toList ws

----------------------------------------------
post :: Base -> [Var] -> SolveM Base
----------------------------------------------
post phi vars = error "TODO: FILL THIS IN" 

---------------------------------------------
implies :: Base -> Base -> SolveM Bool
---------------------------------------------
implies p q = lift $ Z3.implies p q

---------------------------------------------
pred_abs :: Base -> [Predicate] -> SolveM [Base]
---------------------------------------------
pred_abs phi preds =  error "TODO: FILL THIS IN"

-------------------------------------------------------------------------------------
fixpoint_step :: [Predicate] -> Solution -> WorkItem -> Horn a -> SolveM WorkSet
-------------------------------------------------------------------------------------
fixpoint_step preds sol w h = error "TODO: FILL THIS IN"

------------------------------------------------------------------------------
fixpoint :: WorkSet -> Solution -> [Predicate] -> [Horn a] -> SolveM Solution
------------------------------------------------------------------------------
fixpoint ws sol preds hs = case (Set.size ws) of
    0 -> return sol
    _ -> do
      newWs <- Set.unions <$> mapM (fixpoint_step preds sol w) hs'
      let ws' = Set.union (ws Set.\\ Set.singleton w) newWs
      let sol' = updateSolution newWs sol
      -- lift $ putStrLn $ "Fixpoint loop: new workItems: " ++ (show ws')
      -- lift $ putStrLn $ "Fixpoint loop: new solution: " ++ (show sol')
      -- lift $ putStrLn $ "Clauses that get scheduled " ++ (show hs')
      fixpoint ws' sol' preds hs
    where
      w    = pick ws
      hs'  = filter (dependsOn (query w)) hs

--------------------------------------------------
updateSolution :: WorkSet -> Solution -> Solution
--------------------------------------------------
updateSolution ws sol = foldl update sol (Set.toList ws)
  where
    add st (vs,p) = (vs, Set.union p (Set.singleton $ st))
    update sol w = Map.adjust (add (state w)) (query w) sol

---------------------------------------------------------------
initWL_ :: [Predicate] -> Solution -> [Var] -> Horn a  -> SolveM (Solution, WorkSet)
---------------------------------------------------------------
initWL_ preds sol solVs h = do
      post_ <- post (base h) exVs
      absPost <-  pred_abs post_ instPreds
      -- lift $ putStrLn $ "dbg init clause " ++ (show h)
      -- lift $ putStrLn $ "dbg init preds " ++ (show instPreds)
      -- lift $ putStrLn $ "dbg init absPost " ++ (show absPost)
      let newState = map (substVars solVs vs) absPost
      let ws = Set.singleton $ Item {state=(And newState), query=(name $ hd h)}
      let sol' = updateSolution ws sol
      -- lift $ putStrLn $ "dbg init solution " ++ (show sol')
      -- lift $ putStrLn $ "dbg init ws " ++ (show ws)
      return (sol', ws)
    where
      vs = vars $ hd h
      instPreds = map (substVars vs solVs) preds
      exVs = Set.toList $ (get_vars (base h)) Set.\\ (Set.fromList $ vs)

----------------------------------------------------------------------------
initWL :: [Predicate] -> [Horn a] -> [Var] -> SolveM (Solution, WorkSet)
----------------------------------------------------------------------------
initWL preds hs vs = do
  sols <- mapM (initWL_ preds sol0 vs) hs'
  return $ foldl combine (Map.empty, Set.empty) sols
  where
    hs' = filter isBase hs
    combine (sol, ws) (sol', ws') = (Map.union sol sol', Set.union ws ws')
    queryNames = Set.toList $ Set.unions $ map getQueryNames hs
    sol0 = Map.fromList $ zip queryNames (repeat (vs, Set.singleton Fl))

----------------------------------------------------
solve :: [Horn a] -> [Predicate] -> [Var] -> SolveM Solution
----------------------------------------------------
solve hs preds vs = do
    (sol0, ws0) <- initWL preds hs vs
    fixpoint ws0 sol0 preds hs
    

--------------------------------------------
check :: Solution -> Horn a -> IO Bool
--------------------------------------------
check sol h = do
  let phi = And [base h, solBd, Neg solHd]
  --putStrLn $ "\n dbg solHd " ++ (show solHd)
  model <- Z3.get_model phi (Set.toList $ get_vars phi)
  case model of
      Nothing -> return True
      Just m  -> return False
  where
    solBd = And $ map (plugin sol) (bd h)
    solHd = plugin sol (hd h)


-------------------------------------------
test :: IO ()
-------------------------------------------
test = do
  putStrLn $ "Henlo"
    --let hs' = (filter isBase hs)
    --(sol,ws) <- evalStateT (initWL preds hs') initState
    --putStrLn $ "Initial Solution and worklist " ++ (show sol) ++ " " ++ (show ws)
    --  putStrLn $ "Solving clauses: "
    --  putStrLn ""
    --  mapM_ (\h -> putStrLn (show h)) hs
    --  putStrLn ""
    --  sol <- evalStateT (solve hs preds [Var "x"]) initState
    --  putStr $ "Found solution " ++ (show sol)
    -- where
    --   preds = [Geq (Var "x") (Num 0)]
    --   hs = [ Horn {      hd = Pred "h" [Var "x"]
    --               ,      bd = []
    --               ,      base = Eq (Var "x") (Num 0)
    --               ,      annot=()
    --        },
    --        Horn {    hd = Pred "h" [Var "x'"]
    --            ,      bd = [Pred "h" [Var "x"]]
    --            ,      base = Eq (Var "x'") (Plus [Var "x", Num 1])
    --            ,      annot=()},
    --         Horn {      hd = Pred "q" [Var "x'"]
    --                     ,      bd = [Pred "h" [Var "x"]]
    --                     ,      base = Eq (Var "x") (Var "x'")
    --                     ,      annot=()
    --         }
    --        ]
