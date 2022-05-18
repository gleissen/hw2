module Horn.Monad where
import           Control.Monad.State.Strict
import           Horn.Logic.Clauses


type SolveM = StateT SolverState IO
data SolverState = Solver {nx :: Int}

initState = Solver {nx = 0}

-------------------------------------------------------------------
getNx :: SolveM Int
-------------------------------------------------------------------
getNx = nx <$> get

-------------------------------------------------------------------
putNx :: Int -> SolveM ()
-------------------------------------------------------------------
putNx n = do
  put Solver {nx = n}

-------------------------------------------------------------------
freshVar :: SolveM Var
-------------------------------------------------------------------
freshVar = do
  n <- getNx
  putNx (n+1)
  return $ Var ("tmp" ++ (show n))

-------------------------------------------------------------------
freshVars :: Int -> SolveM [Var]
-------------------------------------------------------------------
freshVars k = do
  replicateM k freshVar
