module Horn.Bindings.Z3 where
import           Control.Applicative
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           Debug.Trace
import qualified Horn.Logic.Clauses  as Logic
import qualified Z3.Monad            as Z3

------------------------------------------------
implies :: Logic.Base -> Logic.Base -> IO Bool
------------------------------------------------
implies p q = do
    model <- get_model (Logic.And [p, Logic.Neg q]) vars
    --putStrLn $ "checking if " ++ (show p) ++ "implies " ++ (show q) ++ " :" ++ (show model)
    case model of
      Nothing -> return True
      Just _  -> return False
    where
      vars = Set.toList $ Set.union (Logic.get_vars p) (Logic.get_vars q)

-----------------------------------------------------------------
get_model :: Logic.Base -> [Logic.Exp] -> IO (Maybe [Integer])
-----------------------------------------------------------------
get_model phi vs = Z3.evalZ3 $ get_model_ phi vs

-----------------------------------------------------------------
get_model_ :: Logic.Base -> [Logic.Exp] -> Z3.Z3 (Maybe [Integer])
-----------------------------------------------------------------
get_model_ phi vs = do
  vars  <- mkVars vs
  let varMap = Map.fromList $ zip vs vars
  phiz3 <- toZ3 varMap phi
  Z3.assert phiz3
  model <- fmap snd $ (Z3.withModel $ \m -> (catMaybes <$> (mapM (Z3.evalInt m) vars)))
  return $ model

----------------------------------------------------------------
toZ3 :: Map.Map Logic.Exp Z3.AST -> Logic.Base -> Z3.Z3 Z3.AST
----------------------------------------------------------------
toZ3 varMap (Logic.Eq e1 e2) = do
      e1' <- toZ3Exp varMap e1
      e2' <- toZ3Exp varMap e2
      Z3.mkEq e1' e2'

toZ3 varMap (Logic.Geq e1 e2) = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkGe e1' e2'

toZ3 varMap (Logic.Leq e1 e2) = do
  e1' <- toZ3Exp varMap e1
  e2' <- toZ3Exp varMap e2
  Z3.mkGe e2' e1'

toZ3 varMap (Logic.Neg e) = do
    e' <- toZ3 varMap e
    Z3.mkNot e'

toZ3 varMap (Logic.And es) = do
    es' <- mapM (toZ3 varMap) es
    Z3.mkAnd es'

toZ3 varMap (Logic.Or es) = do
    es' <- mapM (toZ3 varMap) es
    Z3.mkOr es'

toZ3 varMap (Logic.Implies e1 e2) = do
    e1' <- toZ3 varMap e1
    e2' <- toZ3 varMap e2
    Z3.mkImplies e1' e2'

toZ3 varMap (Logic.Tr) = do
    Z3.mkTrue

toZ3 varMap (Logic.Fl) = do
    Z3.mkFalse    

-----------------------------------------------------------------
toZ3Exp :: Map.Map Logic.Exp Z3.AST -> Logic.Exp ->   Z3.Z3 Z3.AST
-----------------------------------------------------------------
toZ3Exp varMap v@(Logic.Var _) = return $ fromJust $ Map.lookup v varMap

toZ3Exp varMap (Logic.Num n)         = Z3.mkInteger n

toZ3Exp varMap (Logic.Plus es)    = do
      es' <- mapM (toZ3Exp varMap) es
      Z3.mkAdd es'

toZ3Exp varMap (Logic.Minus es)    = do
      es' <- mapM (toZ3Exp varMap) es
      Z3.mkSub es'

toZ3Exp varMap (Logic.Times es)    = do
      es' <- mapM (toZ3Exp varMap) es
      Z3.mkMul es'

mkVar :: Logic.Exp -> Z3.Z3 Z3.AST
mkVar (Logic.Var x) = Z3.mkFreshIntVar x

mkVars :: [Logic.Exp] -> Z3.Z3 [Z3.AST]
mkVars vs = mapM mkVar vs

test = do
    let bd = Logic.And [  Logic.Geq (Logic.Var "x")(Logic.Var "y"),  Logic.Geq (Logic.Var "x")(Logic.Num 2)]
    let phi =  Logic.And [bd, Logic.Neg (Logic.Geq (Logic.Var "x")(Logic.Num 0))]
    let vars = [Logic.Var "x",Logic.Var "y"]
    putStrLn (show $ Set.toList $ Logic.get_vars phi)
    (get_model phi (Set.toList $ Logic.get_vars phi))
