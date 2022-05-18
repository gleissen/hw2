module Main where
import qualified Horn.Bindings.Z3          as HZ3
import qualified Horn.Fixpoint.Fixpoint    as Fix
import qualified Horn.Logic.Clauses        as HC
import qualified Horn.Nano.Nano        as Nano
import qualified Horn.HornVCs.HornVCs  as HornVCs
import qualified Horn.Logic.Clauses as Logic
import           Z3.Monad
import System.Exit  
import Horn.CmdLine                    

----------------------------------------
resultExit :: [Bool] -> ExitCode
----------------------------------------
resultExit rs 
    | and rs = ExitSuccess
resultExit _ = ExitFailure 1

main :: IO ()
main = do
    cfg <-  getOpts  
    rs <- mapM HornVCs.verifyFile $ Nano.files cfg
    exitWith (resultExit rs)
    --HornVCs.test  -- uncomment to test Horn clause generation
    --Logic.test    -- uncomment to test normalization
    --Fix.test      -- uncomment to test Horn clause solving
