{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Horn.CmdLine (getOpts) where

import Horn.Nano.Nano                      (Config (..))
import System.Console.CmdArgs

---------------------------------------------------------------------------------
-- | Parsing Command Line -------------------------------------------------------
---------------------------------------------------------------------------------

config = Config { 
   files   = def &= typ "TARGET" 
                 &= args 
                 &= typFile  
 } &= verbosity
   &= program "nano-horn" 
   &= help    "The Nano Software Verification System" 
   &= summary "nano-horn" 
   &= details [ "nano-js is suite of toy program verifiers"
              , ""
              , "To check a file foo.js, type:"
              , "  nano-horn foo.js "
              ]

getOpts :: IO Config 
getOpts = do md <- cmdArgs config 
             putStrLn $ banner md
             return   $ md

banner args =  "nano-horn\n" 
            ++ "nano-horn" ++ show args ++ "\n" 