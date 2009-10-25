module Satchmo.Solver.Quantor

( solve
)

where

import Satchmo.Data
import qualified Satchmo.Solve

import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve quantor

quantor :: Satchmo.Solve.Implementation
quantor cs = do
    let debug = True
    if debug 
       then putStrLn cs
       else putStrLn $ head $ lines cs
    ( code, stdout, stderr ) <- 
        readProcessWithExitCode "quantor" [ "-v", "--resolve-exported=0" ] cs
    when debug $ putStrLn stdout
    let 
    case filter ( \ ws -> take 1 ws /= [ "c" ] ) $ map words $ lines stdout of
        [ "s", ok ] : rest | ok `elem` [ "TRUE", "SATISFIABLE" ] -> 
            return $ Just $ M.fromList $ do
                "v" : vars <- rest
                x <- takeWhile ( /= 0 ) $ map read vars
                let l = literal $ abs x
                return ( l, x > 0 )
        _ -> return $ Nothing

