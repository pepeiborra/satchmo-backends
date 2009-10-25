module Satchmo.Solver.Minisat 

( solve
)

where

import Satchmo.Data
import qualified Satchmo.Solve

import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve minisat

minisat :: Satchmo.Solve.Implementation
minisat cs = do
    let debug = True
    if debug 
       then putStrLn cs
       else putStrLn $ head $ lines cs
    ( code, stdout, stderr ) <- 
        readProcessWithExitCode "minisat" [ "/dev/stdin", "/dev/stdout" ] cs
    when debug $ putStrLn stdout
    case lines stdout of
        "SAT" : xs : _ -> return $ Just $ M.fromList $ do
            x <- takeWhile ( /= 0 ) $ map read $ words xs
            let l = literal $ abs x
            return ( l, x > 0 )
        _ -> return $ Nothing

