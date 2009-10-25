module Satchmo.Solver.Minisat 

( solve
)

where

import Satchmo.Data
import qualified Satchmo.Solve
import Satchmo.Solver.Internal

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS

import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve minisat

minisat :: Satchmo.Solve.Implementation
minisat cs = do
    let debug = False
    if False 
       then BS.putStrLn cs
       else BS.putStrLn $ head $ BS.lines cs
    ( code, stdout, stderr ) <- 
        readProcessWithExitCodeBS  "minisat" [ "/dev/stdin", "/dev/stdout" ] cs
    when debug $ putStrLn stdout
    case lines stdout of
        "SAT" : xs : _ -> return $ Just $ M.fromList $ do
            x <- takeWhile ( /= 0 ) $ map read $ words xs
            let l = literal $ abs x
            return ( l, x > 0 )
        _ -> return $ Nothing

