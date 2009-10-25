module Satchmo.Solver.Qube

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

solve = Satchmo.Solve.solve qube

qube :: Satchmo.Solve.Implementation
qube cs = do
    let debug = True
    if debug 
       then BS.putStrLn cs
       else BS.putStrLn $ head $ BS.lines cs
    ( code, stdout, stderr ) <- 
        readProcessWithExitCodeBS "QuBE6.5" [ "/dev/stdin" ] cs
    when debug $ putStrLn stdout
    let 
    case filter ( \ ws -> take 1 ws /= [ "c" ] ) $ map words $ lines stdout of
        [ "s", "cnf", "1" ] : rest -> 
            return $ Just $ M.fromList $ do
                "v" : vars <- rest
                x <- takeWhile ( /= 0 ) $ map read vars
                let l = literal $ abs x
                return ( l, x > 0 )
        _ -> return $ Nothing

