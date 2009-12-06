module Satchmo.Solver.Qube

( solve
)

where

import Satchmo.Data
import qualified Satchmo.Solve
import Satchmo.Solver.Internal
import Satchmo.SAT

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Monoid
import System.IO (stderr, hPutStrLn)
import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve qube

qube :: Satchmo.Solve.Implementation
qube cs Header{numVars=numVars, numClauses=numClauses} = do
    let header = mkDimacsHeader numVars numClauses
    let debug = True
    if debug 
       then BS.hPut stderr cs
       else hPutStrLn stderr header
    ( code, stdout, stderr ) <- 
        readProcessWithExitCodeBS "QuBE6.5" [ "/dev/stdin" ] (BS.pack header `mappend` cs)
    when debug $ hPutStrLn System.IO.stderr stdout
    let 
    case filter ( \ ws -> take 1 ws /= [ "c" ] ) $ map words $ lines stdout of
        [ "s", "cnf", "1" ] : rest -> 
            return $ Just $ M.fromList $ do
                "v" : vars <- rest
                l <- map read $ takeWhile ( /= "0" ) vars
                return ( variable l, positive l )
        _ -> return $ Nothing

