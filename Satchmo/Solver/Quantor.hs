module Satchmo.Solver.Quantor

( solve
)

where

import Satchmo.Data
import qualified Satchmo.Solve
import Satchmo.Solver.Internal
import Satchmo.SAT

import Data.Monoid
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO  (stderr, hFlush, hPutStrLn)
import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve quantor

quantor :: Satchmo.Solve.Implementation
quantor cs Header{numVars=numVars, numClauses=numClauses} = do
    let header = mkDimacsHeader numVars numClauses
        cs'    = BS.pack header `mappend` cs
    let debug = True
    if debug 
       then BS.hPut stderr cs'
       else hPutStrLn stderr header >> hFlush stderr
    ( code, stdout, stderr ) <- 
        readProcessWithExitCodeBS "quantor" [ "-v", "--resolve-exported=0" ] cs'
    when debug $ hPutStrLn System.IO.stderr stdout
    let 
    case filter ( \ ws -> take 1 ws /= [ "c" ] ) $ map words $ lines stdout of
        [ "s", ok ] : rest | ok `elem` [ "TRUE", "SATISFIABLE" ] -> 
            return $ Just $ M.fromList $ do
                "v" : vars <- rest
                l <- map read $ takeWhile ( /= "0" ) vars
                return ( variable l, positive l  )
        _ -> return $ Nothing

