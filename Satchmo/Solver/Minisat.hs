module Satchmo.Solver.Minisat 

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
import System.IO (stderr, hFlush, hPutStrLn)
import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve minisat

minisat :: Satchmo.Solve.Implementation
minisat cs Header{numVars=numVars, numClauses=numClauses} = do
    let header = mkDimacsHeader numVars numClauses
        cs'    = BS.pack header `mappend` cs
    let debug = False
    if False 
       then BS.hPut stderr cs'
       else hPutStrLn stderr header >> hFlush stderr
    ( code, stdout, stderr ) <- 
        readProcessWithExitCodeBS  "minisat" [ "/dev/stdin", "/dev/stdout" ] cs
    when debug $ hPutStrLn System.IO.stderr stdout
    case lines stdout of
        "SAT" : xs : _ -> do
           let dict = Just $ M.fromList $ do
                            x <- takeWhile ( /= 0 ) $ map read $ words xs
                            let l = literal $ abs x
                            return ( l, x > 0 )
           when debug $ print dict
           return dict
        _ -> return $ Nothing

