{-# language PatternSignatures #-}

module Satchmo.Solver.Minisat 

( solve
, using
)

where

import Satchmo.Data
import qualified Satchmo.Solve
import Satchmo.Solver.Internal
import Satchmo.SAT

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS
-- import qualified Data.ByteString.Lazy as BS

import Data.Monoid
import System.IO (stderr, hFlush, hPutStrLn, hGetContents)
import System.Process
import Control.Monad ( when )
import Control.Concurrent
import Control.Exception

import qualified Data.Map as M

solve = using "minisat"

using fp = Satchmo.Solve.solve $ minisat fp

minisat :: FilePath -> Satchmo.Solve.Implementation
minisat fp cs Header{numVars=numVars, numClauses=numClauses} = do
    let header = mkDimacsHeader numVars numClauses
        cs'    = BS.pack header `mappend` cs
    let debug = False
    if False 
       then BS.hPut stderr cs'
       else hPutStrLn stderr header >> hFlush stderr


    ( hin, hout, herr, proc ) <- 
        System.Process.runInteractiveCommand
            $ unwords [ fp, "/dev/stdin", "/dev/stdout" ] 

    container <- newEmptyMVar
    forkIO $ do 
        -- BS.hPut hin cs' 
        BS.hPut hin cs
        -- waitForProcess proc
        ds <- hGetContents hout
        hPutStrLn stderr $ unwords [ "output", "length", show ( length ds ) ]
        putMVar container ds

    out <- takeMVar container `Control.Exception.catch` \ ( _ :: AsyncException ) ->  do 
        -- hPutStrLn stderr "got exception in waitForProcess"
        terminateProcess proc
        -- hPutStrLn stderr "terminateProcess done"
        return ""
    -- hPutStrLn stderr "end waitForProcess"

    case lines out of
        "SAT" : xs : _ -> do
           let dict = Just $ M.fromList $ do
                            l <- map read $ takeWhile ( /= "0" ) $ words xs
                            return ( variable l, positive l )
           when debug $ print dict
           return dict
        _ -> return $ Nothing

