{-# LANGUAGE RecordWildCards #-}

module Satchmo.Solver.Yices

( solve, solveW
)

where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Monoid
import Satchmo.Data
import qualified Satchmo.Solve
import Satchmo.Solver.Internal
import Satchmo.SAT
import qualified Satchmo.SAT.Weighted as Weighted

import Control.Exception
import System.IO
import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve yices
solveW maxW = Satchmo.Solve.solveW maxW yicesW

yices :: Satchmo.Solve.Implementation
yices cs Header{numVars=numVars, numClauses=numClauses} = do
    let header = mkDimacsHeader numVars numClauses
    let debug = False
    if debug
       then BS.hPut stderr cs
       else hPutStrLn stderr header >> hFlush stderr
    ( code, stdout, stderr ) <- readProcessWithExitCodeBS "yices" ["-e","-d"] (BS.pack header `mappend` cs)
    when debug $ hPutStrLn System.IO.stderr stdout
    when (not $ null stderr) $ putStrLn stderr
    case lines stdout of
        "sat" : xs : _ -> return $ Just $ M.fromList $ do
            x <- map read $ words xs
            let l = literal $ abs x
            return ( l, x > 0 )
        _ -> return $ Nothing

yicesW :: Satchmo.Solve.WeightedImplementation
yicesW cs h = do
    let header = mkMaxWalkSatDimacsHeader h
    let debug = False
    if debug
       then BS.hPut stderr cs
       else hPutStrLn stderr header >> hFlush stdout
    ( code, stdout, stderr ) <- readProcessWithExitCodeBS "yices" ["-e","-d","-ms"] (BS.pack header `mappend` cs)
    when debug $ hPutStrLn System.IO.stderr stdout
    when (not $ null stderr) $ putStrLn stderr
    case lines stdout of
        "sat" : xs : _ -> return $ Just $ M.fromList $ do
            x <- map read $ words xs
            let l = literal $ abs x
            return ( l, x > 0 )
        _ -> return $ Nothing

mkMaxWalkSatDimacsHeader Weighted.Header{..}
  = "p wcnf " ++ show numVars ++ " " ++ show numClauses ++ " " ++ show maxWeight