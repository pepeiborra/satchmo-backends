{-# LANGUAGE RecordWildCards, NamedFieldPuns, PatternSignatures #-}

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

type Seconds = Int

solve  timeout = Satchmo.Solve.solve (yices timeout)
solveW timeout maxW = Satchmo.Solve.solveW maxW (yicesW timeout)

yices :: Maybe Seconds -> Satchmo.Solve.Implementation
yices timeout cs Header{numVars=numVars, numClauses=numClauses} = do
    let header = mkDimacsHeader numVars numClauses
    let debug = False
    if debug
       then BS.hPut stderr cs
       else hPutStrLn stderr header >> hFlush stderr

    let opts = ["-e","-d"] ++ maybe [] (\t -> ["-tm", show t]) timeout

    ( code, stdout, stderr ) <- readProcessWithExitCodeBS "yices" opts (BS.pack header `mappend` cs)
    when debug $ hPutStrLn System.IO.stderr stdout
    when (not $ null stderr) $ putStrLn stderr
    case lines stdout of
        "sat" : xs : _ -> return $ Just $ M.fromList $ do
            l :: Literal <- map read $ words xs
            return ( variable l, positive l )
        _ -> return $ Nothing

yicesW :: Maybe Seconds -> Satchmo.Solve.WeightedImplementation
yicesW timeout cs h@Weighted.Header{Weighted.maxWeight=maxWeight} = do
    let header = mkMaxWalkSatDimacsHeader h
    let debug = False
    if debug
       then BS.hPut stderr cs
       else hPutStrLn stderr header >> hFlush stdout

    let opts =  ["-e","-d","-ms", "-mw", show maxWeight] ++ maybe [] (\t -> ["-tm", show t]) timeout

    ( code, stdout, stderr ) <- readProcessWithExitCodeBS "yices" opts (BS.pack header `mappend` cs)
    when debug $ hPutStrLn System.IO.stderr stdout
    when (not $ null stderr) $ putStrLn stderr
    case lines stdout of
        "sat" : xs : _ -> return $ Just $ M.fromList $ do
            l <- map read $ words xs
            return ( variable l, positive l )
        _ -> return $ Nothing

mkMaxWalkSatDimacsHeader Weighted.Header{..}
  = "p wcnf " ++ show numVars ++ " " ++ show numClauses ++ " " ++ show maxWeight