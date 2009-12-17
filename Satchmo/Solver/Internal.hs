{-# LANGUAGE ScopedTypeVariables #-}

module Satchmo.Solver.Internal where

import Control.Concurrent
import Control.Exception as CE
import Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Exit
import System.IO
import System.Process

readProcessWithExitCodeBS exec args input = do
        outMVar <- newEmptyMVar
        errMVar <- newEmptyMVar
        (hIn, hOut, hErr, hProc) <- runInteractiveProcess exec args Nothing Nothing
        (`CE.catch` handleInterruption hProc) $ do
          _ <- forkIO (S.hGetContents hOut >>= putMVar outMVar)
          _ <- forkIO (S.hGetContents hErr >>= putMVar errMVar)
          when (not $ LBS.null input) $ LBS.hPut hIn input
          hClose hIn

          out <- takeMVar outMVar
          err <- takeMVar errMVar

          code <- waitForProcess hProc

          return (code, S.unpack out, S.unpack err)
  where
   handleInterruption hProc (e::AsyncException) = do
     terminateProcess hProc
     return (ExitFailure 1, "", "")


mkDimacsHeader numVars numClauses = "p cnf " ++ show numVars ++ " " ++ show numClauses