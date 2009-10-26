module Satchmo.Solver.Internal where

import Control.Exception
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO
import System.Process

readProcessWithExitCodeBS exec args input = do
        (hIn, hOut, hErr, hProc) <- runInteractiveProcess exec args Nothing Nothing
        try(BS.hPut hIn input) :: IO (Either SomeException ())
        stdout <- S.hGetContents hOut
        stderr <- hGetContents hErr
        code   <- waitForProcess hProc
        return (code, S.unpack stdout, stderr)

mkDimacsHeader numVars numClauses = "p cnf " ++ show numVars ++ " " ++ show numClauses