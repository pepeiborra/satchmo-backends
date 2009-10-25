module Satchmo.Solver.Yices

( solve
)

where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS
import Satchmo.Data
import qualified Satchmo.Solve
import Satchmo.Solver.Internal

import Control.Exception
import System.IO
import System.Process
import Control.Monad ( when )

import qualified Data.Map as M

solve = Satchmo.Solve.solve yices

yices :: Satchmo.Solve.Implementation
yices cs = do
    let debug = False
    if False
       then BS.putStrLn cs
       else BS.hPut stderr (head $ BS.lines cs) >> hFlush stdout
    ( code, stdout, stderr ) <- readProcessWithExitCodeBS "yices" ["-e","-d"] cs
    when debug $ putStrLn stdout
    when (not $ null stderr) $ putStrLn stderr
    case lines stdout of
        "sat" : xs : _ -> return $ Just $ M.fromList $ do
            x <- map read $ words xs
            let l = literal $ abs x
            return ( l, x > 0 )
        _ -> return $ Nothing
