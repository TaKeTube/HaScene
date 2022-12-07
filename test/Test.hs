
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.IORef
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.Exit
import           System.Process
import           System.IO
import           Control.Exception
import           Text.Printf
import           System.FilePath
import           Linear
import qualified Test.QuickCheck as QC
import qualified HaScene             as SC
-- import qualified CSE230.WhilePlus.Types as W
-- import qualified CSE230.WhilePlus.Eval  as W
import qualified Data.Map as M 


type Score = IORef (Int, Int)

main :: IO ()
main = runTests 
  [ 
    unitTest
  ]

unitTest ::  Score -> TestTree
unitTest sc = testGroup "WhilePlus" 
  [ 
    runTest (SC.eulerMatrix,  (V3 0 0 0), SC.indentityMatrix, 0, "Euler Matrix calc"),
    runTest (SC.eulerMatrix,  (V3 0 2 0), SC.mat020, 0, "Euler Matrix calc"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["v","0","0","0"]])),  (), ([(0.0,0.0,0.0)],[]), 0, "vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["v","1","0","1"]])),  (), ([(1.0,0.0,1.0)],[]), 0, "vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["v","0","0","0"],["v","1","0","1"]])),  (), ([(1.0,0.0,1.0),(0.0,0.0,0.0)],[]), 0, "multi vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["v","0","1","0"],["v","1","0","1"]])),  (), ([(1.0,0.0,1.0),(0.0,1.0,0.0)],[]), 0, "multi vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["v","0","1","0"],["v","1","0"]])),  (), ([(0.0,1.0,0.0)],[]), 0, "multi invalid vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["f","0","0","0","0"]])),  (), ([],[(0,0,0),(0,0,0)]), 0, "faces parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["f","1","2","3","4"]])),  (), ([],[(1,2,3),(1,3,4)]), 0, "faces parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["f","1","2","3","4"],["f","2","3","4","5"]])),  (), ([],[(2,3,4),(2,4,5),(1,2,3),(1,3,4)]), 0, "multi faces parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["f","1","2","3"]])),  (), ([],[(1,2,3)]), 0, "special face parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["v","1","0","1"],["f","1","2","3","4"]])),  (), ([(1.0,0.0,1.0)],[(1,2,3),(1,3,4)]), 0, "faces + vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["f","1","2","3","4"],["v","1","0","1"]])),  (), ([(1.0,0.0,1.0)],[(1,2,3),(1,3,4)]), 0, "faces + vertex parse"),
    runTest ((\_ -> (foldl SC.updatevf ([],[]) [["f","1","2","3","4"],["v","1","0"]])),  (), ([],[(1,2,3),(1,3,4)]), 0, "invalid faces + vertex parse")
    -- runTest (SC.readOBJ,  "src/models/test.obj", rst, 0, "read file")
  ]
  where
    runTest :: ( Eq b) => (a -> b, a, b, Int, String) -> TestTree
    runTest (f, x, r, n, msg) = runTest' sc (return . f, x, r, n, msg)


runTests :: [Score -> TestTree] -> IO ()
runTests groups = do
  sc <- initScore
  -- defaultMain (tests sc groups) `catch` (\(e :: ExitCode) -> do
  defaultMain (localOption (mkTimeout 1000000) (tests sc groups)) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- readIORef sc
    throwIO e)

tests :: Score -> [Score -> TestTree] -> TestTree
tests x gs = testGroup "Tests" [ g x | g <- gs ]

--------------------------------------------------------------------------------
-- | Construct a single test case
--------------------------------------------------------------------------------
mkTest' :: (Eq b) => Score -> (a -> IO b) -> a -> b -> String -> TestTree
-------------------------------------------------------------------------------
mkTest' sc f x r name = runTest' sc (f, x, r, 1, name)

--------------------------------------------------------------------------------
runTest' :: (Eq b) => Score -> (a -> IO b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
runTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    actR <- f x
    if actR == expR
      then updateCurrent sc points
      else assertFailure "(putStrLns(show actR))"
      -- else assertFailure show(actR)

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)

