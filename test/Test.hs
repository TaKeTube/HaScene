
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
  [ unitTest
--   , genTest
  ]

unitTest ::  Score -> TestTree
unitTest sc = testGroup "WhilePlus" 
  [ scoreTest ((\_ -> SC.eulerMatrix (V3 0 0 0)),  (), SC.indentityMatrix, 10, "test-1")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

-- genTest :: Score -> TestTree
-- genTest sc = testGroup "BinSearchTree"
--   [ scoreProp sc ("prop_insert_bso"      , BST.prop_insert_bso     , 3) 
--   , scoreProp sc ("prop_insert_map"      , BST.prop_insert_map     , 4)
--   , scoreProp sc ("prop_delete_bso"      , BST.prop_delete_bso     , 6)
--   , scoreProp sc ("prop_delete_map"      , BST.prop_delete_map     , 6)
--   , scoreProp sc ("prop_genBalHeight"    , BST.prop_genBalHeight   , 2)
--   , scoreProp sc ("prop_genBalBalanced"  , BST.prop_genBalBalanced , 2)
--   , scoreProp sc ("prop_genBalBSO"       , BST.prop_genBalBSO      , 2)
--   ]



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
mkTest' :: (Show b, Eq b) => Score -> (a -> IO b) -> a -> b -> String -> TestTree
--------------------------------------------------------------------------------
mkTest' sc f x r name = scoreTest' sc (f, x, r, 1, name)

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> IO b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    actR <- f x
    if actR == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)

--------------------------------------------------------------------------------
scoreProp :: (QC.Testable prop) => Score -> (String, prop, Int) -> TestTree
--------------------------------------------------------------------------------
scoreProp sc (name, prop, n) = scoreTest' sc (act, (), True, n, name)
  where
    act _                    = QC.isSuccess <$> QC.labelledExamplesWithResult args prop
    args                     = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 100 }

--------------------------------------------------------------------------------
-- | Binary (Executable) Tests
--------------------------------------------------------------------------------
data BinCmd = BinCmd
  { bcCmd    :: String
  , bcInF    :: FilePath
  , bcExpF   :: FilePath
  , bcPoints :: Int
  , bcName   :: String
  } deriving (Show)

binTest :: Score -> BinCmd -> TestTree
binTest sc b  = scoreTest' sc (act, (), True, bcPoints b, bcName b)
  where act _ = mkBinTest (bcCmd b) (bcInF b) (bcExpF b)

mkBinTest execS inF expF = do
  hSetBuffering stdout LineBuffering -- or even NoBuffering
  withFile log WriteMode $ \h -> do
    (_,_,_,ph) <- createProcess $ (shell cmd) {std_out = UseHandle h, std_err = UseHandle h}
    c          <- waitForProcess ph
    expected   <- readFile expF
    actual     <- readFile log
    return (expected == actual)
  where
    log  = inF <.> "log"
    cmd  = printf "%s < %s" execS inF