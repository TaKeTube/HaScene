{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module HaScene
  (
  -- Game state modifiers
    initGame
  , timeStep
  -- Game state handlers
  , execTetris
  , evalTetris
  , move
  , Game(..)
  , HaScene
  , Direction(..)
  -- Lenses
  , camera, objects, initFile
  -- Constants
  , boardHeight, boardWidth
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             (forM_, mfilter, when, (<=<))
import           Control.Monad.IO.Class    (MonadIO (..), liftIO)
import           Prelude                   hiding (Left, Right)

import           Control.Lens              hiding (Empty)
import           Control.Monad.Trans.State (StateT (..), evalStateT, execStateT,
                                            gets)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Sequence             (Seq (..), (><))
import qualified Data.Sequence             as Seq
import           Linear.V3
import qualified Linear.V3                 as LV
import           System.Random             (getStdRandom, randomR)
-- Types and instances


-- | Coordinates
type Coord = V3 Double

-- | Triangle in location context
type Triangle = V3 Coord

newtype Object = Object
  { _triangles :: [Triangle]
  }
  deriving (Eq, Show)
makeLenses ''Object

data Camera = Camera
  { _pos   :: Coord
  , _angle :: Coord
  }
  deriving (Eq, Show)
makeLenses ''Camera

data Direction = Left | Right | Back | Forward
  deriving (Eq, Show)

-- | Game state
data Game = Game
  { _objects  :: [Object]
  , _camera   :: Camera
  , _initFile :: String
  }
  deriving (Eq, Show)
makeLenses ''Game

type HaSceneT = StateT Game
type HaScene a = forall m. (Monad m) => HaSceneT m a

evalTetris :: HaScene a -> Game -> a
evalTetris m = runIdentity . evalStateT m

execTetris :: HaScene a -> Game -> Game
execTetris m = runIdentity . execStateT m

-- Translate class for direct translations, without concern for boundaries
-- 'shift' concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s
  translate = translateBy 1
  translateBy :: Double -> Direction -> s -> s

instance Translatable Coord where
  translateBy n Left (V3 x y z)    = V3 (x-n) y z
  translateBy n Right (V3 x y z)   = V3 (x+n) y z
  translateBy n Back (V3 x y z)    = V3 x (y-n) z
  translateBy n Forward (V3 x y z) = V3 x (y-n) z

instance Translatable Camera where
  translateBy n d b =
    b & pos %~ translateBy n d

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

defaultScene :: String -> [Object]
defaultScene filename = []

defaultCamera :: Camera
defaultCamera = Camera
  {
    _pos = V3 1 1 1
  , _angle = V3 1 1 1
  }

-- | Initialize a game with a given level
initGame :: String-> IO Game
initGame filename = do

  pure $ Game
    { _objects      = defaultScene filename
    , _camera       = defaultCamera
    , _initFile     = "temp"
    }

-- | The main game execution, this is executed at each discrete time step
timeStep :: MonadIO m => HaSceneT m ()
timeStep = do
  move Forward
  return ()

move :: Direction -> HaScene ()
move dir = do
  c <- use camera
  let candidate = translate dir c
  camera .= candidate
