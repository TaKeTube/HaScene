{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
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
  , move, rotate
  -- data structures
  , Game(..)
  , HaScene
  , Direction(..), RDirection(..)
  , Mesh(..)
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
type Coord = V3 Float

-- | Triangle in location context
type Triangle = V3 Coord

data Mesh = Mesh
  { _triangles :: [Triangle]
  , _name      :: String
  }
  deriving (Eq)
makeLenses ''Mesh

instance Show Mesh where
  show :: Mesh -> String
  show a = a ^. name

data Camera = Camera
  { _pos   :: Coord
  , _angle :: Coord
  }
  deriving (Eq, Show)
makeLenses ''Camera

data Direction = Left | Right | Back | Forward
  deriving (Eq, Show)
data RDirection = RLeft | RRight | RUp | RDown
  deriving (Eq, Show)

-- | Game state
data Game = Game
  { _objects  :: [Mesh]
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
  translateBy :: Float -> Direction -> s -> s

  translateR :: RDirection -> s -> s
  translateR = translateRBy 1
  translateRBy :: Float -> RDirection -> s -> s

instance Translatable Coord where
  translateBy n Left (V3 x y z)    = V3 (x-n) y z
  translateBy n Right (V3 x y z)   = V3 (x+n) y z
  translateBy n Back (V3 x y z)    = V3 x (y-n) z
  translateBy n Forward (V3 x y z) = V3 x (y+n) z
  translateRBy n RLeft (V3 x y z)  = V3 (x-n) y z
  translateRBy n RRight (V3 x y z) = V3 (x+n) y z
  translateRBy n RUp (V3 x y z)    = V3 x (y-n) z
  translateRBy n RDown (V3 x y z)  = V3 x (y+n) z

instance Translatable Camera where
  translateBy n d c = c & pos %~ translateBy n d
  translateRBy n d c = c & angle %~ translateRBy n d

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

defaultScene :: String -> [Mesh]
defaultScene filename = [
    Mesh {_triangles=[],_name="OBJ1"}
  , Mesh {_triangles=[],_name="OBJ2"}
  , Mesh {_triangles=[],_name="OBJ3"}
  ]

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
  return ()

move :: Direction -> HaScene ()
move dir = do
  c <- use camera
  let candidate = translate dir c
  camera .= candidate

rotate :: RDirection -> HaScene ()
rotate dir = do
  c <- use camera
  let candidate = translateR dir c
  camera .= candidate
