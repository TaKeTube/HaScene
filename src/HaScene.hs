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
  , execHaScene
  , evalHaScene
  , eulerMatrix
  , move, rotate
  -- data structures
  , Game(..)
  , HaScene
  , Direction(..), RDirection(..)
  , Coord(..)
  , Triangle(..)
  , Mesh(..)
  , Camera(..)
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
import           Linear                    hiding (rotate)

-- Types and instances
eulerMatrix :: V3 Float -> M33 Float
eulerMatrix (V3 a b c) = let
    cosa = cos a
    sina = sin a
    cosb = cos b
    sinb = sin b
    cosc = cos c
    sinc = sin c
    ma = V3 (V3 1       0       0      )
            (V3 0       cosa    (-sina))
            (V3 0       sina    cosa   )
    mb = V3 (V3 cosb    0       sinb   )
            (V3 0       1       0      )
            (V3 (-sinb) 0       cosb   )
    mc = V3 (V3 cosc    (-sinc) 0      )
            (V3 sinc    cosc    0      )
            (V3 0       0       1      )
    in mc !*! mb !*! ma

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
  { _pos :: Coord
  , _dir :: Coord
  , _up  :: Coord
  }
  deriving (Eq, Show)
makeLenses ''Camera

data Direction = Left | Right | Back | Forward | Up | Down
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

evalHaScene :: HaScene a -> Game -> a
evalHaScene m = runIdentity . evalStateT m

execHaScene :: HaScene a -> Game -> Game
execHaScene m = runIdentity . execStateT m

-- Translate class for direct translations, without concern for boundaries
-- 'shift' concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> Coord -> s -> s
  translate = translateBy 0.05
  translateBy :: Float -> Direction -> Coord -> s -> s

  translateR :: RDirection -> s -> s
  translateR = translateRBy 0.01
  translateRBy :: Float -> RDirection -> s -> s


instance Translatable Coord where
  translateBy n Forward dir (V3 x y z)    =
    let
      dx = (dir ^. _x) * n
      dz = (dir ^. _z) * n
      in
    V3 (x+dx) y (z+dz)
  translateBy n Back dir c   = translateBy (-n) Forward dir c
  translateBy n Left dir (V3 x y z)    =
    let
      dx = - (dir ^. _z) * n
      dz = (dir ^. _x) * n
      in
    V3 (x+dx) y (z+dz)
  translateBy n Right dir c = translateBy (-n) Left dir c
  translateBy n Up _ (V3 x y z)        = V3 x (y+n) z
  translateBy n Down d c               = translateBy (-n) Up d c

  translateRBy n RLeft v3  = normalize $ transpose (eulerMatrix (V3 0 n    0)) !* v3
  translateRBy n RRight v3 = translateRBy (-n) RLeft v3
  translateRBy n RUp v3    = normalize $ eulerMatrix (V3 n    0    0) !* v3
  translateRBy n RDown v3 = translateRBy (-n) RUp v3

instance Translatable Camera where
  translateBy n op dir cam = cam & pos %~ translateBy n op dir
  translateRBy n d c = c & dir %~ translateRBy n d
                         & up  %~ translateRBy n d

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

defaultScene :: String -> IO [Mesh]
defaultScene filename = do
  obj1 <- buildMesh "src/models/hat.obj" "hat"
  obj2 <- buildMesh "src/models/cube.obj" "cube"
  return [obj1]

defaultCamera :: Camera
defaultCamera = Camera
  {
    _pos = V3 0 0 3
  , _dir = V3 0 0 (-1)
  , _up  = V3 0 1 0
  }

-- | Initialize a game with a given level
initGame :: String-> IO Game
initGame filename = do
  scene <- defaultScene filename
  pure $ Game
    { _objects      = scene
    , _camera       = defaultCamera
    , _initFile     = filename
    }

-- | The main game execution, this is executed at each discrete time step
timeStep :: MonadIO m => HaSceneT m ()
timeStep = do
  return ()

move :: Direction -> HaScene ()
move dir = do
  c <- use camera
  let candidate = translate dir (_dir c) c
  camera .= candidate

rotate :: RDirection -> HaScene ()
rotate dir = do
  c <- use camera
  let candidate = translateR dir c
  camera .= candidate

safeSelect :: [a] -> Int -> Maybe a
safeSelect [] _ = Nothing
safeSelect xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

buildMesh:: FilePath -> String -> IO Mesh
buildMesh filepath name= do
    (vsraw, fsraw) <- readOBJ filepath
    let vs = map f vsraw
        f (x, y, z) = V3 x y z
    let fs = map f2 fsraw
        f2 (x, y, z) = V3 (vs !! (x-1)) (vs !! (y-1)) (vs !! (z-1))
    return Mesh {_triangles=fs,_name=name}

split :: (Eq a) => a -> [a] -> [[a]]
split d xs = split' d (reverse xs)

split' :: (Eq a) => a -> [a] -> [[a]]
split' d xs = let
    addchar (l:ls) x
        | x == d  = []:l:ls
        | x /= d  = (x:l):ls
    addchar _ x = []
    in foldl addchar [[]] xs

updatevf :: ([(Float,Float,Float)],[(Int,Int,Int)]) -> [String] -> ([(Float,Float,Float)],[(Int,Int,Int)])
updatevf (vs,fs) ("v":sx:sy:sz:_) = ((read sx,read sy,read sz):vs,fs)
updatevf (vs,fs) ("f":fc) = let
    getv s = read $ head $ split '/' s
    idx = map getv fc
    tris = zip3 (repeat (head idx)) (drop 1 idx) (drop 2 idx) -- NOTE: Triangularization may not be correct for complex faces.
    in (vs,tris ++ fs)
updatevf (vs,fs) _ = (vs,fs)

readOBJ :: FilePath -> IO ([(Float,Float,Float)],[(Int,Int,Int)])
readOBJ fname = do
    contents <- readFile fname
    let lins    = map words (lines contents)
    let (vs,fs) = foldl updatevf ([],[]) lins
    return (reverse vs,fs)
