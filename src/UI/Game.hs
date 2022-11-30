{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module UI.Game
  ( playGame
  ) where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Prelude                    hiding (Left, Right)

import           Brick                      hiding (Down)
import           Brick.BChan
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Lens               hiding (op, preview)
import           Control.Monad.Trans.State
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import qualified Graphics.Vty               as V
import           Linear.V3                  (V3 (..))

import           Data.Maybe                 (fromMaybe)
import           HaScene

data UI = UI
  { _game   :: Game
  , _isEdit :: Bool
  }

makeLenses ''UI

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

-- App definition and execution

app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

playGame
  :: Maybe Int    -- ^ FPS
  -> Maybe String -- ^ Preview cell (Nothing == no preview)
  -> IO Game
playGame fps filename = do
  let delay = 24000000 `div` fromMaybe 24 fps
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  initialGame <- initGame "temp"
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  ui <- customMain initialVty builder (Just chan) app $ UI
    { _game    = initialGame
    , _isEdit  = False
    }
  return $ ui ^. game

-- Handling events

handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui (AppEvent Tick                      ) = handleTick ui
handleEvent ui (VtyEvent (V.EvKey V.KRight      [])) = exec (move Right) ui
handleEvent ui (VtyEvent (V.EvKey V.KLeft       [])) = exec (move Left) ui
handleEvent ui (VtyEvent (V.EvKey V.KDown       [])) = exec (move Back) ui
handleEvent ui (VtyEvent (V.EvKey V.KUp         [])) = exec (move Forward) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui _                                     = continue ui

-- | This common execution function is used for all game user input except hard
-- drop and pause. If paused or locked (from hard drop) do nothing, else
-- execute the state computation.
exec :: HaScene () -> UI -> EventM Name (Next UI)
exec op =
  guarded
    (not . \ui -> ui ^. isEdit)
    (game %~ execTetris op)

-- | This base execution function takes a predicate and only issues UI
-- modification when predicate passes and game is not over.
guarded :: (UI -> Bool) -> (UI -> UI) -> UI -> EventM Name (Next UI)
guarded p f ui = continue
  $ if not (p ui)
    then ui
    else f ui

-- | Handles time steps, does nothing if game is over or paused
handleTick :: UI -> EventM Name (Next UI)
handleTick ui = do
  next <- execStateT timeStep $ ui ^. game
  continue $ ui & game .~ next

-- | Restart game at the same level
restart :: UI -> EventM Name (Next UI)
restart ui = do
  let filename = ui ^. (game . initFile)
  g <- liftIO $ initGame filename
  continue $ ui & game .~ g
                & isEdit .~ False

-- Drawing

drawUI :: UI -> [Widget Name]
drawUI ui =
  [ C.vCenter $ vLimit 22 $ hBox
      [ padLeft Max $ padRight (Pad 2) $ drawStats (ui ^. game)
      , vLimit 22
          $ withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Scene") (str $ show $ ui ^. (game . camera))
      ]
  ]

showObjList::[Polygon]->[Widget Name]
showObjList = map f
  where
    f a   = padLeftRight 1 $ str (show a)

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Obj Lists")
    $ vBox
      $ showObjList (g ^. objects) ++ [str "press \"q\" to quit"]

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  []
