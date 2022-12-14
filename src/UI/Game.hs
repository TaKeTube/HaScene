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

import           Brick                      hiding (Down, Up)
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

import qualified Brick                      as V
import           Data.Maybe                 (fromMaybe)
import           HaRender                   (render)
import           HaScene

data UI = UI
  { _game     :: Game
  , _isEdit   :: Bool
  , _selected :: Int
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
  let delay = 1000000 `div` fromMaybe 24 fps
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
    ,_selected  = -1
    }
  return $ ui ^. game

editOrView :: HaScene () -> HaScene() -> UI -> EventM Name (Next UI)
editOrView f1 f2 ui =
  if ui ^. isEdit then exec f1 ui  else exec f2 ui
emptyOp :: HaScene ()
emptyOp = do return ()

-- Handling events
handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui (AppEvent Tick                      ) = handleTick ui

handleEvent ui (VtyEvent (V.EvKey (V.KChar 'w') [])) =
  editOrView (moveMesh Forward $ ui^.selected) (move Forward) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'a') [])) =
  editOrView (moveMesh Left $ ui^.selected) (move Left) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 's') [])) =
  editOrView (moveMesh Back $ ui^.selected) (move Back) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  editOrView (moveMesh Right $ ui^.selected) (move Right) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'W') [])) =
  editOrView (moveMesh Up $ ui^.selected) (move Up) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'S') [])) =
  editOrView (moveMesh Down $ ui^.selected) (move Down) ui

handleEvent ui (VtyEvent (V.EvKey V.KLeft       [])) =
  editOrView (rotateMesh RLeft $ ui^.selected) (rotate RLeft) ui
handleEvent ui (VtyEvent (V.EvKey V.KRight      [])) =
  editOrView (rotateMesh RRight $ ui^.selected) (rotate RRight) ui
handleEvent ui (VtyEvent (V.EvKey V.KDown       [])) =
  editOrView (rotateMesh RDown $ ui^.selected) (rotate RDown) ui
handleEvent ui (VtyEvent (V.EvKey V.KUp         [])) =
  editOrView (rotateMesh RUp $ ui^.selected) (rotate RUp) ui
handleEvent ui (VtyEvent (V.EvKey V.KLeft       [V.MShift])) =
  editOrView (rotateMesh RLeftR $ ui^.selected) (rotate RLeftR) ui
handleEvent ui (VtyEvent (V.EvKey V.KRight      [V.MShift])) =
  editOrView (rotateMesh RRightR $ ui^.selected) (rotate RRightR) ui

handleEvent ui (VtyEvent (V.EvKey V.KUp       [V.MShift])) =
  editOrView (scaleMesh ScaleUp $ ui^.selected) emptyOp ui
handleEvent ui (VtyEvent (V.EvKey V.KDown      [V.MShift])) =
  editOrView (scaleMesh ScaleDown $ ui^.selected) emptyOp ui

handleEvent ui (VtyEvent (V.EvKey V.KEsc        [])) = halt ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'i') [])) =
    continue $ over isEdit not ui & selected .~ if ui ^.isEdit then -1 else 0
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'j') [])) =
  continue $ ui & selected .~ min (ui ^. selected + 1) (length (ui ^. (game .objects)) - 1)
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'k') [])) =
  continue $ ui & selected .~ max (ui ^. selected - 1) 0
handleEvent ui _                                     = continue ui

-- | This common execution function is used for all game user input except hard
-- drop and pause. If paused or locked (from hard drop) do nothing, else
-- execute the state computation.
exec :: HaScene () -> UI -> EventM Name (Next UI)
exec op ui = continue $ (game %~ execHaScene op) ui

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
                & selected .~ -1

-- Drawing
drawUI :: UI -> [Widget Name]
drawUI ui =
  [ C.vCenter $ vLimit 80 $ hBox
      [ padLeft Max $ padRight (Pad 2) $ drawStats (ui ^. selected) (ui ^. game)
      , vLimit 80
          $ withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Scene") (str $ HaRender.render 100 60 (ui ^. (game . objects)) (ui ^. (game . camera)))
      ]
  ]

hlAttr :: V.AttrName
hlAttr = "highlight"

showObjList :: Int -> [(Int, Mesh)] -> [Widget Name]
showObjList selected = map f
  where
    f (i,a) =
      if selected == i then
        withAttr hlAttr $ padLeftRight 1 $ str (show a)
      else
         padLeftRight 1 $ str (show a)

drawStats :: Int -> Game -> Widget Name
drawStats selected g =
  hLimit 80
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Obj Lists")
    $ vBox
      $ showObjList selected (zip [0..] (g ^. objects)) ++
      [str "press ESC to quit"]

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (hlAttr , V.defAttr `V.withStyle` V.bold
                        `V.withForeColor` V.black
                        `V.withBackColor` V.white)
  ]
