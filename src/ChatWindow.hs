-- | Run chat window with `runChatWindow` and wait for chat end
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module ChatWindow
    ( runChatWindow
    ) where


import Brick
import Brick.BChan
import Brick.Markup (markup)
import Brick.Util (on, fg)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Edit as E
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.String.Utils
import Data.Text.Markup ((@@))
import Data.Text.Zipper (clearZipper)
import Data.Time.Format
import Lens.Micro
import Lens.Micro.TH
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Control.Concurrent.Chan as CC
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Types


data IfaceStruct = WidgetInputLine
                 | WidgetHistory
                 deriving (Ord, Show, Eq)


data ChatEvent = NewChatMessage ChatMessage
                 deriving Show


data St = St
        { _inputLine :: E.Editor String IfaceStruct
        , _messages  :: L.List IfaceStruct ChatMessage
        , _followEnd :: Bool
        }


makeLenses ''St


drawUI :: String -> UserName -> St -> [Widget IfaceStruct]
drawUI chatHeader ownUserName st = [ui]
  where
    ui = withBorderStyle unicode $
         borderWithLabel (str chatHeader) $
         vBox [drawHistory ownUserName st, hBorder, drawInput st]


drawHistory :: UserName -> St -> Widget IfaceStruct
drawHistory ownUserName St{..} =
    L.renderList (renderChatMessage ownUserName) True _messages


renderChatMessage :: UserName -> Bool -> ChatMessage -> Widget IfaceStruct
renderChatMessage ownUserName _selected ChatMessage{..} =
    markup $ (T.pack showPrefix @@ fg (prefixColor user)) <> " " <> ((T.pack $ strip $ message) @@ fg V.white)
  where
    prefixColor UserDescription{..}
        | userName == ownUserName = V.yellow
        | otherwise               = V.blue
    showPrefix = concat ["[", showTime time, " ", showUser user, "]"]
    showTime = formatTime defaultTimeLocale "%F %T"
    showUser UserDescription{..} = concat [userName, "@", userHost, ":", show userPort]


drawInput :: St -> Widget IfaceStruct
drawInput st =
    vLimit 1 $ center (E.renderEditor (str . unlines) True (st ^. inputLine))


appEvent :: CC.Chan String -> St -> T.BrickEvent IfaceStruct ChatEvent -> T.EventM IfaceStruct (T.Next St)
appEvent outputMessages st@St{..} (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey V.KEnter [] -> do
            let msg = strip $ unlines $ getEditContents $ st ^. inputLine
            unless (null msg) $ liftIO $ CC.writeChan outputMessages msg
            let st' = st { _inputLine = applyEdit clearZipper _inputLine }
            M.continue st'
        V.EvKey V.KLeft [] -> M.continue (st {
            _messages = L.listMoveTo 0 _messages,
            _followEnd = False
        })
        V.EvKey V.KRight [] -> M.continue (st {
            _messages = L.listMoveTo (-1) _messages,
            _followEnd = True
        })
        V.EvKey V.KDown [] -> do
            messages' <- L.listMovePageDown _messages
            M.continue (st { _messages = messages', _followEnd = False })
        V.EvKey V.KUp [] -> do
            messages' <- L.listMovePageUp _messages
            M.continue (st { _messages = messages', _followEnd = False })
        _ -> T.handleEventLensed st inputLine E.handleEditorEvent ev >>= M.continue
appEvent _ st@St{..} (T.AppEvent (NewChatMessage msg)) =
    M.continue (st {
        _messages =
            (if _followEnd then L.listMoveTo (-1) else id) $
            L.listInsert (length _messages) msg _messages
        })
appEvent _ st _ = M.continue st


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]


appCursor :: St -> [T.CursorLocation IfaceStruct] -> Maybe (T.CursorLocation IfaceStruct)
appCursor _ = showCursorNamed WidgetInputLine


theApp :: String -> UserName -> CC.Chan String -> M.App St ChatEvent IfaceStruct
theApp chatHeader ownUserName outputMessages =
    M.App { M.appDraw         = drawUI chatHeader ownUserName
          , M.appChooseCursor = appCursor
          , M.appHandleEvent  = appEvent outputMessages
          , M.appStartEvent   = return
          , M.appAttrMap      = const theMap
          }


runChatWindow :: String -> UserName -> History -> CC.Chan ChatMessage -> CC.Chan String -> IO ()
runChatWindow chatHeader ownUserName history inputMessages outputMessages = do
    inputMessages' <- newBChan 1
    void $ forkIO $ forever $ CC.readChan inputMessages >>= (writeBChan inputMessages' . NewChatMessage)
    let initialState = St (E.editor WidgetInputLine (Just 1) "") (L.list WidgetHistory (Vec.fromList history) 1) True
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just inputMessages') (theApp chatHeader ownUserName outputMessages) initialState

