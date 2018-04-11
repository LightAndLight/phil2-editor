{-# language RecursiveDo #-}
{-# language OverloadedStrings #-}
module Main where

import Brick (Widget)
import Brick.ReflexMain (brickWrapper)
import Data.Functor (($>))
import Reflex (runSpiderHost, fmapMaybe, constDyn)
import Reflex.Dynamic (holdDyn)
import Reflex.Host.App (hostApp, performPostBuild_, infoQuit)

import qualified Brick as Brick
import qualified Brick.Widgets.Border as Widget
import qualified Brick.Widgets.Center as Widget
import qualified Graphics.Vty as Vty

main :: IO ()
main = runSpiderHost . hostApp $ mdo
  let
    eDoShutdown = fmapMaybe (quitEvent =<<) eInput
    dCursor = pure $ const Nothing
    dAttrMap = pure $ Brick.attrMap Vty.defAttr []

  dWidgets <- holdDyn [testWidget] (eInput $> [testWidget])

  (eInput, eWasShutdown, suspend) <- brickWrapper eDoShutdown dWidgets dCursor dAttrMap

  performPostBuild_ . pure . infoQuit $ pure eWasShutdown
  pure ()

testWidget :: Widget ()
testWidget = Widget.center . Widget.border $ Brick.txt "hello world"

quitEvent :: Vty.Event -> Maybe ()
quitEvent (Vty.EvKey (Vty.KChar 'q') mods) | Vty.MCtrl `elem` mods = Just ()
quitEvent _ = Nothing
