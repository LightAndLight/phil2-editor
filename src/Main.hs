{-# language RecursiveDo #-}
{-# language OverloadedStrings #-}
module Main where

import Brick (Widget)
import Brick.ReflexMain (brickWrapper)
import Data.Functor (($>))
import Reflex
  ((<@), Reflex, MonadHold, Event, Behavior, Dynamic, runSpiderHost,
   fmapMaybe, never)
import Reflex.Dynamic (holdDyn)
import Reflex.Host.App (hostApp, performPostBuild_, infoQuit)
import System.Environment (getArgs)

import qualified Brick as Brick
import qualified Brick.Widgets.Border as Widget
import qualified Brick.Widgets.Center as Widget
import qualified Graphics.Vty as Vty

import Phil.Core (Expr, Type)
import Phil.Parser (Span, parseExpr)
import Phil.Printer (printExpr, printType)

main :: IO ()
main = do
  file:_ <- getArgs
  content <- readFile file
  case parseExpr content of
    Left err -> putStrLn err
    Right ast ->
      runSpiderHost . hostApp $ mdo
        let
          eDoShutdown = fmapMaybe (quitEvent =<<) eInput
          dCursor = pure $ const Nothing
          dAttrMap = pure $ Brick.attrMap Vty.defAttr []

        dAst <- holdDyn ast $ fmapMaybe (updateEvent =<<) eInput $> ast
        dWidgets <- makeUI dAst

        (eInput, eWasShutdown, suspend) <- brickWrapper eDoShutdown dWidgets dCursor dAttrMap

        performPostBuild_ . pure . infoQuit $ pure eWasShutdown
        pure ()

makeUI
  :: (Reflex t, MonadHold t m)
  => Dynamic t (Expr (Type Span) Span)
  -> m (Dynamic t [Widget String])
makeUI dAst =
  pure $ sequence [astDisplay <$> dAst]
  where
    astDisplay =
      Widget.hCenter .
      Widget.border .
      Brick.hLimit 80 .
      Brick.viewport "testing" Brick.Vertical .
      Brick.visible .
      Brick.str .
      printExpr (printType id)

quitEvent :: Vty.Event -> Maybe ()
quitEvent (Vty.EvKey (Vty.KChar 'q') mods) | Vty.MCtrl `elem` mods = Just ()
quitEvent _ = Nothing

updateEvent :: Vty.Event -> Maybe ()
updateEvent (Vty.EvKey Vty.KEnter []) = Just ()
updateEvent _ = Nothing

