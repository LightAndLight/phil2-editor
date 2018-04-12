{-# language RecursiveDo #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module Main where

import Brick (Widget)
import Brick.ReflexMain (brickWrapper)
import Control.Applicative ((<|>))
import Reflex
  (Reflex, MonadHold, Dynamic, runSpiderHost,
   fmapMaybe, attachWithMaybe, current)
import Reflex.Dynamic (holdDyn)
import Reflex.Host.App (hostApp, performPostBuild_, infoQuit)
import System.Environment (getArgs)

import qualified Brick as Brick
import qualified Brick.Widgets.Border as Widget
import qualified Brick.Widgets.Center as Widget
import qualified Graphics.Vty as Vty

import Phil.Core (Expr(..))
import Phil.Parser (parseExpr)

import TokenTree

exprTokens :: (ty String -> TokenTree) -> Expr ty a -> TokenTree
exprTokens tyTokens e =
  case e of
    Var _ name ->
      Leaf True $ token name
    Abs _ n body ->
      tokenTree
        [ Leaf False $ token "\\"
        , Leaf True $ token n
        , Leaf False $ token " -> "
        , exprTokens tyTokens body
        ]
    App _ f x ->
      tokenTree $
        [ exprTokens tyTokens f
        , Leaf False $ token " "
        , nested x
        ]
    Hole _ -> Leaf True $ token "??"
    Quote _ e' ->
      tokenTree
        [ Leaf False $ token "'"
        , nested e'
        ]
    String _ s -> Leaf True $ token (show s)
    Int _ i -> Leaf True $ token (show i)
    Unquote _ e' ->
      tokenTree
        [ Leaf False $ token "$"
        , nested e'
        ]
    Ann _ e' ty ->
      tokenTree
        [ (case e' of; Ann{} -> brackets; _ -> id) (exprTokens tyTokens e')
        , Leaf False $ token " : "
        , tyTokens ty
        ]
  where
    brackets tree =
      tokenTree [Leaf False $ token "(", tree, Leaf False $ token ")"]
    nested a@App{} = brackets (exprTokens tyTokens a)
    nested a@Ann{} = brackets (exprTokens tyTokens a)
    nested a = exprTokens tyTokens a

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

        dAst <-
          holdDyn
            (zipTokenTree $ exprTokens undefined ast)
            (attachWithMaybe
               (\a b ->
                  (b >>= tabEvent) *> nextEditable a <|>
                  (b >>= shiftTabEvent) *> prevEditable a <|>
                  (b >>= wEvent) *> nextLeaf a <|>
                  (b >>= bEvent) *> prevLeaf a
               )
               (current dAst)
               eInput)
        dWidgets <- makeUI dAst

        (eInput, eWasShutdown, _) <- brickWrapper eDoShutdown dWidgets dCursor dAttrMap

        performPostBuild_ . pure . infoQuit $ pure eWasShutdown
        pure ()

makeUI
  :: (Reflex t, MonadHold t m)
  => Dynamic t TokenTreeZ
  -> m (Dynamic t [Widget String])
makeUI dAst =
  pure $
  sequence
    [ astDisplay <$> dAst
    ]
  where
    astDisplay =
      Widget.hCenter .
      Widget.border .
      Brick.hLimit 80 .
      Brick.viewport "testing" Brick.Vertical .
      renderTokenTreeZ

quitEvent :: Vty.Event -> Maybe ()
quitEvent (Vty.EvKey (Vty.KChar 'q') mods) | Vty.MCtrl `elem` mods = Just ()
quitEvent _ = Nothing

tabEvent :: Vty.Event -> Maybe ()
tabEvent (Vty.EvKey (Vty.KChar '\t') []) = Just ()
tabEvent _ = Nothing

wEvent :: Vty.Event -> Maybe ()
wEvent (Vty.EvKey (Vty.KChar 'w') []) = Just ()
wEvent _ = Nothing

bEvent :: Vty.Event -> Maybe ()
bEvent (Vty.EvKey (Vty.KChar 'b') []) = Just ()
bEvent _ = Nothing

shiftTabEvent :: Vty.Event -> Maybe ()
shiftTabEvent (Vty.EvKey Vty.KBackTab []) = Just ()
shiftTabEvent _ = Nothing

updateEvent :: Vty.Event -> Maybe ()
updateEvent (Vty.EvKey Vty.KEnter []) = Just ()
updateEvent _ = Nothing

