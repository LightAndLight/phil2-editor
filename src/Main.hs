{-# language RecursiveDo #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
module Main where

import Brick (Widget)
import Brick.ReflexMain (brickWrapper)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup(Semigroup, (<>))
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

import Phil.Core (Expr(..), Type)
import Phil.Parser (Span, parseExpr)
import Phil.Printer (printExpr, printType)

data Interval
  = Interval
  { line :: !Int
  , colStart :: !Int
  , offset :: !Int
  }
  deriving (Eq, Show)

data TokenTree
  = Tree !String !Int (NonEmpty TokenTree)
  | Leaf !Bool Token
  deriving (Eq, Show, Ord)
data Token = Token !String !Int deriving (Eq, Show, Ord)
instance Semigroup Token where
  Token a b <> Token c d = Token (a <> c) (b + d)

toToken :: TokenTree -> Token
toToken (Leaf _ t) = t
toToken (Tree s off _) = Token s off

tokenTree :: NonEmpty TokenTree -> TokenTree
tokenTree ts =
  let
    Token s off = foldr1 (<>) $ fmap toToken ts
  in
    Tree s off ts

token :: String -> Token
token str = Token str $ length str

data TokenHistory
  = WentDown [TokenTree]
  | WentRight TokenTree
  deriving (Eq, Show, Ord)

data TokenTreeZ
  = TokenTreeZ
  { history :: [TokenHistory]
  , focus :: TokenTree
  , rights :: [TokenTree]
  } deriving (Eq, Show)

zipTokenTree :: TokenTree -> TokenTreeZ
zipTokenTree t = TokenTreeZ [] t []

untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f a = maybe a (untilNothing f) (f a)

untilJust :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
untilJust f g a = f a >>= (\b -> g b <|> untilJust f g b)

down :: TokenTreeZ -> Maybe TokenTreeZ
down (TokenTreeZ hist cur rs) =
  case cur of
    Leaf{} -> Nothing
    Tree _ _ (tt :| tts) -> Just $ TokenTreeZ (WentDown rs : hist) tt tts

up :: TokenTreeZ -> Maybe TokenTreeZ
up ttz =
  case untilNothing left ttz of
    TokenTreeZ (WentDown tts : hist) cur rs ->
      Just $ TokenTreeZ hist (tokenTree $ cur :| rs) tts
    _ -> Nothing

left :: TokenTreeZ -> Maybe TokenTreeZ
left (TokenTreeZ (WentRight tt : hist) cur rs) =
  Just $ TokenTreeZ hist tt (cur:rs)
left _ = Nothing

right :: TokenTreeZ -> Maybe TokenTreeZ
right (TokenTreeZ hist cur rs) =
  case rs of
    [] -> Nothing
    rr:rrs -> Just $ TokenTreeZ (WentRight cur : hist) rr rrs

nextToken :: TokenTreeZ -> Maybe TokenTreeZ
nextToken tz =
  down tz $> untilNothing down tz <|>
  right tz <|>
  untilJust up right tz

prevToken :: TokenTreeZ -> Maybe TokenTreeZ
prevToken tz = left tz <|> up tz

nextEditable :: TokenTreeZ -> Maybe TokenTreeZ
nextEditable tz = do
  tz' <- nextToken tz
  case focus tz' of
    Leaf False _ -> nextEditable tz'
    _ -> Just tz'

prevEditable :: TokenTreeZ -> Maybe TokenTreeZ
prevEditable tz = do
  tz' <- prevToken tz
  case focus tz' of
    Leaf False _ -> prevEditable tz'
    _ -> Just tz'

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

        dAst <- holdDyn ast $ fmapMaybe (updateEvent =<<) eInput $> ast
        dHighlighted <- holdDyn (Interval 1 50 10) (eInput $> Interval 1 50 10)
        dWidgets <- makeUI dAst dHighlighted

        (eInput, eWasShutdown, suspend) <- brickWrapper eDoShutdown dWidgets dCursor dAttrMap

        performPostBuild_ . pure . infoQuit $ pure eWasShutdown
        pure ()

makeUI
  :: (Reflex t, MonadHold t m)
  => Dynamic t (Expr (Type Span) Span)
  -> Dynamic t Interval
  -> m (Dynamic t [Widget String])
makeUI dAst dHighlighted =
  pure $
  sequence
    [ fullDisplay <$> dAst <*> dHighlighted
    ]
  where
    fullDisplay ast interval =
      let
        astD = astDisplay ast
        intervalD = intervalDisplay interval
      in
        Brick.Widget (Brick.hSize astD) (Brick.vSize astD) $ do
          Brick.render intervalD
          Brick.render astD

    astDisplay =
      Widget.hCenter .
      Widget.border .
      Brick.hLimit 80 .
      Brick.viewport "testing" Brick.Vertical .
      Brick.visible .
      Brick.str .
      printExpr (printType id)

    intervalDisplay (Interval line col offset) =
      Brick.translateBy (Brick.Location (col, line)) $
      Brick.raw
        (Vty.charFill
          (Vty.defAttr `Vty.withBackColor` Vty.cyan)
          ' '
          offset
          1)

quitEvent :: Vty.Event -> Maybe ()
quitEvent (Vty.EvKey (Vty.KChar 'q') mods) | Vty.MCtrl `elem` mods = Just ()
quitEvent _ = Nothing

updateEvent :: Vty.Event -> Maybe ()
updateEvent (Vty.EvKey Vty.KEnter []) = Just ()
updateEvent _ = Nothing

