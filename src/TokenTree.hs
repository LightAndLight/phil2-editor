{-# language LambdaCase #-}
module TokenTree where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe.Util (untilNothing, untilJust)
import Data.Semigroup(Semigroup, (<>))

import Brick (Widget, (<+>))
import qualified Brick as Brick
import qualified Graphics.Vty as Vty
import qualified Data.List.NonEmpty as NonEmpty

data Token
  = Token !String !Int
  | Newline
  deriving (Eq, Show, Ord)

tokenStr :: Token -> String
tokenStr (Token s _) = s
tokenStr Newline = "\n"

instance Semigroup Token where
  Token a b <> Token c d = Token (a <> c) (b + d)
  Newline <> Token a b = Token ("\n" <> a) (1 + b)
  Token a b <> Newline = Token (a <> "\n") (b + 1)
  Newline <> Newline = Token "\n\n" 2

data TokenTree
  = Tree !String !Int (NonEmpty TokenTree)
  | Leaf !Bool Token
  deriving (Eq, Show, Ord)

tokens :: TokenTree -> [Token]
tokens (Leaf _ t) = [t]
tokens (Tree _ _ ts) = NonEmpty.toList ts >>= tokens

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
  down tz <|>
  right tz <|>
  untilJust up right tz

prevToken :: TokenTreeZ -> Maybe TokenTreeZ
prevToken tz =
  (fmap (untilNothing $ fmap (untilNothing right) . down) . left) tz <|>
  up tz

nextLeaf :: TokenTreeZ -> Maybe TokenTreeZ
nextLeaf tz = do
  tz' <- nextToken tz
  case focus tz' of
    Leaf{} -> Just tz'
    _ -> nextLeaf tz'

prevLeaf :: TokenTreeZ -> Maybe TokenTreeZ
prevLeaf tz = do
  tz' <- prevToken tz
  case focus tz' of
    Leaf{} -> Just tz'
    _ -> prevLeaf tz'

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

splitOn :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitOn p str =
  let
    (noC, rest) = span (not . p) str
  in
    case rest of
      [] -> [noC]
      _ : rest' -> noC : splitOn p rest'

renderTokenTree :: Ord n => [(Vty.Attr, Token)] -> [Widget n]
renderTokenTree ts =
  foldr1 (<+>) .
  fmap (\(a, b) -> Brick.raw . Vty.string a $ tokenStr b) <$>
  splitOn (\case; (_, Newline) -> True; _ -> False) ts

renderHistory :: Ord n => [TokenHistory] -> [(Vty.Attr, Token)] -> [Widget n]
renderHistory [] tt = renderTokenTree tt
renderHistory (WentRight tt : hist) ts = renderHistory hist (fmap ((,) Vty.defAttr) (tokens tt) <> ts)
renderHistory (WentDown rs : hist) ts = renderHistory hist (ts <> fmap ((,) Vty.defAttr) (rs >>= tokens))

renderTokenTreeZ :: Ord n => TokenTreeZ -> Widget n
renderTokenTreeZ ttz =
  Brick.vBox $
  renderHistory (history ttz) $
    fmap ((,) blackOnWhite) (tokens $ focus ttz) <>
    fmap ((,) Vty.defAttr) (rights ttz >>= tokens)
  where
    blackOnWhite = (Vty.black `Brick.on` Vty.white)
