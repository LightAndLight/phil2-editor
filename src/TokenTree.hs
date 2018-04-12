module TokenTree where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe.Util (untilNothing)
import Data.Semigroup(Semigroup, (<>))

import Brick (Widget, (<+>))
import qualified Brick as Brick
import qualified Graphics.Vty as Vty

data Token = Token !String !Int deriving (Eq, Show, Ord)

instance Semigroup Token where
  Token a b <> Token c d = Token (a <> c) (b + d)

data TokenTree
  = Tree !String !Int (NonEmpty TokenTree)
  | Leaf !Bool Token
  deriving (Eq, Show, Ord)

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
nextToken tz = down tz <|> right tz

prevToken :: TokenTreeZ -> Maybe TokenTreeZ
prevToken tz = left tz <|> up tz

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

renderTokenTree :: Ord n => Vty.Attr -> TokenTree -> Widget n
renderTokenTree a (Leaf _ (Token s _)) = Brick.raw $ Vty.string a s
renderTokenTree a (Tree s _ _) = Brick.raw $ Vty.string a s

renderTokenTrees :: (Functor f, Foldable f, Ord n) => f TokenTree -> Widget n
renderTokenTrees = foldr (<+>) Brick.emptyWidget . fmap (renderTokenTree Vty.defAttr)

renderHistory :: Ord n => [TokenHistory] -> Widget n -> Widget n
renderHistory [] w = w
renderHistory (WentRight tt : hist) w = renderHistory hist (renderTokenTree Vty.defAttr tt <+> w)
renderHistory (WentDown rs : hist) w = renderHistory hist (w <+> renderTokenTrees rs)

renderTokenTreeZ :: Ord n => TokenTreeZ -> Widget n
renderTokenTreeZ ttz =
  renderHistory (history ttz) $
    renderTokenTree (Vty.black `Brick.on` Vty.white) (focus ttz) <+>
    renderTokenTrees (rights ttz)
