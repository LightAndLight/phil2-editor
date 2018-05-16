{-# language LambdaCase #-}
module TokenTree where

import Control.Applicative ((<|>))
import Control.Monad.State (runState, modify)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe.Util (untilNothing, untilNothingM, untilJust)
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

tokenSize :: Token -> Int
tokenSize (Token _ n) = n
tokenSize Newline = 1

instance Semigroup Token where
  Token a b <> Token c d = Token (a <> c) (b + d)
  Newline <> Token a b = Token ("\n" <> a) (1 + b)
  Token a b <> Newline = Token (a <> "\n") (b + 1)
  Newline <> Newline = Token "\n\n" 2

data TokenTree
  = Tree !String !Int (NonEmpty TokenTree)
  | Leaf !Bool Token
  deriving (Eq, Show, Ord)

tokenTreeSize :: TokenTree -> Int
tokenTreeSize (Tree _ n _) = n
tokenTreeSize (Leaf _ t) = tokenSize t

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

-- | Zip to closest newline to the left, and report how much input was
-- traversed
closestNewlineLeft :: TokenTreeZ -> Maybe (TokenTreeZ, Int)
closestNewlineLeft tz =
  prevToken tz $>
  runState
    (untilNothingM
      (\a ->
        case prevToken a of
          Nothing -> pure Nothing
          Just a'
            | Leaf _ Newline <- focus a' -> pure Nothing
            -- Implementation detail: if there is no token before this
            -- one that we are highlighting the first line
            | Nothing <- prevToken a' -> pure Nothing
            | otherwise ->
                case prevLeaf a of
                  Nothing -> pure $ Just a'
                  Just a'' ->
                    let fa'' = focus a'' in
                    case fa'' of
                      Leaf _ Newline -> pure $ Just a'
                      _ -> do
                        modify (+ tokenTreeSize fa'')
                        pure $ Just a'')
      tz)
    0

-- | Zip to closest newline to the right, and report how much input was
-- traversed
closestNewlineRight :: TokenTreeZ -> Maybe (TokenTreeZ, Int)
closestNewlineRight tz =
  nextToken tz $>
  runState
    (untilNothingM
      (\a ->
         let fa = focus a in
         case fa of
           Leaf _ Newline -> pure Nothing
           _ ->
             case nextLeaf a of
               Nothing -> pure $ nextToken a
               Just a' -> do
                 modify (+ tokenTreeSize fa)
                 pure $ Just a')
      tz)
    0

-- | Move n characters to the right
rightN :: Int -> TokenTreeZ -> Maybe TokenTreeZ
rightN n tz =
  case compare n 0 of
    LT -> error "negative input in rightN"
    EQ -> Just tz
    GT ->
      let
        f = focus tz
        ts = tokenTreeSize f
      in
        case f of
          Leaf{}
            | ts <= n -> nextLeaf tz >>= rightN (n-ts)
            | otherwise -> Just tz
          _ -> nextToken tz >>= rightN n

-- | Move n characters to the left
leftN :: Int -> TokenTreeZ -> Maybe TokenTreeZ
leftN n tz =
  case compare n 0 of
    LT -> error "negative input in leftN"
    EQ -> Just tz
    GT -> do
      tz' <- prevToken tz
      let
        f = focus tz'
        ts = tokenTreeSize f
      case f of
        Leaf{}
          | n <= ts -> Just tz'
          | otherwise -> leftN (n-ts) tz'
        _ -> leftN n tz'

upAdjacent :: TokenTreeZ -> Maybe TokenTreeZ
upAdjacent tz = do
  (tz', toMove) <- closestNewlineLeft tz
  (tz'', _) <- closestNewlineLeft =<< prevToken tz'
  rightN
    -- So that if a whole line is selected, then the whole line above will
    -- be selected, but if the first leaf of a line is selected, then that
    -- will be selected above
    (case focus <$> prevToken tz of
       Just (Leaf _ Newline) -> toMove
       _ -> max 1 toMove)
    tz''

downAdjacent :: TokenTreeZ -> Maybe TokenTreeZ
downAdjacent tz = do
  (_, toMove) <- closestNewlineLeft tz
  (tz', _) <- closestNewlineRight tz
  let ptz = prevToken tz
  rightN
    (case focus <$> ptz of
       Just (Leaf _ Newline) -> toMove
       -- Implementation detail: because we're an extra level deep in the
       -- first line of the file
       _ ->
         maybe
           toMove
           (maybe toMove (const $ max 1 toMove) . prevToken)
           ptz)
    =<< nextToken tz'

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
renderHistory (WentRight tt : hist) ts =
  renderHistory hist (fmap ((,) Vty.defAttr) (tokens tt) <> ts)
renderHistory (WentDown rs : hist) ts =
  renderHistory hist (ts <> fmap ((,) Vty.defAttr) (rs >>= tokens))

renderTokenTreeZ :: Ord n => TokenTreeZ -> Widget n
renderTokenTreeZ ttz =
  Brick.vBox $
  renderHistory (history ttz) $
    fmap ((,) blackOnWhite) (tokens $ focus ttz) <>
    fmap ((,) Vty.defAttr) (rights ttz >>= tokens)
  where
    blackOnWhite = (Vty.black `Brick.on` Vty.white)
