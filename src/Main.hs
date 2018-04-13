{-# language RecursiveDo #-}
{-# language OverloadedStrings #-}
{-# language OverloadedLists #-}
{-# language MonadComprehensions #-}
{-# language ViewPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
module Main where

import Brick (Widget)
import Brick.ReflexMain (brickWrapper)
import Control.Applicative (Alternative(..), (<|>))
import Data.Foldable (asum, toList)
import Data.Functor (($>))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Contravariant (Contravariant(..), Op(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), Decidable(..))
import Data.Functor.Invariant (Invariant)
import Data.Functor.Invariant.Generic (eotSum)
import Data.Functor.Invariant.Multiplicable
  (Multiply(..), Factor(..), Multiplicable(..), Factorable(..))
import Data.Functor.Invariant.Product (ProductFC(..))
import Data.List (intersperse)
import Data.Semigroup ((<>))
import Reflex
  (Reflex, MonadHold, Dynamic, runSpiderHost,
   fmapMaybe, ffilter, attachWithMaybe, current)
import Reflex.Dynamic (holdDyn)
import Reflex.Host.App (hostApp, performPostBuild_, infoQuit)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import qualified Brick
import qualified Brick.Widgets.Border as Widget
import qualified Brick.Widgets.Center as Widget
import qualified Graphics.Vty as Vty
import qualified Data.Sequence as Seq

import Phil.Core (Expr(..), Type(..), TypeScheme(..), Definition(..))
import Phil.Parser (parseDefinitions)

import TokenTree

brackets :: TokenTree -> TokenTree
brackets tree =
  branch [Leaf False $ strToken "(", tree, Leaf False $ strToken ")"]

definitionsTokens :: [Definition a] -> TokenTree
definitionsTokens ds =
  branch . Seq.fromList $
  intersperse (Leaf False Newline) (fmap definitionTokens ds)

definitionTokens :: Definition a -> TokenTree
definitionTokens d =
  case d of
    DefTypeSig _ name ts ->
      branch
        [ Leaf True $ strToken name
        , Leaf False $ strToken " : "
        , typeSchemeTokens (Leaf True . strToken) ts
        ]
    DefValue _ name val ->
      branch
        [ Leaf True $ strToken name
        , Leaf False $ strToken " = "
        , exprTokens (typeTokens $ Leaf True . strToken) val
        ]

newtype ToTokens a = ToTokens { runToTokens :: Op TokenTree a }
  deriving (Contravariant, Divisible, Decidable)
newtype FromTokens a = FromTokens { runFromTokens :: Compose ((->) TokenTree) Maybe a }
  deriving (Functor, Applicative)
instance Alternative FromTokens where
  empty = FromTokens . Compose $ const Nothing
  FromTokens a <|> FromTokens b =
    FromTokens . Compose $ \x -> getCompose a x <|> getCompose b x

newtype Syntax a = Syntax (ProductFC FromTokens ToTokens a)
  deriving (Invariant, Multiply, Factor, Multiplicable, Factorable)

runSyntaxTo :: Syntax a -> a -> TokenTree
runSyntaxTo (Syntax (ProductFC _ f)) = getOp (runToTokens f)

runSyntaxFrom :: Syntax a -> TokenTree -> Maybe a
runSyntaxFrom (Syntax (ProductFC f _)) = getCompose (runFromTokens f)

string :: Bool -> Syntax String
string editable =
  Syntax $
  ProductFC
    (FromTokens $ Compose $ \tt ->
        case tt of
          Leaf e v | e == editable -> Just $ tokenStr v
          _ -> Nothing)
    (ToTokens $ Op $ Leaf editable . strToken)

keyword :: String -> Syntax ()
keyword str =
  Syntax $
  ProductFC
    (FromTokens $ Compose $ \tt ->
        case tt of
          Leaf False v | tokenStr v == str -> Just ()
          _ -> Nothing)
    (ToTokens . Op $ \_ -> Leaf False (strToken str))

readShow :: (Read a, Show a) => Bool -> Syntax a
readShow editable =
  Syntax $
  ProductFC
    (FromTokens . Compose $ \tt ->
       case tt of
         Leaf e v | editable == e -> readMaybe (tokenStr v)
         _ -> Nothing)
    (ToTokens . Op $ Leaf editable . strToken . show)

syntaxExpr :: Syntax (ty String) -> Syntax (Expr ty a)
syntaxExpr sTy = atom
  where
    atom =
      eotSum $
      -- Var Nothing str
      (munit Nothing >>*<< string True >>*<< munit ()) >>|<<

      -- Abs Nothing str expr
      plug >>|<<

      -- App Nothing expr expr
      plug >>|<<

      -- Hole Nothing
      (munit Nothing >>*<< keyword "??" *<< munit ()) >>|<<

      -- Quote Nothing expr
      (munit Nothing >>*<< keyword "'" *<< syntaxExpr sTy >>*<< munit ()) >>|<<

      -- String Nothing string
      (munit Nothing >>*<< keyword "\"" *<< string True >>*<< keyword "\"" *<< munit ()) >>|<<

      -- Unquote Nothing expr
      (munit Nothing >>*<< keyword "$" *<< syntaxExpr sTy >>*<< munit ()) >>|<<

      -- Ann Nothing expr ty
      plug >>|<<

      -- Int Nothing int
      (munit Nothing >>*<< readShow True >>*<< munit ()) >>|<<

      -- done
      plug
{-
  -- Abs Nothing str expr
  (munit Nothing >>*<<
   keyword "\\" *<< string True >>*<< keyword " -> " *<<
   syntaxExpr sTy >>*<< munit ()) >>|<<

  -- App Nothing expr expr
  (munit Nothing >>*<< syntaxExpr sTy >>*<< syntaxExpr sTy >>*<< munit ()) >>|<<

  -- Hole Nothing
  (munit Nothing >>*<< keyword "??" *<< munit ()) >>|<<

  -- Quote Nothing expr
  (munit Nothing >>*<< keyword "'" *<< syntaxExpr sTy >>*<< munit ()) >>|<<

  -- String Nothing string
  (munit Nothing >>*<< keyword "\"" *<< string True >>*<< keyword "\"" *<< munit ()) >>|<<

  -- Unquote Nothing expr
  (munit Nothing >>*<< keyword "$" *<< syntaxExpr sTy >>*<< munit ()) >>|<<

  -- Ann Nothing expr ty
  (munit Nothing >>*<< syntaxExpr sTy >>*<< keyword " : " *<< sTy >>*<< munit ()) >>|<<

  -- Int Nothing int
  (munit Nothing >>*<< readShow True >>*<< munit ()) >>|<<

  -- done
  plug
  -}

typeSchemeTokens :: (a -> TokenTree) -> TypeScheme ann a -> TokenTree
typeSchemeTokens varTokens (Forall _ vars ty) =
  branch . Seq.fromList $
    [ Leaf False $ strToken "forall " | not (null vars) ] <>
    intersperse (Leaf False $ strToken " ") (fmap varTokens vars) <>
    [ Leaf False $ strToken ". " | not (null vars) ] <>
    [ typeTokens varTokens ty ]

typeTokens :: (a -> TokenTree) -> Type ann a -> TokenTree
typeTokens varTokens e =
  case e of
    TyVar _ a -> varTokens a
    TyArr _ a b ->
      branch
        [ nested a
        , Leaf False $ strToken " -> "
        , typeTokens varTokens b
        ]
    TyCtor _ a -> Leaf True $ strToken a
  where
    nested a@TyArr{} = brackets (typeTokens varTokens a)
    nested a = typeTokens varTokens a

exprTokens :: (ty String -> TokenTree) -> Expr ty a -> TokenTree
exprTokens tyTokens e =
  case e of
    Var _ name ->
      Leaf True $ strToken name
    Abs _ n body ->
      branch
        [ Leaf False $ strToken "\\"
        , Leaf True $ strToken n
        , Leaf False $ strToken " -> "
        , exprTokens tyTokens body
        ]
    App _ f x ->
      branch
        [ exprTokens tyTokens f
        , Leaf False $ strToken " "
        , nested x
        ]
    Hole _ -> Leaf True $ strToken "??"
    Quote _ e' ->
      branch
        [ Leaf False $ strToken "'"
        , nested e'
        ]
    String _ s -> Leaf True $ strToken (show s)
    Int _ i -> Leaf True $ strToken (show i)
    Unquote _ e' ->
      branch
        [ Leaf False $ strToken "$"
        , nested e'
        ]
    Ann _ e' ty ->
      branch
        [ (case e' of; Ann{} -> brackets; _ -> id) (exprTokens tyTokens e')
        , Leaf False $ strToken " : "
        , tyTokens ty
        ]
  where
    nested a@App{} = brackets (exprTokens tyTokens a)
    nested a@Ann{} = brackets (exprTokens tyTokens a)
    nested a = exprTokens tyTokens a

tokensExpr :: (TokenTree -> Maybe (ty String)) -> TokenTree -> Maybe (Expr ty a)
tokensExpr tokensTy e =
  case e of
    Leaf True (tokenStr -> "??") -> Just $ Hole Nothing
    Leaf True (tokenStr -> tok) ->
      Int Nothing <$> readMaybe tok <|>
      String Nothing <$> readMaybe tok <|>
      Just (Var Nothing tok)
    Branch _
      (toList ->
       [ Leaf False (tokenStr -> "\\")
       , Leaf True (tokenStr -> n)
       , Leaf False (tokenStr -> " -> ")
       , body
       ])
      -> Abs Nothing n <$> tokensExpr tokensTy body
    Branch _
      (toList ->
       [ f
       , Leaf False (tokenStr -> " ")
       , x
       ])
      -> App Nothing <$> tokensExpr tokensTy f <*> tokensExpr tokensTy x
    Branch _
      (toList ->
       [ Leaf False (tokenStr -> "'")
       , x
       ])
      -> Quote Nothing <$> tokensExpr tokensTy x
    Branch _
      (toList ->
       [ Leaf False (tokenStr -> "$")
       , x
       ])
      -> Unquote Nothing <$> tokensExpr tokensTy x
    Branch _
      (toList ->
       [ a
       , Leaf False (tokenStr -> " : ")
       , ty
       ])
      -> Ann Nothing <$> tokensExpr tokensTy a <*> tokensTy ty
    Branch _
      (toList ->
       [ Leaf False (tokenStr -> "(")
       , tree
       , Leaf False (tokenStr -> ")")
       ])
      -> tokensExpr tokensTy tree
    _ -> Nothing

keybindings :: [(Vty.Event -> Bool, TokenTreeZ -> Maybe TokenTreeZ)]
keybindings =
  [ (charEvent '\t', nextEditable)
  , (shiftTabEvent, prevEditable)
  , (charEvent 'w', nextLeaf)
  , (charEvent 'b', prevLeaf)
  , (charEvent 'k', upAdjacent)
  , (charEvent 'j', downAdjacent)
  ]
  where
    charEvent c (Vty.EvKey (Vty.KChar c') []) = c == c'
    charEvent _ _ = False

    shiftTabEvent (Vty.EvKey Vty.KBackTab []) = True
    shiftTabEvent _ = False

main :: IO ()
main = do
  file:_ <- getArgs
  content <- readFile file
  case parseDefinitions content of
    Left err -> putStrLn err
    Right ast ->
      runSpiderHost . hostApp $ mdo
        let
          eBrickEvent = fmapMaybe id eInput
          eDoShutdown = ffilter quitEvent eBrickEvent $> ()
          dCursor = pure $ const Nothing
          dAttrMap = pure $ Brick.attrMap Vty.defAttr []

        dAst <-
          holdDyn
            (zipTokenTree $ definitionsTokens ast)
            (attachWithMaybe
               (\tree event -> asum $ (\(f, g) -> [ e | e <- g tree, f event]) <$> keybindings)
               (current dAst)
               eBrickEvent)
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

quitEvent :: Vty.Event -> Bool
quitEvent (Vty.EvKey (Vty.KChar 'q') mods) = Vty.MCtrl `elem` mods
quitEvent _ = False
