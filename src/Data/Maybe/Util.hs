module Data.Maybe.Util where

import Control.Applicative ((<|>))

{-# inline untilNothing #-}
untilNothing :: (a -> Maybe a) -> a -> a
untilNothing f = go
  where
    go a = maybe a go (f a)

{-# inline untilJust #-}
untilJust :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
untilJust f g = go
  where
    go a = f a >>= (\b -> g b <|> go b)
