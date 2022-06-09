{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Property
    ( Val(..)
    , Value(..)
    ) where


import Control.Arrow (second)
import Data.List (partition, sort)
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.Text (Text, replace)


quote :: Text -> Text
quote t = "\"" <> replace "\"" "\\\"" t <> "\""


newtype Value = Value { unvalue :: Text }
    deriving (Eq, IsString, Monoid, Semigroup, Show)


class Val a where
    value :: a -> Value


instance Val Text where
    value = Value


instance Val Value where
    value = id


newtype Literal = Literal Text
    deriving (IsString, Monoid, Semigroup, Show)


instance Val Literal where
    value (Literal t) = Value . quote $ t


instance Val Integer where
    value = fromString . show


instance Val Int where
    value = fromString . show


instance Val a => Val [a] where
    value = intercalate "," . map value


intercalate :: Monoid a => a -> [a] -> a
intercalate _ []     = mempty
intercalate s (x:xs) = foldl (\a b -> a `mappend` s `mappend` b) x xs
