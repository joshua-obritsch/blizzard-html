{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Property
    ( (!)
    , Literal(..)
    , Val(..)
    , Value(..)
    , fromDouble
    , noCommas
    , quote
    ) where


import Data.Fixed (Fixed, HasResolution(..), showFixed)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.String (IsString, fromString)
import Data.Text (Text, replace)


data E5 = E5


instance HasResolution E5 where resolution _ = 100000


newtype Value = Value { unvalue :: Text }
    deriving (Eq, IsString, Monoid, Semigroup, Show)


newtype Literal = Literal Text
    deriving (IsString, Monoid, Semigroup, Show)


class Val a where
    value :: a -> Value


instance Val Text where
    value = Value


instance Val Value where
    value = id


instance Val Literal where
    value (Literal a) = Value . quote $ a


instance Val Int where
    value = fromString . show


instance Val a => Val [a] where
    value = intercalate "," . map value


instance Val Double where
    value = Value . fromDouble


instance Val a => Val (Maybe a) where
    value Nothing  = ""
    value (Just a) = value a


instance (Val a, Val b) => Val (a, b) where
    value (a, b) = value a <> " " <> value b


instance Val a => Val (NonEmpty a) where
    value = value . toList


infixr !
(!) :: a -> b -> (a, b)
(!) = (,)


fromDouble :: Double -> Text
fromDouble = fromString . showFixed' . realToFrac
  where
    showFixed' :: Fixed E5 -> String
    showFixed' = showFixed True


intercalate :: Monoid a => a -> [a] -> a
intercalate _ []     = mempty
intercalate s (x:xs) = foldl (\a b -> a `mappend` s `mappend` b) x xs


noCommas :: Val a => [a] -> Value
noCommas = intercalate " " . map value


quote :: Text -> Text
quote t = "\"" <> replace "\"" "\\\"" t <> "\""
