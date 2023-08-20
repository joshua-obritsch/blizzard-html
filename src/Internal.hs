module Internal (Buildable(..)) where


import Data.Text.Lazy.Builder (Builder)


class Buildable a where
    build :: a -> Builder
