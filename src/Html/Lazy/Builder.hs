-- | Module    : Html.Lazy.Builder
-- Copyright   : (c) Joshua Obritsch, 2025
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Lazy.Builder" module provides a typeclass for converting data types to 'Data.Text.Lazy.Builder.Builder'.
module Html.Lazy.Builder
    ( -- * Primitives
      -- ** ToLazyBuilder
      ToLazyBuilder(..)
    ) where


import Data.Text.Lazy.Builder (Builder)


-- PRIMITIVES


-- | Enables conversion to 'Data.Text.Lazy.Builder.Builder'.
class ToLazyBuilder a where

    -- | Converts to 'Data.Text.Lazy.Builder.Builder'.
    toLazyBuilder :: a -> Builder
