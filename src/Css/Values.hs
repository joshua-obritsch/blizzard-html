{-# LANGUAGE OverloadedStrings #-}

module Css.Values
    ( auto
    , initial
    , inherit
    ) where


import Data.Text.Lazy.Builder (Builder)


auto :: Builder
auto = "auto"


initial :: Builder
initial = "initial"


inherit :: Builder
inherit = "inherit"
