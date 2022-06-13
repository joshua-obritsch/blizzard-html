{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Common
    ( All(..)
    , Auto(..)
    , Baseline(..)
    , Both(..)
    , Center(..)
    , Fixed(..)
    , Hidden(..)
    , Inherit(..)
    , Initial(..)
    , None(..)
    , Normal(..)
    , Other(..)
    , Revert(..)
    , RevertLayer(..)
    , Scroll(..)
    , Unset(..)
    , Visible(..)
    , call
    , map2
    , map3
    , map4
    , map5
    , map6
    , map7
    ) where


import Data.String (IsString)

import Blizzard.Css.Property ((!), Val(..), Value)


class All         a where all         :: a
class Auto        a where auto        :: a
class Baseline    a where baseline    :: a
class Both        a where both        :: a
class Center      a where center      :: a
class Fixed       a where fixed       :: a
class Hidden      a where hidden      :: a
class Inherit     a where inherit     :: a
class Initial     a where initial     :: a
class None        a where none        :: a
class Normal      a where normal      :: a
class Other       a where other       :: Value -> a
class Revert      a where revert      :: a
class RevertLayer a where revertLayer :: a
class Scroll      a where scroll      :: a
class Unset       a where unset       :: a
class Visible     a where visible     :: a


instance All         Value where all         = "all"
instance Auto        Value where auto        = "auto"
instance Baseline    Value where baseline    = "baseline"
instance Both        Value where both        = "both"
instance Center      Value where center      = "center"
instance Fixed       Value where fixed       = "fixed"
instance Hidden      Value where hidden      = "hidden"
instance Inherit     Value where inherit     = "inherit"
instance Initial     Value where initial     = "initial"
instance None        Value where none        = "none"
instance Normal      Value where normal      = "normal"
instance Other       Value where other       = id
instance Revert      Value where revert      = "revert"
instance RevertLayer Value where revertLayer = "revert-layer"
instance Scroll      Value where scroll      = "scroll"
instance Unset       Value where unset       = "unset"
instance Visible     Value where visible     = "visible"


call :: (IsString a, Monoid a) => a -> a -> a
call a b = a <> "(" <> b <> ")"


map2 :: (Val a, Val b) => [ ( a, b ) ] -> [Value]
map2 = map (\(a, b) -> value (a ! b))


map3 :: (Val a, Val b, Val c) => [ ( a, b, c ) ] -> [Value]
map3 = map (\(a, b, c) -> value (a ! b ! c))


map4 :: (Val a, Val b, Val c, Val d) => [ ( a, b, c, d ) ] -> [Value]
map4 = map (\(a, b, c, d) -> value (a ! b ! c ! d))


map5 :: (Val a, Val b, Val c, Val d, Val e) => [ ( a, b, c, d, e ) ] -> [Value]
map5 = map (\(a, b, c, d, e) -> value (a ! b ! c ! d ! e))


map6 :: (Val a, Val b, Val c, Val d, Val e, Val f) => [ ( a, b, c, d, e, f ) ] -> [Value]
map6 = map (\(a, b, c, d, e, f) -> value (a ! b ! c ! d ! e ! f))


map7 :: (Val a, Val b, Val c, Val d, Val e, Val f, Val g) => [ ( a, b, c, d, e, f, g ) ] -> [Value]
map7 = map (\(a, b, c, d, e, f, g) -> value (a ! b ! c ! d ! e ! f ! g))
