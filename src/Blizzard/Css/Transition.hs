{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Transition
    ( transition
    , transitions
    , transitionProperty
    , transitionProperties
    , transitionDuration
    , transitionDurations
    , transitionTimingFunction
    , transitionTimingFunctions
    , transitionDelay
    , transitionDelays
    ) where

import Data.Text (Text)

import Blizzard.Css.Common (Auto, Other, none, other)
import Blizzard.Css.Property ((!), Val, Value, value)
import Blizzard.Css.Stylesheet (prop)

import Blizzard.Internal (Attribute(..))
import Blizzard.Internal.Css.Time (Time)
import Blizzard.Internal.Css.TimingFunction (TimingFunction)


transition :: Text -> Time -> TimingFunction -> Time -> Attribute
transition a b c d = prop "transition" (a ! b ! c ! d)


transitions :: [(Text, Time, TimingFunction, Time)] -> Attribute
transitions [] = prop "transition" (none :: Value)
transitions xs = prop "transition" $ map (\(a, b, c, d) -> value (a ! b ! c ! d)) xs


transitionProperty :: Text -> Attribute
transitionProperty = prop "transition-property"


transitionProperties :: [Text] -> Attribute
transitionProperties [] = prop "transition-property" (none :: Value)
transitionProperties xs = prop "transition-property" xs


transitionDuration :: Time -> Attribute
transitionDuration = prop "transition-duration"


transitionDurations :: [Time] -> Attribute
transitionDurations [] = prop "transition-duration" (none :: Value)
transitionDurations xs = prop "transition-duration" xs


transitionTimingFunction :: TimingFunction -> Attribute
transitionTimingFunction = prop "transition-timing-function"


transitionTimingFunctions :: [TimingFunction] -> Attribute
transitionTimingFunctions [] = prop "transition-timing-function" (none :: Value)
transitionTimingFunctions xs = prop "transition-timing-function" xs


transitionDelay :: Time -> Attribute
transitionDelay = prop "transition-delay"


transitionDelays :: [Time] -> Attribute
transitionDelays = prop "transition-delay"
