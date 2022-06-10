{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Transition
    ( transition
    , transitions
    , transitionProperty
    , transitionProperties
    , transitionDuration
    , transitionDurations
    , TimingFunction
    , transitionTimingFunction
    , transitionTimingFunctions
    , ease, easeIn, easeOut, easeInOut, linear, stepStart, stepEnd
    , stepsStart, stepsStop
    , cubicBezier
    , transitionDelay
    , transitionDelays
    ) where


import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Common (Auto, Other, none, other)
import Blizzard.Css.Property ((!), Val, Value, value)
import Blizzard.Css.Stylesheet (prop)
import Blizzard.Css.Time (Time)
import Data.Text (Text)


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


newtype TimingFunction = TimingFunction Value
    deriving (Auto, Other, Val)


ease, easeIn, easeInOut, easeOut, linear, stepEnd, stepStart :: TimingFunction

ease      = other "ease"
easeIn    = other "ease-in"
easeInOut = other "ease-in-out"
easeOut   = other "ease-out"
linear    = other "linear"
stepEnd   = other "step-end"
stepStart = other "step-start"


stepsStart, stepsStop :: Int -> TimingFunction

stepsStart a = other $ "steps(" <> value a <> ", start)"
stepsStop  a = other $ "steps(" <> value a <> ", end)"


cubicBezier :: Double -> Double -> Double -> Double -> TimingFunction
cubicBezier a b c d = other $ "cubic-bezier(" <> value [a, b, c, d] <> ")"


transitionTimingFunction :: TimingFunction -> Attribute
transitionTimingFunction = prop "transition-timing-function"


transitionTimingFunctions :: [TimingFunction] -> Attribute
transitionTimingFunctions [] = prop "transition-timing-function" (none :: Value)
transitionTimingFunctions xs = prop "transition-timing-function" xs


transitionDelay :: Time -> Attribute
transitionDelay = prop "transition-delay"


transitionDelays :: [Time] -> Attribute
transitionDelays = prop "transition-delay"
