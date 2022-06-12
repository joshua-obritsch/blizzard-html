{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Internal.Css.TimingFunction
    ( TimingFunction
    , ease, easeIn, easeInOut, linear, stepEnd, stepStart
    , stepsStart, stepsStop
    , cubicBezier
    ) where


import Blizzard.Css.Common
    ( Inherit
    , Initial
    , Revert
    , RevertLayer
    , Unset
    )
import Blizzard.Css.Property (Val(..), Value)

newtype TimingFunction = TimingFunction Value
    deriving
        ( Inherit
        , Initial
        , Revert
        , RevertLayer
        , Unset
        , Val
        )


ease, easeIn, easeInOut, easeOut, linear, stepEnd, stepStart :: TimingFunction

ease      = TimingFunction "ease"
easeIn    = TimingFunction "ease-in"
easeInOut = TimingFunction "ease-in-out"
easeOut   = TimingFunction "ease-out"
linear    = TimingFunction "linear"
stepEnd   = TimingFunction "step-end"
stepStart = TimingFunction "step-start"


stepsStart, stepsStop :: Int -> TimingFunction

stepsStart a = TimingFunction $ "steps(" <> value a <> ", start)"
stepsStop  a = TimingFunction $ "steps(" <> value a <> ", end)"


cubicBezier :: Double -> Double -> Double -> Double -> TimingFunction
cubicBezier a b c d = TimingFunction $ "cubic-bezier(" <> value [a, b, c, d] <> ")"
