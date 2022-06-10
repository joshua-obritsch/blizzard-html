{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Animation
    (
    -- animation
      animation
    , animations

    -- animation-delay
    , animationDelay
    , animationDelays

    -- animation-direction
    , AnimationDirection
    , animationDirection
    , animationDirections
    , alternate
    , reverse
    , alternateReverse

    -- animation-duration
    , animationDuration
    , animationDurations

    -- animation-fill-mode
    , FillMode
    , animationFillMode
    , forwards
    , backwards

    -- animation-iteration-count
    , IterationCount
    , animationIterationCount
    , animationIterationCounts
    , infinite
    , iterationCount

    -- animation-name
    , AnimationName
    , animationName

    -- animation-play-state
    , PlayState
    , animationPlayState
    , running
    , paused

    -- animation-timing-function
    , animationTimingFunction
    ) where


import Prelude hiding (reverse)

import Data.String (IsString)

import Blizzard.Internal (Attribute(..))
import Blizzard.Css.Common (Inherit, Initial, None, Normal, Other, Unset)
import Blizzard.Css.Property ((!), Val, Value, value)
import Blizzard.Css.Stylesheet (prop)
import Blizzard.Css.Time (Time)
import Blizzard.Css.Transition (TimingFunction)


animation
    :: AnimationName
    -> Time
    -> TimingFunction
    -> Time
    -> IterationCount
    -> AnimationDirection
    -> FillMode
    -> Attribute
animation a b c d e f g = prop "animation" (a ! b ! c ! d ! e ! f ! g)


animations
    :: [ ( AnimationName
         , Time
         , TimingFunction
         , Time
         , IterationCount
         , AnimationDirection
         , FillMode
         )
       ] -> Attribute
animations = prop "animation" . map (\(a, b, c, d, e, f, g) -> value (a ! b ! c ! d ! e ! f ! g))


animationDelay :: Time -> Attribute
animationDelay = prop "animation-delay"


animationDelays :: [Time] -> Attribute
animationDelays = prop "animation-delay"


newtype AnimationDirection = AnimationDirection Value
    deriving (Normal, Other, Val)


animationDirection :: AnimationDirection -> Attribute
animationDirection = prop "animation-direction"


animationDirections :: [AnimationDirection] -> Attribute
animationDirections = prop "animation-direction"


alternate, alternateReverse, reverse :: AnimationDirection

alternate        = AnimationDirection "alternate"
alternateReverse = AnimationDirection "alternate-reverse"
reverse          = AnimationDirection "reverse"


animationDuration :: Time -> Attribute
animationDuration = prop "animation-duration"


animationDurations :: [Time] -> Attribute
animationDurations = prop "animation-duration"


newtype IterationCount = IterationCount Value
    deriving (Normal, Other, Val)


animationIterationCount :: IterationCount -> Attribute
animationIterationCount = prop "animation-iteration-count"


animationIterationCounts :: [IterationCount] -> Attribute
animationIterationCounts = prop "animation-iteration-count"


infinite :: IterationCount
infinite = IterationCount "infinite"


iterationCount :: Double -> IterationCount
iterationCount = IterationCount . value


newtype AnimationName = AnimationName Value
    deriving (Inherit, Initial, IsString, Other, Unset, Val)


animationName :: AnimationName -> Attribute
animationName = prop "animation-name"


newtype PlayState = PlayState Value
    deriving (Other, Val)


animationPlayState :: PlayState -> Attribute
animationPlayState = prop "animation-play-state"


paused, running :: PlayState

paused  = PlayState "paused"
running = PlayState "running"


newtype FillMode = FillMode Value
    deriving (None, Other, Val)


animationFillMode :: FillMode -> Attribute
animationFillMode = prop "animation-fill-mode"


backwards, forwards :: FillMode

backwards = FillMode "backwards"
forwards  = FillMode "forwards"


animationTimingFunction :: TimingFunction -> Attribute
animationTimingFunction = prop "animation-timing-function"
