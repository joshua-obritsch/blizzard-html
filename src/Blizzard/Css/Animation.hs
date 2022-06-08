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

import Blizzard.Internal (Attribute(..))
import Clay.Animation
    ( AnimationDirection
    , alternate
    , reverse
    , alternateReverse
    , IterationCount
    , infinite
    , iterationCount
    , AnimationName
    , PlayState
    , running
    , paused
    , FillMode
    , forwards
    , backwards
    )
import Clay.Time (Time)
import Clay.Transition (TimingFunction)

import qualified Clay.Animation as A


animation
    :: AnimationName
    -> Time
    -> TimingFunction
    -> Time
    -> IterationCount
    -> AnimationDirection
    -> FillMode
    -> Attribute
animation a b c d e f g = AttrCss $ A.animation a b c d e f g


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
animations a = AttrCss $ A.animations a


animationDelay :: Time -> Attribute
animationDelay a = AttrCss $ A.animationDelay a


animationDelays :: [Time] -> Attribute
animationDelays a = AttrCss $ A.animationDelays a


animationDirection :: AnimationDirection -> Attribute
animationDirection a = AttrCss $ A.animationDirection a


animationDirections :: [AnimationDirection] -> Attribute
animationDirections a = AttrCss $ A.animationDirections a


animationDuration :: Time -> Attribute
animationDuration a = AttrCss $ A.animationDuration a


animationDurations :: [Time] -> Attribute
animationDurations a = AttrCss $ A.animationDurations a


animationIterationCount :: IterationCount -> Attribute
animationIterationCount a = AttrCss $ A.animationIterationCount a


animationIterationCounts :: [IterationCount] -> Attribute
animationIterationCounts a = AttrCss $ A.animationIterationCounts a


animationName :: AnimationName -> Attribute
animationName a = AttrCss $ A.animationName a


animationPlayState :: PlayState -> Attribute
animationPlayState a = AttrCss $ A.animationPlayState a


animationFillMode :: FillMode -> Attribute
animationFillMode a = AttrCss $ A.animationFillMode a


animationTimingFunction :: TimingFunction -> Attribute
animationTimingFunction a = AttrCss $ A.animationTimingFunction a
