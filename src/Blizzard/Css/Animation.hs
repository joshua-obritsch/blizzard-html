{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Animation
    ( -- * animation

      -- | __Functions__
      animation
    , animations

      -- * animation-delay

      -- | __Functions__
    , animationDelay
    , animationDelays

      -- * animation-direction
    , AnimationDirection

      -- | __Constants__
    , alternate
    , reverse
    , alternateReverse

      -- | __Functions__
    , animationDirection
    , animationDirections

      -- * animation-duration

      -- | __Functions__
    , animationDuration
    , animationDurations

      -- * animation-fill-mode
    , FillMode

      -- | __Constants__
    , forwards
    , backwards

      -- | __Functions__
    , animationFillMode

      -- * animation-iteration-count
    , AnimationIterationCount

      -- | __Constants__
    , infinite

      -- | __Functions__
    , animationIterationCount
    , animationIterationCounts

      -- * animation-name
    , AnimationName

      -- | __Functions__
    , animationName

      -- * animation-play-state
    , PlayState

      -- | __Constants__
    , running
    , paused

      -- | __Functions__
    , animationPlayState

      -- * animation-timing-function

      -- | __Functions__
    , animationTimingFunction
    ) where


import Prelude hiding (reverse)

import Data.String (IsString)
import Debug.Trace (trace)

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
    -> AnimationIterationCount
    -> AnimationDirection
    -> FillMode
    -> Attribute
animation a b c d e f g = prop "animation" (a ! b ! c ! d ! e ! f ! g)


animations
    :: [ ( AnimationName
         , Time
         , TimingFunction
         , Time
         , AnimationIterationCount
         , AnimationDirection
         , FillMode
         )
       ] -> Attribute
animations = prop "animation" . map (\(a, b, c, d, e, f, g) -> value (a ! b ! c ! d ! e ! f ! g))


-- | Corresponds to the CSS property __animation-delay__.
--
-- Indicates when the animation will start after a specified delay.
--
-- __Examples:__
--
-- >>> animationDelay (ms 500)
-- "animation-delay:500.0ms"
animationDelay :: Time -> Attribute
animationDelay = prop "animation-delay"


-- | Corresponds to the CSS property __animation-delay__.
--
-- Indicates when the animations will start after a specified delay.
--
-- __Examples:__
--
-- >>> animationDelays [ms 500, ms 750]
-- "animation-delay:500.0ms,750.0ms"
animationDelays :: [Time] -> Attribute
animationDelays = prop "animation-delay"


-- | Encompasses any value accepted by the CSS property __animation-direction__.
--
-- Accepted values are:
--
-- +------------------+-------------------+
-- | Input            | Output            |
-- +==================+===================+
-- | alternate        | alternate         |
-- +------------------+-------------------+
-- | alternateReverse | alternate-reverse |
-- +------------------+-------------------+
-- | inherit          | inherit           |
-- +------------------+-------------------+
-- | initial          | initial           |
-- +------------------+-------------------+
-- | normal           | normal            |
-- +------------------+-------------------+
-- | reverse          | reverse           |
-- +------------------+-------------------+
-- | unset            | unset             |
-- +------------------+-------------------+
newtype AnimationDirection = AnimationDirection Value
    deriving (Inherit, Initial, Normal, Unset, Val)


alternate, alternateReverse, reverse :: AnimationDirection

alternate        = AnimationDirection "alternate"
alternateReverse = AnimationDirection "alternate-reverse"
reverse          = AnimationDirection "reverse"


-- | Corresponds to the CSS property __animation-direction__.
--
-- Indicates whether or not the animation will play in alternate or reverse cycles.
--
-- __Examples:__
--
-- >>> animationDirection reverse
-- "animation-direction:reverse"
animationDirection :: AnimationDirection -> Attribute
animationDirection = prop "animation-direction"


-- | Corresponds to the CSS property __animation-direction__.
--
-- Indicates whether or not the animations will play in alternate or reverse cycles.
--
-- __Examples:__
--
-- >>> animationDirections [alternateReverse, reverse]
-- "animation-direction:alternate-reverse,reverse"
animationDirections :: [AnimationDirection] -> Attribute
animationDirections = prop "animation-direction"


-- | Corresponds to the CSS property __animation-duration__.
--
-- Indicates how long the animation needs to complete one cycle.
--
-- __Examples:__
--
-- >>> animationDuration (sec 3)
-- "animation-duration:3.0s"
animationDuration :: Time -> Attribute
animationDuration = prop "animation-duration"


-- | Corresponds to the CSS property __animation-duration__.
--
-- Indicates how long the animations need to complete one cycle.
--
-- __Examples:__
--
-- >>> animationDurations [sec 3, sec 5]
-- "animation-duration:3.0s,5.0s"
animationDurations :: [Time] -> Attribute
animationDurations = prop "animation-duration"


-- | Encompasses any value accepted by the CSS property __animation-iteration-count__.
--
-- Accepted values are:
--
-- +------------+------------+
-- | Input      | Output     |
-- +============+============+
-- | inherit    | inherit    |
-- +------------+------------+
-- | initial    | initial    |
-- +------------+------------+
-- | infinite   | infinite   |
-- +------------+------------+
-- | unset      | unset      |
-- +------------+------------+
-- | \<number\> | \<number\> |
-- +------------+------------+
newtype AnimationIterationCount = AnimationIterationCount Value
    deriving (Inherit, Initial, Unset, Val)


instance Num AnimationIterationCount where
    (+)         _ _ = animationIterationCountTrace "(+)"
    (-)         _ _ = animationIterationCountTrace "(-)"
    (*)         _ _ = animationIterationCountTrace "(*)"
    negate      _   = animationIterationCountTrace "'negate'"
    abs         _   = animationIterationCountTrace "'abs'"
    signum      _   = animationIterationCountTrace "'signum'"
    fromInteger     = AnimationIterationCount . (value :: Int -> Value) . fromInteger


instance Fractional AnimationIterationCount where
    (/)          _ _ = animationIterationCountTrace "(/)"
    recip        _   = animationIterationCountTrace "'recip'"
    fromRational     = AnimationIterationCount . (value :: Double -> Value) . fromRational


-- Prints an error message and defaults when attempting to perform an illegal operation on AnimationIterationCount.
animationIterationCountTrace :: String -> AnimationIterationCount
animationIterationCountTrace a
    = trace ("Warning: " <> a <> " called on AnimationIterationCount. Defaulting to '1'.")
    $ AnimationIterationCount "1"


infinite :: AnimationIterationCount
infinite = AnimationIterationCount "infinite"


-- | Corresponds to the CSS property __animation-iteration-count__.
--
-- Indicates how many times the animation will play.
--
-- __Examples:__
--
-- >>> animationIterationCount 5
-- "animation-iteration-count:5"
animationIterationCount :: AnimationIterationCount -> Attribute
animationIterationCount = prop "animation-iteration-count"


-- | Corresponds to the CSS property __animation-iteration-count__.
--
-- Indicates how many times the animations will play.
--
-- __Examples:__
--
-- >>> animationIterationCount [2.3, infinite]
-- "animation-iteration-count:2.3,infinite"
animationIterationCounts :: [AnimationIterationCount] -> Attribute
animationIterationCounts = prop "animation-iteration-count"


-- | Encompasses any value accepted by the CSS property __animation-name__.
--
-- Accepted values are:
--
-- +----------------+----------------+
-- | Input          | Output         |
-- +================+================+
-- | inherit        | inherit        |
-- +----------------+----------------+
-- | initial        | initial        |
-- +----------------+----------------+
-- | none           | none           |
-- +----------------+----------------+
-- | unset          | unset          |
-- +----------------+----------------+
-- | \<identifier\> | \<identifier\> |
-- +----------------+----------------+
newtype AnimationName = AnimationName Value
    deriving (Inherit, Initial, IsString, None, Unset, Val)


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
