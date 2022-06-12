{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blizzard.Css.Animation
    ( -- * animation

      -- | __Functions__
      animation
    , animation2
    , animation3
    , animation4
    , animation5
    , animation6
    , animation7
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
    , AnimationFillMode

      -- | __Constants__
    , forwards
    , backwards

      -- | __Functions__
    , animationFillMode
    , animationFillModes

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
    , animationNames

      -- * animation-play-state
    , AnimationPlayState

      -- | __Constants__
    , running
    , paused

      -- | __Functions__
    , animationPlayState
    , animationPlayStates

      -- * animation-timing-function

      -- | __Functions__
    , animationTimingFunction
    , animationTimingFunctions
    ) where


import Prelude hiding (reverse)

import Data.String (IsString)

import Blizzard.Css.Common
    ( Both
    , Inherit
    , Initial
    , None(..)
    , Normal(..)
    , Revert
    , RevertLayer
    , Unset
    )
import Blizzard.Css.Property ((!), Val, Value, value)
import Blizzard.Css.Stylesheet (prop)

import Blizzard.Internal (Attribute(..))
import Blizzard.Internal.Css.Time (Time, sec)
import Blizzard.Internal.Css.TimingFunction (TimingFunction, ease)
import Blizzard.Internal.Warning (warning)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation "fade-in"
-- "animation:fade-in
animation :: AnimationName -> Attribute
animation = prop "animation"


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation2 "fade-in" (ms 500)
-- "animation:fade-in 500ms
animation2 :: AnimationName -> Time -> Attribute
animation2 a b = prop "animation" (a ! b)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation3 "fade-in" (ms 500) ease
-- "animation:fade-in 500ms ease
animation3 :: AnimationName -> Time -> TimingFunction -> Attribute
animation3 a b c = prop "animation" (a ! b ! c)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation4 "fade-in" (ms 500) ease (ms 250)
-- "animation:fade-in 500ms ease 250ms
animation4
    :: AnimationName
    -> Time
    -> TimingFunction
    -> Time
    -> Attribute
animation4 a b c d = prop "animation" (a ! b ! c ! d)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation5 "fade-in" (ms 500) ease (ms 250) 2
-- "animation:fade-in 500ms ease 250ms 2
animation5
    :: AnimationName
    -> Time
    -> TimingFunction
    -> Time
    -> AnimationIterationCount
    -> Attribute
animation5 a b c d e = prop "animation" (a ! b ! c ! d ! e)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation6 "fade-in" (ms 500) ease (ms 250) 2 alternate
-- "animation:fade-in 500ms ease 250ms 2 alternate
animation6
    :: AnimationName
    -> Time
    -> TimingFunction
    -> Time
    -> AnimationIterationCount
    -> AnimationDirection
    -> Attribute
animation6 a b c d e f = prop "animation" (a ! b ! c ! d ! e ! f)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> animation7 "fade-in" (ms 500) ease (ms 250) 2 alternate forwards
-- "animation:fade-in 500ms ease 250ms 2 alternate forwards
animation7
    :: AnimationName
    -> Time
    -> TimingFunction
    -> Time
    -> AnimationIterationCount
    -> AnimationDirection
    -> AnimationFillMode
    -> Attribute
animation7 a b c d e f g = prop "animation" (a ! b ! c ! d ! e ! f ! g)


-- | Corresponds to the CSS property __animation__.
--
-- __Examples:__
--
-- >>> :{
--     animations
--         [ ("fade-in", ms 500, ease, ms 250, 3, normal, forwards)
--         , ("fade-out", ms 350, linear, ms 750, 1, alternate, backwards)
--         ]
-- :}
-- "animation:fade-in 500ms ease 250ms 3 normal forwards,fade-out 350ms linear 750ms 1 alternate backwards"
animations
    :: [ ( AnimationName
         , Time
         , TimingFunction
         , Time
         , AnimationIterationCount
         , AnimationDirection
         , AnimationFillMode
         )
       ] -> Attribute
animations = prop "animation" . map (\(a, b, c, d, e, f, g) -> value (a ! b ! c ! d ! e ! f ! g))


-- | Corresponds to the CSS property __animation-delay__.
--
-- __Examples:__
--
-- >>> animationDelay (ms 500)
-- "animation-delay:500ms"
animationDelay :: Time -> Attribute
animationDelay = prop "animation-delay"


-- | Corresponds to the CSS property __animation-delay__.
--
-- __Examples:__
--
-- >>> animationDelays [ms 500, ms 750]
-- "animation-delay:500ms,750ms"
animationDelays :: [Time] -> Attribute
animationDelays [] = prop "animation-delay" animationDelaysWarning
animationDelays xs = prop "animation-delay" xs


-- Prints a warning message and defaults when 'animationDelays' is called with an empty list.
animationDelaysWarning :: [Time]
animationDelaysWarning = warning [sec 0]
    "Warning: 'animationDelays' called with empty list. Defaulting to '0s'."


-- | Encompasses any value accepted by the CSS property __animation-direction__.
--
-- Accepted values:
--
-- +------------------+-------------------+
-- | Input            | Output            |
-- +==================+===================+
-- | alternate        | alternate         |
-- +------------------+-------------------+
-- | alternateReverse | alternate-reverse |
-- +------------------+-------------------+
-- | normal           | normal            |
-- +------------------+-------------------+
-- | reverse          | reverse           |
-- +------------------+-------------------+
newtype AnimationDirection = AnimationDirection Value
    deriving
        ( Inherit
        , Initial
        , Normal
        , Revert
        , RevertLayer
        , Unset
        , Val
        )


alternate, alternateReverse, reverse :: AnimationDirection

alternate        = AnimationDirection "alternate"
alternateReverse = AnimationDirection "alternate-reverse"
reverse          = AnimationDirection "reverse"


-- | Corresponds to the CSS property __animation-direction__.
--
-- __Examples:__
--
-- >>> animationDirection reverse
-- "animation-direction:reverse"
animationDirection :: AnimationDirection -> Attribute
animationDirection = prop "animation-direction"


-- | Corresponds to the CSS property __animation-direction__.
--
-- __Examples:__
--
-- >>> animationDirections [alternateReverse, reverse]
-- "animation-direction:alternate-reverse,reverse"
animationDirections :: [AnimationDirection] -> Attribute
animationDirections [] = prop "animation-direction" animationDirectionsWarning
animationDirections xs = prop "animation-direction" xs


-- Prints a warning message and defaults when 'animationDirections' is called with an empty list.
animationDirectionsWarning :: [AnimationDirection]
animationDirectionsWarning = warning [normal]
    "Warning: 'animationDirections' called with empty list. Defaulting to 'normal'."


-- | Corresponds to the CSS property __animation-duration__.
--
-- __Examples:__
--
-- >>> animationDuration (sec 3)
-- "animation-duration:3s"
animationDuration :: Time -> Attribute
animationDuration = prop "animation-duration"


-- | Corresponds to the CSS property __animation-duration__.
--
-- __Examples:__
--
-- >>> animationDurations [sec 3, sec 5]
-- "animation-duration:3s,5s"
animationDurations :: [Time] -> Attribute
animationDurations [] = prop "animation-duration" animationDurationsWarning
animationDurations xs = prop "animation-duration" xs


-- Prints a warning message and defaults when 'animationDurations' is called with an empty list.
animationDurationsWarning :: [Time]
animationDurationsWarning = warning [sec 0]
    "Warning: 'animationDurations' called with empty list. Defaulting to '0s'."


-- | Encompasses any value accepted by the CSS property __animation-fill-mode__.
--
-- Accepted values:
--
-- +-----------+-----------+
-- | Input     | Output    |
-- +===========+===========+
-- | backwards | backwards |
-- +-----------+-----------+
-- | both      | both      |
-- +-----------+-----------+
-- | forwards  | forwards  |
-- +-----------+-----------+
-- | none      | none      |
-- +-----------+-----------+
newtype AnimationFillMode = AnimationFillMode Value
    deriving
        ( Both
        , Inherit
        , Initial
        , None
        , Revert
        , RevertLayer
        , Unset
        , Val
        )


backwards, forwards :: AnimationFillMode

backwards = AnimationFillMode "backwards"
forwards  = AnimationFillMode "forwards"


-- | Corresponds to the CSS property __animation-fill-mode__.
--
-- __Examples:__
--
-- >>> animationFillMode backwards
-- "animation-fill-mode:backwards"
animationFillMode :: AnimationFillMode -> Attribute
animationFillMode = prop "animation-fill-mode"


-- | Corresponds to the CSS property __animation-fill-mode__.
--
-- __Examples:__
--
-- >>> animationFillModes [backwards, forwards]
-- "animation-fill-mode:backwards,forwards"
animationFillModes :: [AnimationFillMode] -> Attribute
animationFillModes [] = prop "animation-fill-mode" animationFillModesWarning
animationFillModes xs = prop "animation-fill-mode" xs


-- Prints a warning message and defaults when 'animationDurations' is called with an empty list.
animationFillModesWarning :: [AnimationFillMode]
animationFillModesWarning = warning [none]
    "Warning: 'animationFillModes' called with empty list. Defaulting to 'none'."


-- | Encompasses any value accepted by the CSS property __animation-iteration-count__.
--
-- Accepted values:
--
-- +------------+------------+
-- | Input      | Output     |
-- +============+============+
-- | infinite   | infinite   |
-- +------------+------------+
-- | \<number\> | \<number\> |
-- +------------+------------+
newtype AnimationIterationCount = AnimationIterationCount Value
    deriving
        ( Inherit
        , Initial
        , Revert
        , RevertLayer
        , Unset
        , Val
        )


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


-- Prints a warning message and defaults when attempting to perform an illegal operation on 'AnimationIterationCount'.
animationIterationCountTrace :: String -> AnimationIterationCount
animationIterationCountTrace a = warning (AnimationIterationCount "1") $
    "Warning: " <> a <> " called on 'AnimationIterationCount'. Defaulting to '1'."


infinite :: AnimationIterationCount
infinite = AnimationIterationCount "infinite"


-- | Corresponds to the CSS property __animation-iteration-count__.
--
-- __Examples:__
--
-- >>> animationIterationCount 5
-- "animation-iteration-count:5"
animationIterationCount :: AnimationIterationCount -> Attribute
animationIterationCount = prop "animation-iteration-count"


-- | Corresponds to the CSS property __animation-iteration-count__.
--
-- __Examples:__
--
-- >>> animationIterationCount [2.3, infinite]
-- "animation-iteration-count:2.3,infinite"
animationIterationCounts :: [AnimationIterationCount] -> Attribute
animationIterationCounts [] = prop "animation-iteration-count" animationIterationCountsWarning
animationIterationCounts xs = prop "animation-iteration-count" xs


-- Prints a warning message and defaults when 'animationIterationCounts' is called with an empty list.
animationIterationCountsWarning :: [AnimationIterationCount]
animationIterationCountsWarning = warning [1]
    "Warning: 'animationIterationCounts' called with empty list. Defaulting to '1'."


-- | Encompasses any value accepted by the CSS property __animation-name__.
--
-- Accepted values:
--
-- +----------------+----------------+
-- | Input          | Output         |
-- +================+================+
-- | none           | none           |
-- +----------------+----------------+
-- | \<identifier\> | \<identifier\> |
-- +----------------+----------------+
newtype AnimationName = AnimationName Value
    deriving
        ( Inherit
        , Initial
        , IsString
        , None
        , Revert
        , RevertLayer
        , Unset
        , Val
        )


-- | Corresponds to the CSS property __animation-name__.
--
-- __Examples:__
--
-- >>> animationName "fade-in"
-- "animation-name:fade-in"
animationName :: AnimationName -> Attribute
animationName = prop "animation-name"


-- | Corresponds to the CSS property __animation-name__.
--
-- __Examples:__
--
-- >>> animationNames [none, "fade-out"]
-- "animation-name:none,fade-out"
animationNames :: [AnimationName] -> Attribute
animationNames [] = prop "animation-name" animationNamesWarning
animationNames xs = prop "animation-name" xs


-- Prints a warning message and defaults when 'animationNames' is called with an empty list.
animationNamesWarning :: [AnimationName]
animationNamesWarning = warning [none]
    "Warning: 'animationNames' called with empty list. Defaulting to 'none'."


-- | Encompasses any value accepted by the CSS property __animation-play-state__.
--
-- Accepted values:
--
-- +---------+---------+
-- | Input   | Output  |
-- +=========+=========+
-- | paused  | paused  |
-- +---------+---------+
-- | running | running |
-- +---------+---------+
newtype AnimationPlayState = AnimationPlayState Value
    deriving
        ( Inherit
        , Initial
        , Revert
        , RevertLayer
        , Unset
        , Val
        )


paused, running :: AnimationPlayState

paused  = AnimationPlayState "paused"
running = AnimationPlayState "running"


-- | Corresponds to the CSS property __animation-play-state__.
--
-- __Examples:__
--
-- >>> animationPlayState running
-- "animation-play-state:running"
animationPlayState :: AnimationPlayState -> Attribute
animationPlayState = prop "animation-play-state"


-- | Corresponds to the CSS property __animation-play-state__.
--
-- __Examples:__
--
-- >>> animationPlayStates [paused, running]
-- "animation-play-states:paused,running"
animationPlayStates :: [AnimationPlayState] -> Attribute
animationPlayStates [] = prop "animation-play-state" animationPlayStatesWarning
animationPlayStates xs = prop "animation-play-state" xs


-- Prints a warning message and defaults when 'animationPlayStates' is called with an empty list.
animationPlayStatesWarning :: [AnimationPlayState]
animationPlayStatesWarning = warning [running]
    "Warning: 'animationPlayStates' called with empty list. Defaulting to 'running'."


-- | Corresponds to the CSS property __animation-timing-function__.
--
-- __Examples:__
--
-- >>> animationTimingFunction easeIn
-- "animation-timing-function:ease-in"
animationTimingFunction :: TimingFunction -> Attribute
animationTimingFunction = prop "animation-timing-function"


-- | Corresponds to the CSS property __animation-timing-function__.
--
-- __Examples:__
--
-- >>> animationTimingFunctions [ease, linear]
-- "animation-timing-function:ease,linear"
animationTimingFunctions :: [TimingFunction] -> Attribute
animationTimingFunctions [] = prop "animation-timing-function" animationTimingFunctionsWarning
animationTimingFunctions xs = prop "animation-timing-function" xs


-- Prints a warning message and defaults when 'animationTimingFunctions' is called with an empty list.
animationTimingFunctionsWarning :: [TimingFunction]
animationTimingFunctionsWarning = warning [ease]
    "Warning: 'animationTimingFunctions' called with empty list. Defaulting to 'running'."
