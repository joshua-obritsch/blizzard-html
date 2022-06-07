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
    , ease, easeIn, easeOut, easeInOut, linear, stepStart, stepEnd, stepStop
    , stepsStart, stepsStop
    , cubicBezier
    , transitionDelay
    , transitionDelays
    ) where


import Blizzard.Internal (Attribute(..))
import Clay.Time (Time)
import Clay.Transition
    ( TimingFunction
    , ease, easeIn, easeOut, easeInOut, linear, stepStart, stepEnd, stepStop
    , stepsStart, stepsStop
    , cubicBezier
    )
import Data.Text (Text)

import qualified Clay.Transition as T


transition :: Text -> Time -> TimingFunction -> Time -> Attribute
transition a b c d = AttrCss $ T.transition a b c d


transitions :: [(Text, Time, TimingFunction, Time)] -> Attribute
transitions a = AttrCss $ T.transitions a


transitionProperty :: Text -> Attribute
transitionProperty a = AttrCss $ T.transitionProperty a


transitionProperties :: [Text] -> Attribute
transitionProperties a = AttrCss $ T.transitionProperties a


transitionDuration :: Time -> Attribute
transitionDuration a = AttrCss $ T.transitionDuration a


transitionDurations :: [Time] -> Attribute
transitionDurations a = AttrCss $ T.transitionDurations a


transitionTimingFunction :: TimingFunction -> Attribute
transitionTimingFunction a = AttrCss $ T.transitionTimingFunction a


transitionTimingFunctions :: [TimingFunction] -> Attribute
transitionTimingFunctions a = AttrCss $ T.transitionTimingFunctions a


transitionDelay :: Time -> Attribute
transitionDelay a = AttrCss $ T.transitionDelay a


transitionDelays :: [Time] -> Attribute
transitionDelays a = AttrCss $ T.transitionDelays a
