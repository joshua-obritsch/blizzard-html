module Html.Internal (Buildable(..)) where


import Data.Text.Lazy.Builder (Builder)


class Buildable a where
    build :: a -> Builder

{-

module Blizzard.Internal.Html
    ( Html
    , boolAttribute
    , textAttribute
    , documentTag
    , normalTag
    , voidTag
    ) where


import Data.Maybe (catMaybes)
import Data.Monoid (mempty)
import Data.Text (Text, null)
import Prelude ((>>), ($), (.), Bool(..), Maybe(..), foldl, foldl1)
import Text.Blaze.Internal ((!), Attribute, AttributeValue, Markup, textValue)


-- | The __Html__ type defines an HTML element.
--
-- ==== __Example__
--
-- @
-- import Data.Text (Text)
--
-- import Blizzard.Html (Html)
--
-- import qualified Blizzard.Html as Html
-- import qualified Blizzard.Html.Attributes as Attr
--
-- renderLink :: (Text, Text) -> Html
-- renderLink (href, text) =
--     Html.a
--         [ Attr.href href ]
--         [ Html.text text ]
-- @
type Html = Markup


boolAttribute :: Attribute -> Bool -> Maybe Attribute
boolAttribute attr False = Nothing
boolAttribute attr True  = Just attr


textAttribute :: (AttributeValue -> Attribute) -> Text -> Maybe Attribute
textAttribute attr text | null text = Nothing
textAttribute attr text             = Just . attr . textValue $ text


documentTag :: [Html] -> Html
documentTag = foldl1 (>>)


normalTag :: (Html -> Html) -> [Maybe Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element (catMaybes attributes) mempty
normalTag element attributes children = foldl (!) element (catMaybes attributes) $ foldl1 (>>) children


voidTag :: Html -> [Maybe Attribute] -> Html
voidTag element attributes = foldl (!) element (catMaybes attributes)
-}
