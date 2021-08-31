module Text.Blizzard.Html
    ( module Text.Blaze.Html
    , documentTag
    , normalTag
    , voidTag
    ) where


import Text.Blaze.Html


documentTag :: (Html -> Html) -> [Html] -> Html
documentTag element []       = element $ toHtml ("" :: String)
documentTag element children = element $ foldl1 (>>) children

normalTag :: (Html -> Html) -> [Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element attributes $ toHtml ("" :: String)
normalTag element attributes children = foldl (!) element attributes $ foldl1 (>>) children

voidTag :: Html -> [Attribute] -> Html
voidTag = foldl (!)
