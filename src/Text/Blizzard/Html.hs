module Text.Blizzard.Html
    ( Attribute
    , Html
    , normalTag
    , preEscapedToHtml
    , toHtml
    , voidTag
    ) where


import Text.Blaze.Html5
    ( (!)
    , Attribute
    , Html
    , preEscapedToHtml
    , toHtml
    )


normalTag :: (Html -> Html) -> [Attribute] -> [Html] -> Html
normalTag element attributes []       = foldl (!) element attributes $ toHtml ("" :: String)
normalTag element attributes children = foldl (!) element attributes $ foldl1 (>>) children

voidTag :: Html -> [Attribute] -> Html
voidTag = foldl (!)
