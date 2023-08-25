module Html.Intl
    ( translate
    , intl
    ) where


import Data.Text.Lazy.Builder (Builder)
import Html (Html(..), Translatable)


-- INTERNATIONALIZATION


-- | Converts all instances of 'IntlNode' to 'TextNode' according to the target language. This function should always be called before the
-- 'build' function.
--
-- ==== __Example__
--
-- __Initialization:__
--
-- @
-- data Intl = Intl
--     { de :: Builder
--     , en :: Builder
--     }
--
-- instance Translatable Intl where
--     defaultLanguage = en
-- @
--
-- @
-- translate de $ Html.p []
--     [ Html.intl Intl
--         { de = ""
--         , en = ""
--         }
--     ]
-- @
translate :: Translatable a => (a -> Builder) -> Html a -> Html a
translate lang html = case html of
    ParentNode startTag endTag attributes children -> ParentNode startTag endTag attributes (map (translate lang) children)
    RootNode   startTag                   children -> RootNode   startTag                   (map (translate lang) children)
    LeafNode   startTag        attributes          -> LeafNode   startTag        attributes
    TextNode   text                                -> TextNode   text
    IntlNode   intl                                -> TextNode   text
      where text = lang intl


intl :: Translatable a => a -> Html a
intl = IntlNode
{-# INLINE intl #-}
