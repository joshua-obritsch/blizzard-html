{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module    : Html.Aria
-- Copyright   : (c) Joshua Obritsch, 2025
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Aria" module provides a set of functions for generating HTML aria attributes.
module Html.Aria
    ( -- * Attributes
      -- ** ariaActivedescendant
      ariaActivedescendant
      -- ** ariaAtomic
    , ariaAtomic
      -- ** ariaAutocomplete
    , ariaAutocomplete
      -- ** ariaBraillelabel
    , ariaBraillelabel
      -- ** ariaBrailleroledescription
    , ariaBrailleroledescription
      -- ** ariaBusy
    , ariaBusy
      -- ** ariaChecked
    , ariaChecked
      -- ** ariaColcount
    , ariaColcount
      -- ** ariaColindex
    , ariaColindex
      -- ** ariaColindextext
    , ariaColindextext
      -- ** ariaColspan
    , ariaColspan
      -- ** ariaControls
    , ariaControls
      -- ** ariaCurrent
    , ariaCurrent
      -- ** ariaDescribedby
    , ariaDescribedby
      -- ** ariaDescription
    , ariaDescription
      -- ** ariaDetails
    , ariaDetails
      -- ** ariaDisabled
    , ariaDisabled
      -- ** ariaErrormessage
    , ariaErrormessage
      -- ** ariaExpanded
    , ariaExpanded
      -- ** ariaFlowto
    , ariaFlowto
      -- ** ariaHaspopup
    , ariaHaspopup
      -- ** ariaHidden
    , ariaHidden
      -- ** ariaInvalid
    , ariaInvalid
      -- ** ariaKeyshortcuts
    , ariaKeyshortcuts
      -- ** ariaLabel
    , ariaLabel
      -- ** ariaLabelledby
    , ariaLabelledby
      -- ** ariaLevel
    , ariaLevel
      -- ** ariaLive
    , ariaLive
      -- ** ariaModal
    , ariaModal
      -- ** ariaMultiline
    , ariaMultiline
      -- ** ariaMultiselectable
    , ariaMultiselectable
      -- ** ariaOrientation
    , ariaOrientation
      -- ** ariaOwns
    , ariaOwns
      -- ** ariaPlaceholder
    , ariaPlaceholder
      -- ** ariaPosinset
    , ariaPosinset
      -- ** ariaPressed
    , ariaPressed
      -- ** ariaReadonly
    , ariaReadonly
      -- ** ariaRelevant
    , ariaRelevant
      -- ** ariaRequired
    , ariaRequired
      -- ** ariaRoledescription
    , ariaRoledescription
      -- ** ariaRowcount
    , ariaRowcount
      -- ** ariaRowindex
    , ariaRowindex
      -- ** ariaRowindextext
    , ariaRowindextext
      -- ** ariaRowspan
    , ariaRowspan
      -- ** ariaSelected
    , ariaSelected
      -- ** ariaSetsize
    , ariaSetsize
      -- ** ariaSort
    , ariaSort
      -- ** ariaValuemax
    , ariaValuemax
      -- ** ariaValuemin
    , ariaValuemin
      -- ** ariaValuenow
    , ariaValuenow
      -- ** ariaValuetext
    , ariaValuetext
      -- ** role
    , role
    ) where


import Data.Text.Lazy.Builder (Builder)
import Html                   (Attribute(..))


-- ATTRIBUTES


-- | Generates an HTML @aria-activedescendant@ attribute with the given value.
ariaActivedescendant :: Builder -> Attribute ctx
ariaActivedescendant = TextAttribute " aria-activedescendant=\""
{-# INLINE ariaActivedescendant #-}


-- | Generates an HTML @aria-atomic@ attribute with the given value.
ariaAtomic :: Builder -> Attribute ctx
ariaAtomic = TextAttribute " aria-atomic=\""
{-# INLINE ariaAtomic #-}


-- | Generates an HTML @aria-autocomplete@ attribute with the given value.
ariaAutocomplete :: Builder -> Attribute ctx
ariaAutocomplete = TextAttribute " aria-autocomplete=\""
{-# INLINE ariaAutocomplete #-}


-- | Generates an HTML @aria-braillelabel@ attribute with the given value.
ariaBraillelabel :: Builder -> Attribute ctx
ariaBraillelabel = TextAttribute " aria-braillelabel=\""
{-# INLINE ariaBraillelabel #-}


-- | Generates an HTML @aria-brailleroledescription@ attribute with the given value.
ariaBrailleroledescription :: Builder -> Attribute ctx
ariaBrailleroledescription = TextAttribute " aria-brailleroledescription=\""
{-# INLINE ariaBrailleroledescription #-}


-- | Generates an HTML @aria-busy@ attribute with the given value.
ariaBusy :: Builder -> Attribute ctx
ariaBusy = TextAttribute " aria-busy=\""
{-# INLINE ariaBusy #-}


-- | Generates an HTML @aria-checked@ attribute with the given value.
ariaChecked :: Builder -> Attribute ctx
ariaChecked = TextAttribute " aria-checked=\""
{-# INLINE ariaChecked #-}


-- | Generates an HTML @aria-colcount@ attribute with the given value.
ariaColcount :: Builder -> Attribute ctx
ariaColcount = TextAttribute " aria-colcount=\""
{-# INLINE ariaColcount #-}


-- | Generates an HTML @aria-colindex@ attribute with the given value.
ariaColindex :: Builder -> Attribute ctx
ariaColindex = TextAttribute " aria-colindex=\""
{-# INLINE ariaColindex #-}


-- | Generates an HTML @aria-colindextext@ attribute with the given value.
ariaColindextext :: Builder -> Attribute ctx
ariaColindextext = TextAttribute " aria-colindextext=\""
{-# INLINE ariaColindextext #-}


-- | Generates an HTML @aria-colspan@ attribute with the given value.
ariaColspan :: Builder -> Attribute ctx
ariaColspan = TextAttribute " aria-colspan=\""
{-# INLINE ariaColspan #-}


-- | Generates an HTML @aria-controls@ attribute with the given value.
ariaControls :: Builder -> Attribute ctx
ariaControls = TextAttribute " aria-controls=\""
{-# INLINE ariaControls #-}


-- | Generates an HTML @aria-current@ attribute with the given value.
ariaCurrent :: Builder -> Attribute ctx
ariaCurrent = TextAttribute " aria-current=\""
{-# INLINE ariaCurrent #-}


-- | Generates an HTML @aria-describedby@ attribute with the given value.
ariaDescribedby :: Builder -> Attribute ctx
ariaDescribedby = TextAttribute " aria-describedby=\""
{-# INLINE ariaDescribedby #-}


-- | Generates an HTML @aria-description@ attribute with the given value.
ariaDescription :: Builder -> Attribute ctx
ariaDescription = TextAttribute " aria-description=\""
{-# INLINE ariaDescription #-}


-- | Generates an HTML @aria-details@ attribute with the given value.
ariaDetails :: Builder -> Attribute ctx
ariaDetails = TextAttribute " aria-details=\""
{-# INLINE ariaDetails #-}


-- | Generates an HTML @aria-disabled@ attribute with the given value.
ariaDisabled :: Builder -> Attribute ctx
ariaDisabled = TextAttribute " aria-disabled=\""
{-# INLINE ariaDisabled #-}


-- | Generates an HTML @aria-errormessage@ attribute with the given value.
ariaErrormessage :: Builder -> Attribute ctx
ariaErrormessage = TextAttribute " aria-errormessage=\""
{-# INLINE ariaErrormessage #-}


-- | Generates an HTML @aria-expanded@ attribute with the given value.
ariaExpanded :: Builder -> Attribute ctx
ariaExpanded = TextAttribute " aria-expanded=\""
{-# INLINE ariaExpanded #-}


-- | Generates an HTML @aria-flowto@ attribute with the given value.
ariaFlowto :: Builder -> Attribute ctx
ariaFlowto = TextAttribute " aria-flowto=\""
{-# INLINE ariaFlowto #-}


-- | Generates an HTML @aria-haspopup@ attribute with the given value.
ariaHaspopup :: Builder -> Attribute ctx
ariaHaspopup = TextAttribute " aria-haspopup=\""
{-# INLINE ariaHaspopup #-}


-- | Generates an HTML @aria-hidden@ attribute with the given value.
ariaHidden :: Builder -> Attribute ctx
ariaHidden = TextAttribute " aria-hidden=\""
{-# INLINE ariaHidden #-}


-- | Generates an HTML @aria-invalid@ attribute with the given value.
ariaInvalid :: Builder -> Attribute ctx
ariaInvalid = TextAttribute " aria-invalid=\""
{-# INLINE ariaInvalid #-}


-- | Generates an HTML @aria-keyshortcuts@ attribute with the given value.
ariaKeyshortcuts :: Builder -> Attribute ctx
ariaKeyshortcuts = TextAttribute " aria-keyshortcuts=\""
{-# INLINE ariaKeyshortcuts #-}


-- | Generates an HTML @aria-label@ attribute with the given value.
ariaLabel :: Builder -> Attribute ctx
ariaLabel = TextAttribute " aria-label=\""
{-# INLINE ariaLabel #-}


-- | Generates an HTML @aria-labelledby@ attribute with the given value.
ariaLabelledby :: Builder -> Attribute ctx
ariaLabelledby = TextAttribute " aria-labelledby=\""
{-# INLINE ariaLabelledby #-}


-- | Generates an HTML @aria-level@ attribute with the given value.
ariaLevel :: Builder -> Attribute ctx
ariaLevel = TextAttribute " aria-level=\""
{-# INLINE ariaLevel #-}


-- | Generates an HTML @aria-live@ attribute with the given value.
ariaLive :: Builder -> Attribute ctx
ariaLive = TextAttribute " aria-live=\""
{-# INLINE ariaLive #-}


-- | Generates an HTML @aria-modal@ attribute with the given value.
ariaModal :: Builder -> Attribute ctx
ariaModal = TextAttribute " aria-modal=\""
{-# INLINE ariaModal #-}


-- | Generates an HTML @aria-multiline@ attribute with the given value.
ariaMultiline :: Builder -> Attribute ctx
ariaMultiline = TextAttribute " aria-multiline=\""
{-# INLINE ariaMultiline #-}


-- | Generates an HTML @aria-multiselectable@ attribute with the given value.
ariaMultiselectable :: Builder -> Attribute ctx
ariaMultiselectable = TextAttribute " aria-multiselectable=\""
{-# INLINE ariaMultiselectable #-}


-- | Generates an HTML @aria-orientation@ attribute with the given value.
ariaOrientation :: Builder -> Attribute ctx
ariaOrientation = TextAttribute " aria-orientation=\""
{-# INLINE ariaOrientation #-}


-- | Generates an HTML @aria-owns@ attribute with the given value.
ariaOwns :: Builder -> Attribute ctx
ariaOwns = TextAttribute " aria-owns=\""
{-# INLINE ariaOwns #-}


-- | Generates an HTML @aria-placeholder@ attribute with the given value.
ariaPlaceholder :: Builder -> Attribute ctx
ariaPlaceholder = TextAttribute " aria-placeholder=\""
{-# INLINE ariaPlaceholder #-}


-- | Generates an HTML @aria-posinset@ attribute with the given value.
ariaPosinset :: Builder -> Attribute ctx
ariaPosinset = TextAttribute " aria-posinset=\""
{-# INLINE ariaPosinset #-}


-- | Generates an HTML @aria-pressed@ attribute with the given value.
ariaPressed :: Builder -> Attribute ctx
ariaPressed = TextAttribute " aria-pressed=\""
{-# INLINE ariaPressed #-}


-- | Generates an HTML @aria-readonly@ attribute with the given value.
ariaReadonly :: Builder -> Attribute ctx
ariaReadonly = TextAttribute " aria-readonly=\""
{-# INLINE ariaReadonly #-}


-- | Generates an HTML @aria-relevant@ attribute with the given value.
ariaRelevant :: Builder -> Attribute ctx
ariaRelevant = TextAttribute " aria-relevant=\""
{-# INLINE ariaRelevant #-}


-- | Generates an HTML @aria-required@ attribute with the given value.
ariaRequired :: Builder -> Attribute ctx
ariaRequired = TextAttribute " aria-required=\""
{-# INLINE ariaRequired #-}


-- | Generates an HTML @aria-roledescription@ attribute with the given value.
ariaRoledescription :: Builder -> Attribute ctx
ariaRoledescription = TextAttribute " aria-roledescription=\""
{-# INLINE ariaRoledescription #-}


-- | Generates an HTML @aria-rowcount@ attribute with the given value.
ariaRowcount :: Builder -> Attribute ctx
ariaRowcount = TextAttribute " aria-rowcount=\""
{-# INLINE ariaRowcount #-}


-- | Generates an HTML @aria-rowindex@ attribute with the given value.
ariaRowindex :: Builder -> Attribute ctx
ariaRowindex = TextAttribute " aria-rowindex=\""
{-# INLINE ariaRowindex #-}


-- | Generates an HTML @aria-rowindextext@ attribute with the given value.
ariaRowindextext :: Builder -> Attribute ctx
ariaRowindextext = TextAttribute " aria-rowindextext=\""
{-# INLINE ariaRowindextext #-}


-- | Generates an HTML @aria-rowspan@ attribute with the given value.
ariaRowspan :: Builder -> Attribute ctx
ariaRowspan = TextAttribute " aria-rowspan=\""
{-# INLINE ariaRowspan #-}


-- | Generates an HTML @aria-selected@ attribute with the given value.
ariaSelected :: Builder -> Attribute ctx
ariaSelected = TextAttribute " aria-selected=\""
{-# INLINE ariaSelected #-}


-- | Generates an HTML @aria-setsize@ attribute with the given value.
ariaSetsize :: Builder -> Attribute ctx
ariaSetsize = TextAttribute " aria-setsize=\""
{-# INLINE ariaSetsize #-}


-- | Generates an HTML @aria-sort@ attribute with the given value.
ariaSort :: Builder -> Attribute ctx
ariaSort = TextAttribute " aria-sort=\""
{-# INLINE ariaSort #-}


-- | Generates an HTML @aria-valuemax@ attribute with the given value.
ariaValuemax :: Builder -> Attribute ctx
ariaValuemax = TextAttribute " aria-valuemax=\""
{-# INLINE ariaValuemax #-}


-- | Generates an HTML @aria-valuemin@ attribute with the given value.
ariaValuemin :: Builder -> Attribute ctx
ariaValuemin = TextAttribute " aria-valuemin=\""
{-# INLINE ariaValuemin #-}


-- | Generates an HTML @aria-valuenow@ attribute with the given value.
ariaValuenow :: Builder -> Attribute ctx
ariaValuenow = TextAttribute " aria-valuenow=\""
{-# INLINE ariaValuenow #-}


-- | Generates an HTML @aria-valuetext@ attribute with the given value.
ariaValuetext :: Builder -> Attribute ctx
ariaValuetext = TextAttribute " aria-valuetext=\""
{-# INLINE ariaValuetext #-}


-- | Generates an HTML @role@ attribute with the given value.
role :: Builder -> Attribute ctx
role = TextAttribute " role=\""
{-# INLINE role #-}
