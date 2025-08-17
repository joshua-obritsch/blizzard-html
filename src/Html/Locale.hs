-- | Module    : Html.Locale
-- Copyright   : (c) Joshua Obritsch, 2025
-- License     : MIT
-- Maintainer  : joshua@obritsch.com
-- Stability   : Experimental
--
-- The "Html.Locale" module provides a data type for representing ISO 639 language codes.
module Html.Locale
    ( -- * Primitives
      -- ** Locale
      Locale(..)
    ) where


-- PRIMITIVES


-- | Represents a locale.
data Locale

  -- | Abkhazian
  = Ab

  -- | Afar
  | Aa

  -- | Afrikaans
  | Af

  -- | Akan
  | Ak

  -- | Albanian
  | Sq

  -- | Amharic
  | Am

  -- | Arabic
  | Ar

  -- | Aragonese
  | An

  -- | Armenian
  | Hy

  -- | Assamese
  | As

  -- | Avaric
  | Av

  -- | Avestan
  | Ae

  -- | Aymara
  | Ay

  -- | Azerbaijani
  | Az

  -- | Bambara
  | Bm

  -- | Bashkir
  | Ba

  -- | Basque
  | Eu

  -- | Belarusian
  | Be

  -- | Bengali
  | Bn

  -- | Bislama
  | Bi

  -- | Bosnian
  | Bs

  -- | Breton
  | Br

  -- | Bulgarian
  | Bg

  -- | Burmese
  | My

  -- | Catalan
  | Ca

  -- | Chamorro
  | Ch

  -- | Chechen
  | Ce

  -- | Chichewa
  | Ny

  -- | Chinese
  | Zh

  -- | Church Slavonic
  | Cu

  -- | Chuvash
  | Cv

  -- | Cornish
  | Kw

  -- | Corsican
  | Co

  -- | Cree
  | Cr

  -- | Croatian
  | Hr

  -- | Czech
  | Cs

  -- | Danish
  | Da

  -- | Divehi
  | Dv

  -- | Dutch
  | Nl

  -- | Dzongkha
  | Dz

  -- | English
  | En

  -- | Esperanto
  | Eo

  -- | Estonian
  | Et

  -- | Ewe
  | Ee

  -- | Faroese
  | Fo

  -- | Fijian
  | Fj

  -- | Finnish
  | Fi

  -- | French
  | Fr

  -- | Western Frisian
  | Fy

  -- | Fulah
  | Ff

  -- | Gaelic
  | Gd

  -- | Galician
  | Gl

  -- | Ganda
  | Lg

  -- | Georgian
  | Ka

  -- | German
  | De

  -- | Greek
  | El

  -- | Kalaallisut
  | Kl

  -- | Guarani
  | Gn

  -- | Gujarati
  | Gu

  -- | Haitian
  | Ht

  -- | Hausa
  | Ha

  -- | Hebrew
  | He

  -- | Herero
  | Hz

  -- | Hindi
  | Hi

  -- | Hiri Motu
  | Ho

  -- | Hungarian
  | Hu

  -- | Icelandic
  | Is

  -- | Ido
  | Io

  -- | Igbo
  | Ig

  -- | Indonesian
  | Id

  -- | Interlingua
  | Ia

  -- | Interlingue
  | Ie

  -- | Inuktitut
  | Iu

  -- | Inupiaq
  | Ik

  -- | Irish
  | Ga

  -- | Italian
  | It

  -- | Japanese
  | Ja

  -- | Javanese
  | Jv

  -- | Kannada
  | Kn

  -- | Kanuri
  | Kr

  -- | Kashmiri
  | Ks

  -- | Kazakh
  | Kk

  -- | Central Khmer
  | Km

  -- | Kikuyu
  | Ki

  -- | Kinyarwanda
  | Rw

  -- | Kyrgyz
  | Ky

  -- | Komi
  | Kv

  -- | Kongo
  | Kg

  -- | Korean
  | Ko

  -- | Kuanyama
  | Kj

  -- | Kurdish
  | Ku

  -- | Lao
  | Lo

  -- | Latin
  | La

  -- | Latvian
  | Lv

  -- | Limburgan
  | Li

  -- | Lingala
  | Ln

  -- | Lithuanian
  | Lt

  -- | Luba-Katanga
  | Lu

  -- | Luxembourgish
  | Lb

  -- | Macedonian
  | Mk

  -- | Malagasy
  | Mg

  -- | Malay
  | Ms

  -- | Malayalam
  | Ml

  -- | Maltese
  | Mt

  -- | Manx
  | Gv

  -- | Maori
  | Mi

  -- | Marathi
  | Mr

  -- | Marshallese
  | Mh

  -- | Mongolian
  | Mn

  -- | Nauru
  | Na

  -- | Navajo
  | Nv

  -- | North Ndebele
  | Nd

  -- | South Ndebele
  | Nr

  -- | Ndonga
  | Ng

  -- | Nepali
  | Ne

  -- | Norwegian
  | No

  -- | Norwegian Bokmål
  | Nb

  -- | Norwegian Nynorsk
  | Nn

  -- | Occitan
  | Oc

  -- | Ojibwa
  | Oj

  -- | Oriya
  | Or

  -- | Oromo
  | Om

  -- | Ossetian
  | Os

  -- | Pali
  | Pi

  -- | Pashto
  | Ps

  -- | Persian
  | Fa

  -- | Polish
  | Pl

  -- | Portuguese
  | Pt

  -- | Punjabi
  | Pa

  -- | Quechua
  | Qu

  -- | Romanian
  | Ro

  -- | Romansh
  | Rm

  -- | Rundi
  | Rn

  -- | Russian
  | Ru

  -- | Northern Sami
  | Se

  -- | Samoan
  | Sm

  -- | Sango
  | Sg

  -- | Sanskrit
  | Sa

  -- | Sardinian
  | Sc

  -- | Serbian
  | Sr

  -- | Shona
  | Sn

  -- | Sindhi
  | Sd

  -- | Sinhala
  | Si

  -- | Slovak
  | Sk

  -- | Slovenian
  | Sl

  -- | Somali
  | So

  -- | Southern Sotho
  | St

  -- | Spanish
  | Es

  -- | Sundanese
  | Su

  -- | Swahili
  | Sw

  -- | Swati
  | Ss

  -- | Swedish
  | Sv

  -- | Tagalog
  | Tl

  -- | Tahitian
  | Ty

  -- | Tajik
  | Tg

  -- | Tamil
  | Ta

  -- | Tatar
  | Tt

  -- | Telugu
  | Te

  -- | Thai
  | Th

  -- | Tibetan
  | Bo

  -- | Tigrinya
  | Ti

  -- | Tonga
  | To

  -- | Tsonga
  | Ts

  -- | Tswana
  | Tn

  -- | Turkish
  | Tr

  -- | Turkmen
  | Tk

  -- | Twi
  | Tw

  -- | Uighur
  | Ug

  -- | Ukrainian
  | Uk

  -- | Urdu
  | Ur

  -- | Uzbek
  | Uz

  -- | Venda
  | Ve

  -- | Vietnamese
  | Vi

  -- | Volapük
  | Vo

  -- | Walloon
  | Wa

  -- | Welsh
  | Cy

  -- | Wolof
  | Wo

  -- | Xhosa
  | Xh

  -- | Sichuan Yi
  | Ii

  -- | Yiddish
  | Yi

  -- | Yoruba
  | Yo

  -- | Zhuang
  | Za

  -- | Zulu
  | Zu
  deriving (Eq)
