module Pages where

import Prelude
import Halogen.HTML as HH
import Data.Array ((..), elem, filter, head, mapWithIndex)
import Data.Foldable (foldl)
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as SE
import Halogen (ClassName(ClassName))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Color as Color
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Data.Int (toNumber, floor, ceil)
import Debug

-- This is a little helper to save some space
cn :: forall t5 t6. String -> HH.IProp ( class :: String | t6 ) t5
cn = HP.class_ <<< HH.ClassName

-- pages
mainPage :: forall i a. HH.HTML i a
mainPage =
  HH.div [ cn "flex flex-wrap justify-center content-center align-center w-screen h-screen" ]
    [ HH.a
        [ HP.href "#/play"
        , cn "rounded-lg px-10 py-10 block bg-red-500 text-5xl"
        ]
        [ HH.text "Play"
        ]
    ]

pianoPage :: forall i a. HH.HTML i a
pianoPage =
  HH.div []
    [ keyboard "G#"
    , wheelOfFifths
    ]

type Key
  = { pitch :: Int
    , position :: Number
    , isBlack :: Boolean
    }

-- elements
keyboard :: forall i a. String -> HH.HTML i a
keyboard tonality =
  SE.svg
    [ SA.classes
        [ ClassName "svg-piano"
        ]
    , SA.viewBox 0.0 0.0 100.0 100.0
    ]
    ((keys <$> whiteKeyParams) <> (keys <$> blackKeyParams))
  where
  nOctaves = 4

  pitches = 0 .. (12 * nOctaves)

  blackKeyPitches = [ 1, 3, 6, 8, 10 ]

  whiteKeyPitches = [ 0, 2, 4, 5, 7, 9, 11 ]

  whiteKeys = filter (\k -> elem (mod k 12) whiteKeyPitches) pitches

  blackKeys = filter (\k -> elem (mod k 12) blackKeyPitches) pitches

  whiteKeyParams =
    mapWithIndex
      ( \iWhite iPitch ->
          { pitch: iPitch
          , position: (toNumber iWhite) * 3.0
          , isBlack: false
          }
      )
      whiteKeys

  blackKeyParams =
    mapWithIndex
      ( \iBlack iPitch ->
          { pitch: iPitch
          , position: 2.2 + (toNumber (iBlack + ceil (toNumber (iPitch - 3) / 12.0) + ceil (toNumber (iPitch - 10) / 12.0)) * 3.0)
          , isBlack: true
          }
      )
      blackKeys

  -- -2 -1    0  1  2
  -- 0  1     2  3  4
  -- x  x  _  x  x  x  _ 
  keys param =
    SE.rect
      [ SA.x param.position
      , SA.y 5.0
      , SA.height $ if param.isBlack then 6.0 else 10.0
      , SA.width $ if param.isBlack then 1.5 else 3.0
      , SA.strokeWidth 0.1
      , SA.stroke $ Color.RGB 0 0 0
      , SA.fill $ if param.isBlack then blackColor else whiteColor
      ]

  blackColor = Color.RGB 0 0 0

  whiteColor = Color.RGB 255 255 255

wheelOfFifths :: forall i a. HH.HTML i a
wheelOfFifths = HH.div [] []
