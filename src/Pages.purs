module Pages where

import Prelude
import Halogen.HTML as HH
import Data.Array ((..), filter)
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as SE
import Halogen (ClassName(ClassName))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Color as Color
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int (toNumber)

-- This is a little helper to save some space
cn :: forall t5 t6. String -> HH.IProp ( class :: String | t6 ) t5
cn = HP.class_ <<< HH.ClassName

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
    [ keyboard
    , wheelOfFifths
    ]

keyboard :: forall i a. HH.HTML i a
keyboard =
  SE.svg
    [ SA.classes
        [ ClassName "svg-piano"
        ]
    , SA.viewBox 0.0 0.0 100.0 100.0
    ]
    (whiteKeys <> blackKeys)
  where
  whiteKeys =
    1 .. 30
      <#> \i ->
          SE.rect
            [ SA.x ((toNumber i) * 3.0)
            , SA.y 5.0
            , SA.height 10.0
            , SA.width 3.0
            , SA.strokeWidth 0.1
            , SA.stroke $ Color.RGB 0 0 0
            , SA.fill $ Color.RGB 255 255 255
            ]

  blackKeys =
    filter (\k -> let rem = mod k 7 in rem /= 3 && rem /= 6) (1 .. 30)
      <#> \i ->
          SE.rect
            [ SA.x (2.2 + (toNumber i) * 3.0)
            , SA.y 5.0
            , SA.height 6.0
            , SA.width 1.5
            , SA.strokeWidth 0.1
            , SA.stroke $ Color.RGB 0 0 0
            , SA.fill $ Color.RGB 0 0 0
            ]

wheelOfFifths :: forall i a. HH.HTML i a
wheelOfFifths = HH.div [] []
