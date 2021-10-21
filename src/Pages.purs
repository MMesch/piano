module Pages where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

{- pages -}
-- This is a little helper to save some space
cn :: forall t5 t6. String -> HH.IProp ( class :: String | t6 ) t5
cn = HP.class_ <<< HH.ClassName

mainPage :: forall i a. HH.HTML i a
mainPage =
  HH.div [ cn "flex flex-wrap justify-center content-center align-center w-screen h-screen" ]
    [ HH.a [ cn "rounded-lg px-10 py-10 block bg-red-500 text-5xl" ]
        [ HH.text "Play"
        ]
    ]
