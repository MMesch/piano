module Types where

import Prelude
import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)

data Page
  = Main
  | Play

derive instance genericPage :: Generic Page _

-- this is required to check whether a new page corresponds to an old one.
derive instance eqPage :: Eq Page

data Query a
  = Navigate Route a

data Action
  = SwitchPage Page
  | Initialize

type State
  = { page :: Page }

type Route
  = Maybe Page
