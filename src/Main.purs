module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.String (stripPrefix, Pattern(Pattern))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root, optional, parse)
import Routing.Duplex as RD
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Hash as RH
import Halogen.Aff as HA
import Halogen.Query as HQ
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

{-
Types
-}
data Page
  = Main

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

{- routing functionality -}
routeCodec :: RouteDuplex' Route
routeCodec =
  root $ optional
    $ sum
        { "Main": noArgs }

listenForUrlHashChanges ::
  forall a b.
  { query :: Query Unit -> Aff a | b } ->
  Effect (Effect Unit)
listenForUrlHashChanges halogenIO =
  RH.matchesWith (stripBang >>> parse routeCodec) \old new -> do
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ HQ.mkTell $ Navigate new

setUrlHash :: forall m. MonadAff m => Route -> m Unit
setUrlHash = H.liftEffect <<< RH.setHash <<< addBang <<< RD.print routeCodec

addBang :: String -> String
addBang = (<>) "!"

stripBang :: String -> String
stripBang str = fromMaybe str $ stripPrefix (Pattern "!") str

{- main component -}
main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitLoad
    body <- HA.awaitBody
    halogenIO <- runUI component unit body
    canceller <- H.liftEffect $ listenForUrlHashChanges halogenIO
    pure unit

component :: forall i. H.Component Query i Void Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  initialState :: forall a. a -> State
  initialState _ = { page: Main }

  handleQuery :: forall c a. Query a -> H.HalogenM State Action c Void Aff (Maybe a)
  handleQuery = case _ of
    Navigate (destPage) a -> do
      { page } <- H.get
      when (Just page /= destPage) do
        H.modify_ \state ->
          state
            { page = fromMaybe Main destPage }
      pure $ Just a

  handleAction :: forall c. Action -> H.HalogenM State Action c Void Aff Unit
  handleAction = case _ of
    SwitchPage page -> do
      state <- H.get
      setUrlHash (Just page)
    Initialize -> pure unit

  render :: forall c. State -> H.ComponentHTML Action c Aff
  render state = case state.page of
    Main -> mainPage

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
