module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(Just), fromMaybe)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Types
  ( Page(Main)
  , Query(Navigate)
  , Action(SwitchPage, Initialize)
  , State
  )
import Routes (listenForUrlHashChanges, setUrlHash)
import Pages (mainPage)

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
