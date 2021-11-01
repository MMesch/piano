module Routes where

import Prelude hiding ((/))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(Just))
import Routing.Hash as RH
import Routing.Duplex as RD
import Routing.Duplex (RouteDuplex', root, optional, parse)
import Routing.Duplex.Generic (sum, noArgs)
import Routing.Duplex.Generic.Syntax ((/))
import Halogen as H
import Halogen.Query as HQ
import Types (Route, Query(Navigate))

routeCodec :: RouteDuplex' Route
routeCodec =
  root $ optional
    $ sum
        { "Main": noArgs
        , "Play": "play" / noArgs
        }

listenForUrlHashChanges ::
  forall a b.
  { query :: Query Unit -> Aff a | b } ->
  Effect (Effect Unit)
listenForUrlHashChanges halogenIO =
  RH.matchesWith (parse routeCodec) \old new -> do
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ HQ.mkTell $ Navigate new

setUrlHash :: forall m. MonadAff m => Route -> m Unit
setUrlHash = H.liftEffect <<< RH.setHash <<< RD.print routeCodec
