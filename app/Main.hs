-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html
import           Miso.Html.Property
import           Miso.Lens
import           Miso.Navigator
-----------------------------------------------------------------------------
data Action
  = GetLocation
  | ErrorLocation GeolocationError
  | SetLocation Geolocation
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = run $ startApp app
-----------------------------------------------------------------------------
type Model = Maybe Geolocation
-----------------------------------------------------------------------------
app :: App Model Action
app = (component Nothing updateModel viewModel)
#ifndef WASM
  { styles =
    [ Href "assets/style.css"
    ]
  }
#endif
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel = \case
  GetLocation -> do
    geolocation SetLocation ErrorLocation
  SetLocation location ->
    this ?= location
  ErrorLocation (GeolocationError _ err) ->
    io_ (consoleError err)
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel x = 
  div_
    [class_ "container"]
    [ div_ [class_ "location-icon"] ["🍜 📍"]
    , h1_ [] ["Geolocation Demo"]
    , p_
        []
        [ "Click the button below to retrieve your current location. We'll display your latitude, longitude, and accuracy."
        ]
    , button_
      [ onClick GetLocation
      ]
      [ "Get My Location"
      ]
    , div_
        [ class_ "location-data"
        ] $
        [ h2_
          []
          [ "Your Location Data"
          ]
        ] ++ concat
        [
          [ div_
            [ class_ "data-item" ]
            [ span_ [class_ "data-label"] ["Latitude:"]
            , span_ [id_ "latitude", class_ "data-value"] [ text (ms latitude) ]
            ]
        , div_
            [class_ "data-item"]
            [ span_ [class_ "data-label"] ["Longitude:"]
            , span_ [id_ "longitude", class_ "data-value"] [ text (ms longitude) ]
            ]
        , div_
            [class_ "data-item"]
            [ span_ [class_ "data-label"] ["Accuracy:"]
            , span_ [id_ "accuracy", class_ "data-value"] [ text (ms accuracy) ]
            ]
          ]
        | Just Geolocation {..} <- pure x
        ]
    ]
-----------------------------------------------------------------------------
