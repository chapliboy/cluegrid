module Landing exposing (renderLandingPage)

import Browser
import Datatypes exposing (Msg(..))
import Html exposing (Html, a, div, i, img, text)
import Html.Attributes exposing (class, classList, href, src)
import Html.Events exposing (onClick)


renderLandingPage : Browser.Document Msg
renderLandingPage =
    Browser.Document "Landing"
        [ div
            [ class "cluegrid-fullscreen-container" ]
            [ img
                [ class "landing-page-logo"
                , src "cluegrid_logo.png"
                ]
                []
            , div
                [ class "box"
                , onClick GoToPuzzle
                ]
                [ text "box" ]
            ]
        ]
