module Landing exposing (renderLandingPage, requestCreateRoom)

import Browser
import Controls exposing (sendSocketMessage)
import Datatypes exposing (LandingData(..), Msg(..), SendSocketMessage)
import Html exposing (Html, a, br, div, i, img, text)
import Html.Attributes exposing (class, classList, href, src)
import Html.Events exposing (onClick)
import Json.Encode as Encode


requestCreateRoom : String -> Cmd Msg
requestCreateRoom link =
    sendSocketMessage
        (SendSocketMessage
            "create_channel"
            (Encode.string link)
        )


renderLandingPage : LandingData -> Browser.Document Msg
renderLandingPage landingData =
    let
        data =
            case landingData of
                LoadingListing key ->
                    [ div [ class "landing-page-text landing-page-loading" ] [ text "Loading ..." ] ]

                FailedJoining key ->
                    [ div [ class "landing-page-text landing-page-loading" ]
                        [ text "Could not join crossword..."
                        , br [] []
                        , text "It may not exist or it may be full..."
                        , br [] []
                        , text "Currently only two players supported"
                        , br [] []
                        , text " ‾\\_(ツ)_/‾"
                        , br [] []
                        , br [] []
                        , br [] []
                        , div [ class "fake-link", onClick (GoHome True) ] [ text "Click here to start your own puzzle" ]
                        ]
                    ]

                LoadedListing ( listing, key ) ->
                    [ div [ class "landing-page-text" ] [ text "select a crossword below and begin your cluegrid" ]
                    , div [ class "landing-page-buttons" ]
                        (listing
                            |> List.map
                                (\puzzle ->
                                    div
                                        [ class "landing-page-button"
                                        , onClick (RequestCreateRoom puzzle.link)
                                        ]
                                        [ text puzzle.title ]
                                )
                        )
                    , div [ class "landing-page-text" ] [ text "or just ask a friend for an invite link" ]
                    ]
    in
    Browser.Document "Landing"
        [ div
            [ class "cluegrid-fullscreen-container" ]
            [ img
                [ class "landing-page-logo logo"
                , src "cluegrid_logo.png"
                ]
                []
            , div
                [ class "landing-page-info-container"
                ]
                data
            , div [ class "landing-page-footer" ]
                [ div []
                    [ a [ href "https://github.com/chapliboy/cluegrid/" ] [ text "built" ]
                    , text " with "
                    , a [ href "https://elm-lang.org" ] [ text "elm" ]
                    ]
                , div []
                    [ text "data from "
                    , a [ href "https://www.xwordinfo.com/" ] [ text "xwordinfo" ]
                    ]
                , div []
                    [ text "created by "
                    , a [ href "https://samhattangady.com" ] [ text "chapliboy" ]
                    ]
                ]
            ]
        ]
