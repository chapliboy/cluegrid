module Main exposing (..)

import Browser
import Browser.Events
import Cell exposing (renderRow)
import Clue exposing (renderCluesData)
import Controls exposing (handleKeyInput, renderAppData, selectCell, setActiveClue)
import Data exposing (decodeCluegridData)
import Datatypes exposing (AppData, CluegridData, Clues, Msg(..))
import Debug exposing (log)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (field, map, string)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Model
    = Loading
    | Failure
    | Loaded AppData


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:8080/Oct01-2019.json"
        , expect = Http.expectJson FetchedData decodeCluegridData
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            case msg of
                FetchedData data ->
                    case data of
                        Ok cluegridData ->
                            ( Loaded (AppData cluegridData Nothing Nothing), Cmd.none )

                        Err _ ->
                            ( Failure, Cmd.none )

                _ ->
                    ( Loading, Cmd.none )

        Loaded appData ->
            case msg of
                -- TODO (13 Dec 2019 sam): See if we should pass the clues-container
                -- scroll here as a Cmd msg.
                KeyPressed key ->
                    ( Loaded (handleKeyInput key appData), Cmd.none )

                CellClicked rowNum colNum ->
                    ( Loaded (selectCell appData rowNum colNum), Cmd.none )

                ClueClicked clueIndex ->
                    ( Loaded (setActiveClue appData clueIndex), Cmd.none )

                -- TODO (13 Dec 2019 sam): This means that the crossword data can
                -- only be fetched once. Will have to change this if we decide that
                -- we want to change the data.
                _ ->
                    ( Loaded appData, Cmd.none )

        Failure ->
            ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        -- TODO (08 Dec 2019 sam): PreventDefauts somehow. Otherwise down arrow
        -- leads to scrolling, and other such UX problems. See if this can all
        -- be moved to some kind of onKeyDown eventListener on the app class.
        [ Browser.Events.onKeyDown
            (map KeyPressed
                (field "code" string)
            )
        ]


view : Model -> Html Msg
view model =
    case model of
        Loaded appData ->
            renderAppData appData

        Failure ->
            div
                [ class "cluegrid-fullscreen-container"
                , class "cluegrid-data-not-loaded"
                ]
                [ text "Could not fetch data ‾\\_(ツ)_/‾" ]

        Loading ->
            div
                [ class "cluegrid-fullscreen-container"
                , class "cluegrid-data-not-loaded"
                ]
                [ text "loading data..." ]
