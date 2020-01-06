port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation exposing (Key)
import Controls exposing (handleKeyInput, renderAppData, renderHeaderRow, selectCellAndScroll, setActiveClue, updateCellData)
import Data exposing (decodeAppData)
import Datatypes exposing (AppData, CellUpdateData, ChannelName, CluegridData, Clues, Model(..), Msg(..), SocketMessage)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (field, map, string)
import Json.Encode as E
import Url exposing (Url)


port recieveCellUpdate : (CellUpdateData -> msg) -> Sub msg


port recieveKeyPress : (String -> msg) -> Sub msg


port sendRequestAllCells : String -> Cmd msg


port recieveSocketMessage : (E.Value -> msg) -> Sub msg


main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        , update = update
        , view = view
        }


getChannelName : ChannelName
getChannelName =
    "Hedwig"


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( Loading
    , Cmd.batch
        [ Http.get
            { url = "/Oct01-2019.json"
            , expect = Http.expectJson FetchedData decodeAppData
            }
        , sendRequestAllCells ""
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            case msg of
                FetchedData data ->
                    case data of
                        Ok appData ->
                            ( Loaded appData, Cmd.none )

                        Err _ ->
                            ( Failure, Cmd.none )

                _ ->
                    ( Loading, Cmd.none )

        Loaded appData ->
            case msg of
                -- TODO (13 Dec 2019 sam): See if we should pass the clues-container
                -- scroll here as a Cmd msg.
                KeyPressed key ->
                    handleKeyInput key appData

                CellClicked rowNum colNum ->
                    selectCellAndScroll appData rowNum colNum

                -- ( Loaded (selectCell appData rowNum colNum), Cmd.none )
                ClueClicked clueIndex ->
                    ( Loaded (setActiveClue appData clueIndex), Cmd.none )

                -- TODO (13 Dec 2019 sam): This means that the crossword data can
                -- only be fetched once. Will have to change this if we decide that
                -- we want to change the data.
                CellUpdate cellUpdateData ->
                    ( Loaded (updateCellData appData cellUpdateData), Cmd.none )

                _ ->
                    ( Loaded appData, Cmd.none )

        Failure ->
            ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recieveCellUpdate CellUpdate
        , recieveKeyPress KeyPressed
        , recieveSocketMessage HandleSocketMessage
        ]


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Loaded appData ->
                    renderAppData appData

                Failure ->
                    div
                        [ class "cluegrid-data-not-loaded" ]
                        [ text "Could not fetch data ‾\\_(ツ)_/‾" ]

                Loading ->
                    div
                        [ class "cluegrid-data-not-loaded" ]
                        [ text "loading data..." ]
    in
    Browser.Document "Cluegrid"
        [ div
            [ class "cluegrid-fullscreen-container" ]
            [ renderHeaderRow
            , div [ class "cluegrid-application-container" ] [ body ]
            ]
        ]
