port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation exposing (Key)
import Controls exposing (checkActiveClue, handleKeyInput, handleLandingSocketMessage, handlePuzzleSocketMessage, renderAppData, renderHeaderRow, selectCellAndScroll, sendRequestAllCells, sendSocketMessage, setActiveClue, solveActiveClue, updateCellData, updateOtherClue)
import Data exposing (decodeAppData)
import Datatypes exposing (ActiveClueIndex, AppData, CellUpdateData, ChannelDetails, ChannelName, Clues, LandingData(..), ModalContents(..), Model(..), Msg(..), PageData, PuzzleData(..), RecieveSocketMessage, SendSocketMessage)
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (field, map, string)
import Json.Encode as Encode
import Landing exposing (renderLandingPage, requestCreateRoom)
import Url exposing (Url)


port recieveKeyPress : (String -> msg) -> Sub msg


port recieveSocketMessage : (RecieveSocketMessage -> msg) -> Sub msg


main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        , update = update
        , view = view
        }


sendRequestCrosswordListing : Cmd msg
sendRequestCrosswordListing =
    sendSocketMessage (SendSocketMessage "request_all_crosswords" Encode.null)


sendLeaveRoom : Cmd msg
sendLeaveRoom =
    sendSocketMessage (SendSocketMessage "leave_room" Encode.null)


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    NoOp


sendJoinChannel : String -> Cmd Msg
sendJoinChannel channelName =
    sendSocketMessage
        (SendSocketMessage
            "join_room"
            (Encode.string channelName)
        )


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    case url.path of
        "/" ->
            ( LandingPage (LoadingListing (PageData url key))
            , sendRequestCrosswordListing
            )

        path ->
            ( LandingPage (LoadingListing (PageData url key))
            , sendJoinChannel path
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        LandingPage landingData ->
            let
                pageData =
                    case landingData of
                        LoadingListing pd ->
                            pd

                        FailedJoining pd ->
                            pd

                        LoadedListing ( _, pd ) ->
                            pd
            in
            case msg of
                RequestCreateRoom link ->
                    ( LandingPage landingData, requestCreateRoom link )

                HandleSocketMessage message ->
                    handleLandingSocketMessage message landingData

                GoHome ->
                    ( LandingPage (LoadingListing pageData)
                    , Cmd.batch
                        [ Browser.Navigation.pushUrl pageData.key "/"
                        , sendRequestCrosswordListing
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        PuzzlePage puzzleData ->
            case puzzleData of
                Failure channelDetails ->
                    ( PuzzlePage puzzleData, Cmd.none )

                Loading channelDetails ->
                    case msg of
                        FetchedData data ->
                            case data of
                                Ok appData ->
                                    ( PuzzlePage (Loaded ( channelDetails, appData )), sendRequestAllCells )

                                Err _ ->
                                    ( PuzzlePage (Failure channelDetails), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Loaded ( channelDetails, appData ) ->
                    case msg of
                        KeyPressed key ->
                            handleKeyInput key ( channelDetails, appData )

                        CellClicked rowNum colNum ->
                            selectCellAndScroll ( channelDetails, appData ) rowNum colNum

                        ClueClicked clueIndex ->
                            ( PuzzlePage (Loaded ( channelDetails, setActiveClue appData clueIndex )), Cmd.none )

                        CloseModal ->
                            ( PuzzlePage
                                (Loaded ( channelDetails, { appData | modal = Empty } ))
                            , Cmd.none
                            )

                        SetModalInfo ->
                            ( PuzzlePage
                                (Loaded ( channelDetails, { appData | modal = Info } ))
                            , Cmd.none
                            )

                        SetModalInvite ->
                            ( PuzzlePage
                                (Loaded ( channelDetails, { appData | modal = Invite } ))
                            , Cmd.none
                            )

                        SolveActiveClue ->
                            solveActiveClue ( channelDetails, appData )

                        CheckActiveClue ->
                            checkActiveClue ( channelDetails, appData )

                        HandleSocketMessage message ->
                            handlePuzzleSocketMessage message ( channelDetails, appData )

                        GoHome ->
                            case channelDetails.pageData of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just pageData ->
                                    ( LandingPage (LoadingListing pageData)
                                    , Cmd.batch
                                        [ Browser.Navigation.pushUrl pageData.key "/"
                                        , sendLeaveRoom
                                        ]
                                    )

                        _ ->
                            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recieveKeyPress KeyPressed
        , recieveSocketMessage HandleSocketMessage
        ]


view : Model -> Browser.Document Msg
view model =
    case model of
        LandingPage landingData ->
            renderLandingPage landingData

        PuzzlePage puzzleData ->
            let
                body =
                    case puzzleData of
                        Loaded ( channelDetails, appData ) ->
                            renderAppData ( channelDetails, appData )

                        Failure channelDetails ->
                            div
                                [ class "cluegrid-data-not-loaded" ]
                                [ text "Could not fetch data ‾\\_(ツ)_/‾" ]

                        Loading channelDetails ->
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
