module Main exposing (..)

import Browser
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , at
        , bool
        , decodeString
        , fail
        , field
        , int
        , list
        , map
        , map2
        , map4
        , map5
        , map6
        , map8
        , nullable
        , string
        , succeed
        )
import List


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Cell =
    { solution : String
    , row : Int
    , col : Int
    , gridNumber : Maybe Int
    , acrossClueIndex : Maybe Int
    , acrossStart : Bool
    , downClueIndex : Maybe Int
    , downStart : Bool
    }


type ClueDirection
    = Across
    | Down


type alias CluegridSize =
    { rows : Int
    , cols : Int
    }


type alias CluegridInfo =
    { date : String
    , title : String
    , author : String
    , editor : String
    , copyright : String
    }


type alias Clue =
    { startCol : Int
    , startRow : Int
    , solution : String
    , direction : ClueDirection
    , gridNumber : Maybe Int
    , text : String
    }


type alias CluegridData =
    { clues : List Clue
    , grid : List (List Cell)
    , size : CluegridSize
    , info : CluegridInfo
    }


type Model
    = Loading
    | Failure
    | Success CluegridData


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:8080/Oct01-2019.json"
        , expect = Http.expectJson FetchedData parseJSON
        }
    )


type Msg
    = FetchedData (Result Http.Error CluegridData)


direction : Decoder ClueDirection
direction =
    string |> andThen directionFromString


directionFromString : String -> Decoder ClueDirection
directionFromString dir =
    case dir of
        "Across" ->
            succeed Across

        "Down" ->
            succeed Down

        _ ->
            fail ("Invalid direction " ++ dir)


parseJSON : Decoder CluegridData
parseJSON =
    map4 CluegridData
        (field "clues"
            (list
                (map6 Clue
                    (field "start_col" int)
                    (field "start_row" int)
                    (field "solution" string)
                    (field "direction" direction)
                    (field "number" (nullable int))
                    (field "text" string)
                )
            )
        )
        (field "grid"
            (list
                (list
                    (map8 Cell
                        (field "solution" string)
                        (field "row" int)
                        (field "col" int)
                        (field "grid_number" (nullable int))
                        (field "across_clue_index" (nullable int))
                        (field "across_start" bool)
                        (field "down_clue_index" (nullable int))
                        (field "down_start" bool)
                    )
                )
            )
        )
        (map2 CluegridSize
            (field "size" (field "rows" int))
            (field "size" (field "cols" int))
        )
        (map5 CluegridInfo
            (field "info" (field "date" string))
            (field "info" (field "title" string))
            (field "info" (field "author" string))
            (field "info" (field "editor" string))
            (field "info" (field "copyright" string))
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedData data ->
            case data of
                Ok cluegridData ->
                    ( Success cluegridData, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Success cluegridData ->
            pre [] [ text cluegridData.info.title ]

        Failure ->
            text "Could not fetch data ‾\\_(ツ)_/‾"

        Loading ->
            text "loading data..."
