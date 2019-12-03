module Main exposing (..)

import Browser
import Css
    exposing
        ( backgroundColor
        , backgroundImage
        , display
        , displayFlex
        , height
        , hex
        , linearGradient2
        , rgba
        , stop
        , toBottomRight
        , vh
        )
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , li
        , p
        , pre
        , text
        , toUnstyled
        , ul
        )
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Events exposing (onClick)
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


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view >> toUnstyled
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


decodeCell : Decoder Cell
decodeCell =
    map8 Cell
        (field "solution" string)
        (field "row" int)
        (field "col" int)
        (field "grid_number" (nullable int))
        (field "across_clue_index" (nullable int))
        (field "across_start" bool)
        (field "down_clue_index" (nullable int))
        (field "down_start" bool)


type ClueDirection
    = Across
    | Down


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
    , clue_text : String
    }


decodeClue : Decoder Clue
decodeClue =
    map6 Clue
        (field "start_col" int)
        (field "start_row" int)
        (field "solution" string)
        (field "direction" direction)
        (field "number" (nullable int))
        (field "text" string)


type alias CluegridData =
    { clues : List Clue
    , grid : List (List Cell)
    , size : CluegridSize
    , info : CluegridInfo
    }


type alias AppData =
    { cluegridData : CluegridData
    }


type Model
    = Loading
    | Failure
    | Success AppData


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


parseJSON : Decoder CluegridData
parseJSON =
    map4 CluegridData
        (field "clues"
            (list decodeClue)
        )
        (field "grid"
            (list (list decodeCell))
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
                    ( Success (AppData cluegridData), Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


renderClue : Clue -> Html Msg
renderClue clue =
    div [] [ text clue.clue_text ]


renderRow : List Cell -> Html Msg
renderRow row =
    div
        [ class "row"
        , css
            [ displayFlex
            ]
        ]
        (List.map (\cell -> p [ class "cell" ] [ renderCell cell ])
            row
        )


renderCell : Cell -> Html Msg
renderCell cell =
    div [] [ text cell.solution ]


view : Model -> Html Msg
view model =
    case model of
        Success appData ->
            div
                [ class "crossword-container"
                , css
                    [ height (vh 100)
                    , backgroundImage
                        (linearGradient2
                            toBottomRight
                            (stop (rgba 59 232 176 0.863))
                            [ stop (rgba 26 175 208 0.863)
                            , stop (rgba 106 103 206 0.863)
                            ]
                            []
                        )
                    ]
                ]
                (List.map
                    (\row -> div [] [ renderRow row ])
                    appData.cluegridData.grid
                )

        Failure ->
            text "Could not fetch data ‾\\_(ツ)_/‾"

        Loading ->
            text "loading data..."
