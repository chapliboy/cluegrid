module Main exposing (..)

import Array
import Browser
import Debug exposing (log)
import Html
    exposing
        ( Html
        , button
        , div
        , li
        , p
        , pre
        , text
        )
import Html.Attributes
    exposing
        ( class
        , classList
        )
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
    , activeClueIndex : Maybe Int
    , activeCell : Maybe ( Int, Int )
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
    | CellClicked Int Int


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
                    ( Success (AppData cluegridData Nothing Nothing), Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        CellClicked rowNum colNum ->
            case model of
                Success appData ->
                    ( Success
                        (AppData
                            appData.cluegridData
                            (updateActiveClue appData.activeClueIndex
                                (getCellFromRowCol
                                    appData.cluegridData.grid
                                    ( rowNum, colNum )
                                )
                            )
                            (Just ( rowNum, colNum ))
                        )
                    , Cmd.none
                    )

                Loading ->
                    ( Loading, Cmd.none )

                Failure ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


renderClue : Clue -> Html Msg
renderClue clue =
    div [] [ text clue.clue_text ]


renderRow : List Cell -> Maybe Int -> Maybe ( Int, Int ) -> Html Msg
renderRow row activeClueIndex activeCell =
    div
        [ class "crossword-row" ]
        (List.map (\cell -> renderCell cell activeClueIndex activeCell)
            row
        )


getCellFromRowCol : List (List Cell) -> ( Int, Int ) -> Cell
getCellFromRowCol cells ( row, col ) =
    case Array.get row (Array.fromList cells) of
        Nothing ->
            -- TODO (07 Dec 2019 sam): What to do here?
            Cell "" -1 -1 Nothing Nothing False Nothing False

        Just correctRow ->
            case Array.get col (Array.fromList correctRow) of
                Nothing ->
                    -- TODO (07 Dec 2019 sam): What to do here?
                    Cell "" -1 -1 Nothing Nothing False Nothing False

                Just cell ->
                    cell


isRowColEqual : Cell -> Int -> Int -> Bool
isRowColEqual cell row col =
    cell.row == row && cell.col == col


resolveCellClueIndex : Cell -> ClueDirection -> Maybe Int
resolveCellClueIndex cell clueDirection =
    -- helper function to return the acrossClueIndex of a cell if it exists
    -- otherwise return the downClueIndex. Written so that we can avoid having
    -- to write multiple case statements to check for `Nothing`
    case clueDirection of
        Across ->
            case cell.acrossClueIndex of
                Just _ ->
                    cell.acrossClueIndex

                Nothing ->
                    cell.downClueIndex

        Down ->
            case cell.downClueIndex of
                Just _ ->
                    cell.downClueIndex

                Nothing ->
                    cell.acrossClueIndex


updateActiveClue : Maybe Int -> Cell -> Maybe Int
updateActiveClue activeClueIndex cell =
    case activeClueIndex of
        Nothing ->
            resolveCellClueIndex cell Across

        Just activeIndex ->
            if activeClueIndex == resolveCellClueIndex cell Across then
                resolveCellClueIndex cell Down

            else
                resolveCellClueIndex cell Across


crosswordCellisBlank : Cell -> Bool
crosswordCellisBlank cell =
    cell.solution == "."


crosswordCellSolution : String -> String
crosswordCellSolution solution =
    if solution == "." then
        ""

    else
        solution


isActiveCell : Cell -> Maybe ( Int, Int ) -> Bool
isActiveCell cell activeCell =
    case activeCell of
        Just ( rowNum, colNum ) ->
            rowNum == cell.row && colNum == cell.col

        Nothing ->
            False


isActiveCellClue : Cell -> Maybe Int -> Bool
isActiveCellClue cell activeClueIndex =
    case activeClueIndex of
        Just index ->
            (case cell.acrossClueIndex of
                Just clueIndex ->
                    index == clueIndex

                Nothing ->
                    False
            )
                || (case cell.downClueIndex of
                        Just clueIndex ->
                            index == clueIndex

                        Nothing ->
                            False
                   )

        Nothing ->
            False


renderCell : Cell -> Maybe Int -> Maybe ( Int, Int ) -> Html Msg
renderCell cell activeClueIndex activeCell =
    div
        [ classList
            [ ( "crossword-cell", True )
            , ( "crossword-cell-is-blank", crosswordCellisBlank cell )
            , ( "crossword-cell-is-active", isActiveCell cell activeCell )
            , ( "crossword-cell-is-active-clue", isActiveCellClue cell activeClueIndex )
            ]
        , onClick (CellClicked cell.row cell.col)
        ]
        [ div
            [ classList
                [ ( "crossword-cell-grid-number", True )
                ]
            ]
            [ text
                (case cell.gridNumber of
                    Just num ->
                        String.fromInt num

                    Nothing ->
                        ""
                )
            ]
        , div [ class "crossword-cell-solution" ]
            [ text (crosswordCellSolution cell.solution) ]
        ]


renderAppData : AppData -> Html Msg
renderAppData appData =
    div
        [ class "crossword-container" ]
        (List.map
            (\row -> renderRow row appData.activeClueIndex appData.activeCell)
            appData.cluegridData.grid
        )


view : Model -> Html Msg
view model =
    case model of
        Success appData ->
            renderAppData appData

        Failure ->
            div
                [ class "crossword-container" ]
                [ text "Could not fetch data ‾\\_(ツ)_/‾" ]

        Loading ->
            div
                [ class "crossword-container" ]
                [ text "loading data..." ]
