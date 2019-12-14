module Clue exposing (decodeClues, renderCluesData)

import Array
import Cell exposing (crosswordCellisBlank, getCellFromRowCol)
import Datatypes exposing (Clue, ClueDirection(..), Clues, Grid, Msg(..))
import ElmEscapeHtml exposing (unescape)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , fail
        , field
        , int
        , list
        , map6
        , string
        , succeed
        )


decodeStartRowCol : Decoder Int
decodeStartRowCol =
    int |> andThen subtract1


subtract1 : Int -> Decoder Int
subtract1 n =
    succeed (n - 1)


decodeClue : Decoder Clue
decodeClue =
    map6 Clue
        (field "start_col" decodeStartRowCol)
        (field "start_row" decodeStartRowCol)
        (field "solution" string)
        (field "direction" decodeDirection)
        (field "number" int)
        (field "text" string)


decodeClues : Decoder Clues
decodeClues =
    list decodeClue


decodeDirection : Decoder ClueDirection
decodeDirection =
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


isActiveClue : Clue -> Maybe Int -> Clues -> Bool
isActiveClue clue activeClueIndex clues =
    case activeClueIndex of
        Just index ->
            case Array.get index (Array.fromList clues) of
                Just activeClue ->
                    if
                        (activeClue.clue_text == clue.clue_text)
                            && (activeClue.gridNumber == clue.gridNumber)
                    then
                        True

                    else
                        False

                Nothing ->
                    False

        Nothing ->
            False


clueDirectionToText : ClueDirection -> String
clueDirectionToText direction =
    case direction of
        Across ->
            "Across"

        Down ->
            "Down"


renderClueText : Clue -> String
renderClueText clue =
    unescape clue.clue_text ++ " (" ++ String.fromInt (String.length clue.solution) ++ ")"


getCurrentSolution : String -> Int -> Int -> ClueDirection -> Grid -> String
getCurrentSolution result row col direction grid =
    case getCellFromRowCol grid ( row, col ) of
        Nothing ->
            result

        Just cell ->
            if crosswordCellisBlank cell then
                result

            else
                let
                    entry =
                        case cell.entry of
                            Just currentEntry ->
                                currentEntry ++ " "

                            Nothing ->
                                "_ "
                in
                case direction of
                    Across ->
                        entry ++ getCurrentSolution result row (col + 1) direction grid

                    Down ->
                        entry ++ getCurrentSolution result (row + 1) col direction grid


getClueCurrentSolution : Clue -> Grid -> String
getClueCurrentSolution clue grid =
    case getCellFromRowCol grid ( clue.startRow, clue.startCol ) of
        Just cell ->
            getCurrentSolution "" clue.startRow clue.startCol clue.direction grid

        Nothing ->
            ""


renderClue : Clue -> Int -> Maybe Int -> Clues -> Grid -> Html Msg
renderClue clue clueIndex activeClueIndex clues grid =
    div
        [ classList
            [ ( "cluegrid-clue", True )
            , ( "cluegrid-clue-is-active", isActiveClue clue activeClueIndex clues )
            ]
        , onClick (ClueClicked clueIndex)
        ]
        [ strong [ class "cluegrid-clue-number" ] [ text (String.fromInt clue.gridNumber) ]
        , div [ class "cluegrid-clue-spacer" ] []
        , strong [ class "cluegrid-clue-direction" ] [ text (clueDirectionToText clue.direction) ]
        , div [ class "cluegrid-clue-spacer" ] []
        , div [ class "cluegrid-clue-text" ] [ text (renderClueText clue) ]
        , div [ class "cluegrid-clue-current-solution" ] [ text (getClueCurrentSolution clue grid) ]
        ]


renderCluesData : Clues -> Grid -> Maybe Int -> Html Msg
renderCluesData clues grid activeClueIndex =
    div
        [ class "cluegrid-clues-container" ]
        (List.append
            [ strong [ class "cluegrid-clues-header" ] [ text "CLUES" ] ]
            [ div [ class "cluegrid-clues-cluelist" ]
                (List.indexedMap (\index clue -> renderClue clue index activeClueIndex clues grid) clues)
            ]
        )
