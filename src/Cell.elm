module Cell exposing (crosswordCellisBlank, decodeGrid, getCellFromRowCol, isRowColEqual, renderCell, renderGrid, updateCellEntry, updateCellInRow)

import Array exposing (Array)
import Datatypes exposing (ActiveCell, ActiveClueIndex, Cell, Grid, Msg(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, andThen, field, int, list, map7, nullable, string, succeed)


invalidCell =
    Cell "" -1 -1 Nothing Nothing Nothing (Just "")


decodeCell : Decoder Cell
decodeCell =
    map7 Cell
        (field "solution" string)
        (field "row" int)
        (field "col" int)
        (field "grid_number" (nullable int))
        (field "across_clue_index" (nullable int))
        (field "down_clue_index" (nullable int))
        (succeed Nothing)


decodeGrid : Decoder Grid
decodeGrid =
    list (list decodeCell)
        |> andThen listListCellToGrid


listListCellToGrid : List (List Cell) -> Decoder Grid
listListCellToGrid l =
    succeed
        (Array.fromList
            (List.map (\row -> Array.fromList row) l)
        )


renderRow : Array Cell -> ActiveClueIndex -> ActiveClueIndex -> ActiveCell -> Html Msg
renderRow row activeClueIndex otherActiveClueIndex activeCell =
    div
        [ class "cluegrid-crossword-row" ]
        (Array.toList
            (Array.map
                (\cell ->
                    renderCell cell
                        activeClueIndex
                        otherActiveClueIndex
                        activeCell
                )
                row
            )
        )


renderGrid : Grid -> ActiveClueIndex -> ActiveClueIndex -> ActiveCell -> Html Msg
renderGrid grid activeClueIndex otherActiveClueIndex activeCell =
    div
        [ class "cluegrid-crossword-container" ]
        (grid
            |> Array.map
                (\row ->
                    renderRow
                        row
                        activeClueIndex
                        otherActiveClueIndex
                        activeCell
                )
            |> Array.toList
        )


getCellFromRowCol : Grid -> ( Int, Int ) -> Maybe Cell
getCellFromRowCol grid ( row, col ) =
    case Array.get row grid of
        Nothing ->
            Nothing

        Just correctRow ->
            case Array.get col correctRow of
                Nothing ->
                    Nothing

                Just cell ->
                    Just cell


isRowColEqual : Cell -> Int -> Int -> Bool
isRowColEqual cell row col =
    cell.row == row && cell.col == col


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


renderCell : Cell -> ActiveClueIndex -> ActiveClueIndex -> ActiveCell -> Html Msg
renderCell cell activeClueIndex otherActiveClueIndex activeCell =
    div
        [ classList
            [ ( "cluegrid-crossword-cell", True )
            , ( "cluegrid-crossword-cell-is-blank", crosswordCellisBlank cell )
            , ( "cluegrid-crossword-cell-is-active", isActiveCell cell activeCell )
            , ( "cluegrid-crossword-cell-is-other-clue", isActiveCellClue cell otherActiveClueIndex )
            , ( "cluegrid-crossword-cell-is-active-clue", isActiveCellClue cell activeClueIndex )
            ]
        , onClick (CellClicked cell.row cell.col)
        ]
        [ div
            [ classList
                [ ( "cluegrid-crossword-cell-grid-number", True )
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
        , div [ class "cluegrid-crossword-cell-solution" ]
            [ text
                (case cell.entry of
                    Just entry ->
                        entry

                    Nothing ->
                        ""
                )
            ]
        ]


isCellEqual : Cell -> Cell -> Bool
isCellEqual cell1 cell2 =
    cell1.row == cell2.row && cell1.col == cell2.col


updateCellInRow : Cell -> Array Cell -> Maybe String -> Array Cell
updateCellInRow cellToUpdate row letter =
    Array.map
        (\cell ->
            if isCellEqual cell cellToUpdate then
                { cell | entry = letter }

            else
                cell
        )
        row


updateCellEntry : Cell -> Maybe String -> Grid -> Grid
updateCellEntry cellToUpdate letter grid =
    Array.map
        (\row -> updateCellInRow cellToUpdate row letter)
        grid
