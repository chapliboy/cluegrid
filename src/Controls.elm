port module Controls exposing (handleKeyInput, renderAppData, selectCell, setActiveClue, updateCellData)

import Array
import Cell exposing (crosswordCellisBlank, getCellFromRowCol, isRowColEqual, renderRow, updateCellEntry)
import Clue exposing (renderCluesData)
import Datatypes exposing (AppData, ArrowKeyDirection(..), Cell, CellUpdateData, ClueDirection(..), CluegridData, Clues, ControlKey(..), KeyboardInput(..), Model(..), Msg(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)


port sendCellUpdate : CellUpdateData -> Cmd msg


updateCellData : AppData -> CellUpdateData -> AppData
updateCellData appData cellUpdateData =
    case getCellFromRowCol appData.cluegridData.grid ( cellUpdateData.row, cellUpdateData.col ) of
        Nothing ->
            appData

        Just cell ->
            let
                newGrid =
                    updateCellEntry cell
                        (Just cellUpdateData.letter)
                        appData.cluegridData.grid

                cluegridData =
                    appData.cluegridData

                newCluegrid =
                    { cluegridData | grid = newGrid }
            in
            { appData | cluegridData = newCluegrid }


setActiveClue : AppData -> Int -> AppData
setActiveClue appData clueIndex =
    case Array.get clueIndex (Array.fromList appData.cluegridData.clues) of
        Just clue ->
            case
                getCellFromRowCol
                    appData.cluegridData.grid
                    ( clue.startRow, clue.startCol )
            of
                Just cell ->
                    case appData.activeClueIndex of
                        Just index ->
                            if index == clueIndex then
                                appData

                            else
                                selectCell
                                    { appData | activeClueIndex = Just clueIndex }
                                    cell.row
                                    cell.col

                        Nothing ->
                            -- TODO (08 Dec 2019 sam): See how this code-duplication
                            -- can be eliminated
                            selectCell
                                { appData | activeClueIndex = Just clueIndex }
                                cell.row
                                cell.col

                Nothing ->
                    appData

        Nothing ->
            appData


noCmdLoaded : AppData -> ( Model, Cmd msg )
noCmdLoaded appData =
    ( Loaded appData, Cmd.none )


sendUpdateData : String -> Int -> Int -> AppData -> ( Model, Cmd msg )
sendUpdateData letter row col appData =
    let
        cellUpdateData =
            CellUpdateData row col letter
    in
    ( Loaded appData, sendCellUpdate cellUpdateData )


changeClueIndex : AppData -> Int -> AppData
changeClueIndex appData change =
    let
        clueIndex =
            case appData.activeClueIndex of
                Nothing ->
                    0

                Just index ->
                    modBy (List.length appData.cluegridData.clues) (index + change)
    in
    setActiveClue appData clueIndex


selectNextClue : AppData -> AppData
selectNextClue appData =
    changeClueIndex appData 1


selectPreviousClue : AppData -> AppData
selectPreviousClue appData =
    changeClueIndex appData -1


handleKeyInput : String -> AppData -> ( Model, Cmd msg )
handleKeyInput key appData =
    let
        keyInput =
            keyToKeyboardInput key
    in
    case keyInput of
        ControlKey control ->
            case control of
                EnterKey ->
                    toggleActiveClue appData
                        |> noCmdLoaded

                BackspaceKey ->
                    changeActiveEntry appData Nothing
                        |> noCmdLoaded

                TabKey ->
                    selectNextClue appData
                        |> noCmdLoaded

                ShiftTabKey ->
                    selectPreviousClue appData
                        |> noCmdLoaded

                _ ->
                    appData
                        |> noCmdLoaded

        ArrowKey arrow ->
            case arrow of
                ArrowKeyRight ->
                    moveRight appData
                        |> noCmdLoaded

                ArrowKeyLeft ->
                    moveLeft appData
                        |> noCmdLoaded

                ArrowKeyUp ->
                    moveUp appData
                        |> noCmdLoaded

                ArrowKeyDown ->
                    moveDown appData
                        |> noCmdLoaded

        LetterKey letter ->
            let
                ( row, col ) =
                    case appData.activeCell of
                        Nothing ->
                            ( -1, -1 )

                        Just ( r, c ) ->
                            ( r, c )
            in
            changeActiveEntry appData (Just letter)
                |> sendUpdateData letter row col

        UnsupportedKey ->
            appData
                |> noCmdLoaded


toggleActiveClue : AppData -> AppData
toggleActiveClue appData =
    case appData.activeCell of
        Just ( row, col ) ->
            selectCell appData row col

        Nothing ->
            appData


changeActiveEntry : AppData -> Maybe String -> AppData
changeActiveEntry appData letter =
    case appData.activeCell of
        Just ( row, col ) ->
            case getCellFromRowCol appData.cluegridData.grid ( row, col ) of
                Just cellAtRowCol ->
                    let
                        grid =
                            updateCellEntry cellAtRowCol letter appData.cluegridData.grid

                        cluegridData =
                            appData.cluegridData

                        updatedClueGridData =
                            { cluegridData | grid = grid }

                        updatedAppData =
                            { appData | cluegridData = updatedClueGridData }
                    in
                    case letter of
                        Just _ ->
                            moveNext updatedAppData

                        Nothing ->
                            movePrevious updatedAppData

                Nothing ->
                    appData

        Nothing ->
            appData


moveCell : AppData -> AppData -> Int -> Int -> AppData
moveCell originalAppData appData rowChange colChange =
    case appData.activeCell of
        Nothing ->
            selectCell appData 0 0

        Just ( row, col ) ->
            case getCellFromRowCol appData.cluegridData.grid ( row + rowChange, col + colChange ) of
                Just cellAtRowCol ->
                    if crosswordCellisBlank cellAtRowCol then
                        moveCell
                            originalAppData
                            { appData
                                | activeCell =
                                    Just ( row + rowChange, col + colChange )
                            }
                            rowChange
                            colChange

                    else
                        selectCell appData (row + rowChange) (col + colChange)

                Nothing ->
                    originalAppData


moveRight : AppData -> AppData
moveRight appData =
    moveCell appData appData 0 1


moveLeft : AppData -> AppData
moveLeft appData =
    moveCell appData appData 0 -1


moveUp : AppData -> AppData
moveUp appData =
    moveCell appData appData -1 0


moveDown : AppData -> AppData
moveDown appData =
    moveCell appData appData 1 0


moveNext : AppData -> AppData
moveNext appData =
    case appData.activeClueIndex of
        Just clueIndex ->
            case Array.get clueIndex (Array.fromList appData.cluegridData.clues) of
                Just clue ->
                    case clue.direction of
                        Across ->
                            moveRight appData

                        Down ->
                            moveDown appData

                Nothing ->
                    appData

        Nothing ->
            appData


movePrevious : AppData -> AppData
movePrevious appData =
    case appData.activeClueIndex of
        Just clueIndex ->
            case Array.get clueIndex (Array.fromList appData.cluegridData.clues) of
                Just clue ->
                    case clue.direction of
                        Across ->
                            moveLeft appData

                        Down ->
                            moveUp appData

                Nothing ->
                    appData

        Nothing ->
            appData


keyToKeyboardInput : String -> KeyboardInput
keyToKeyboardInput code =
    let
        _ =
            Debug.log "arrow code" code
    in
    if String.startsWith "Arrow" code then
        case code of
            "ArrowRight" ->
                ArrowKey ArrowKeyRight

            "ArrowLeft" ->
                ArrowKey ArrowKeyLeft

            "ArrowUp" ->
                ArrowKey ArrowKeyUp

            "ArrowDown" ->
                ArrowKey ArrowKeyDown

            _ ->
                UnsupportedKey

    else if String.startsWith "Key" code then
        LetterKey (String.slice 3 4 code)

    else if code == "Enter" then
        ControlKey EnterKey

    else if code == "Backspace" then
        ControlKey BackspaceKey

    else if code == "Tab" then
        ControlKey TabKey

    else if code == "ShiftTab" then
        ControlKey ShiftTabKey

    else
        UnsupportedKey



-- decodeKeyboardInput : Decoder KeyboardInput
-- decodeKeyboardInput =
--     -- FIXME (08 Dec 2019 sam): Decoder doesn't seem to be working. Currently, I'm
--     -- converting string to KeyboardInput in the update function instead...
--     map keyToKeyboardInput (field "code" string)


resolveCellClueIndex : Cell -> ClueDirection -> Maybe Int
resolveCellClueIndex cell clueDirection =
    {-
       helper function to return the clueDirectionIndex of a cell if it exists
       otherwise return the otherDirection. Written so that we can avoid having
       to write multiple case statements to check for `Nothing`
    -}
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


updateActiveClue : Maybe Int -> Cell -> Maybe ( Int, Int ) -> Clues -> Maybe Int
updateActiveClue activeClueIndex cell activeCell clues =
    {-
       Based on which the new selected cell is, the active clue would have to be
       updated accordingly.
            If there was no clue selected, then select the Across clue of the
        selected cell.
            If a clue was previously selected, we get the direction of that clue.
            We then check whether the selected cell was already active.
                If it was active, then toggle the clueDirection
                If it wasn't active, select the currentDirection clue of selectedCell
    -}
    case activeClueIndex of
        Nothing ->
            resolveCellClueIndex cell Across

        Just activeIndex ->
            let
                currentDirection =
                    case Array.get activeIndex (Array.fromList clues) of
                        Just clue ->
                            clue.direction

                        Nothing ->
                            Across

                otherDirection =
                    case currentDirection of
                        Across ->
                            Down

                        Down ->
                            Across

                cellReclicked =
                    case activeCell of
                        Nothing ->
                            False

                        Just ( row, col ) ->
                            if isRowColEqual cell row col then
                                True

                            else
                                False
            in
            if cellReclicked then
                resolveCellClueIndex cell otherDirection

            else
                resolveCellClueIndex cell currentDirection


selectCell : AppData -> Int -> Int -> AppData
selectCell appData rowNum colNum =
    case getCellFromRowCol appData.cluegridData.grid ( rowNum, colNum ) of
        Just cellAtRowCol ->
            if crosswordCellisBlank cellAtRowCol then
                appData

            else
                { appData
                    | activeClueIndex =
                        updateActiveClue
                            appData.activeClueIndex
                            cellAtRowCol
                            appData.activeCell
                            appData.cluegridData.clues
                    , activeCell = Just ( rowNum, colNum )
                }

        Nothing ->
            appData


renderAppData : AppData -> Html Msg
renderAppData appData =
    div
        [ class "cluegrid-fullscreen-container" ]
        [ div [ class "cluegrid-container" ]
            [ div
                [ class "cluegrid-crossword-container"
                ]
                (Array.toList
                    (Array.map
                        (\row -> renderRow row appData.activeClueIndex appData.activeCell)
                        appData.cluegridData.grid
                    )
                )
            , renderCluesData appData.cluegridData.clues appData.cluegridData.grid appData.activeClueIndex
            ]
        ]
