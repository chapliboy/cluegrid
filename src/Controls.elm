port module Controls exposing (handleKeyInput, renderAppData, renderHeaderRow, selectCell, selectCellAndScroll, setActiveClue, updateCellData)

import Array
import Browser.Dom as Dom
import Cell exposing (crosswordCellisBlank, getCellFromRowCol, isRowColEqual, renderCell, renderGrid, updateCellEntry)
import Clue exposing (getClueId, renderCluesData)
import Datatypes exposing (AppData, ArrowKeyDirection(..), Cell, CellUpdateData, ClueDirection(..), CluegridData, Clues, ControlKey(..), KeyboardInput(..), Model(..), Msg(..), RowCol, SocketMessage)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Task



-- port sendMessage : SocketMessage -> Cmd msg


port sendCellUpdate : CellUpdateData -> Cmd msg


updateCellData : AppData -> CellUpdateData -> AppData
updateCellData appData cellUpdateData =
    case getCellFromRowCol appData.grid ( cellUpdateData.cell.row, cellUpdateData.cell.col ) of
        Nothing ->
            appData

        Just cell ->
            let
                newGrid =
                    updateCellEntry cell
                        cellUpdateData.letter
                        appData.grid
            in
            { appData | grid = newGrid }


setActiveClue : AppData -> Int -> AppData
setActiveClue appData clueIndex =
    case Array.get clueIndex (Array.fromList appData.clues) of
        Just clue ->
            case
                getCellFromRowCol
                    appData.grid
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


getClueYPosition : String -> String -> Int
getClueYPosition clueId scrollAreaId =
    0


scrollToClue : AppData -> Cmd Msg
scrollToClue appData =
    -- Because of the way our app is designed, we have some trouble getting the
    -- exact scroll position of the clue. What we need to do is to get the x and y
    -- position of the scrollarea. We also need to get the scroll position. For this
    -- we need to use both getElement and getViewport. This whole thing would be a
    -- lot simpler if we could set the parent in getElement
    -- TODO (26 Dec 2019 sam): Clean up the code, make it less nested
    let
        clueIndex =
            case appData.activeClueIndex of
                Just index ->
                    index

                Nothing ->
                    0
    in
    getClueId clueIndex
        |> Dom.getElement
        |> Task.andThen
            (\clue ->
                Dom.getElement "cluegrid-clues-scrollable-area"
                    |> Task.andThen
                        (\scrollAreaElement ->
                            Dom.getViewportOf "cluegrid-clues-scrollable-area"
                                |> Task.andThen
                                    (\scrollAreaViewport ->
                                        let
                                            scrollPortHeight =
                                                getScrollPortHeight scrollAreaViewport clue scrollAreaElement
                                        in
                                        Dom.setViewportOf "cluegrid-clues-scrollable-area" 0 scrollPortHeight
                                    )
                        )
            )
        |> Task.attempt (\_ -> SetScroll)


getScrollPortHeight : Dom.Viewport -> Dom.Element -> Dom.Element -> Float
getScrollPortHeight viewport clue scrollArea =
    if clue.element.y < scrollArea.element.y then
        -- clue is above... scroll up
        viewport.viewport.y + clue.element.y - scrollArea.element.y

    else if (clue.element.y + clue.element.height) > (scrollArea.element.height + scrollArea.element.y) then
        -- clue is below... scroll down
        viewport.viewport.y + clue.element.y + clue.element.height - scrollArea.element.y - scrollArea.element.height

    else
        -- clue is in window... don't scroll
        viewport.viewport.y


sendScrollToClue : AppData -> ( Model, Cmd Msg )
sendScrollToClue appData =
    ( Loaded appData, scrollToClue appData )


sendUpdateData : Maybe String -> Int -> Int -> AppData -> ( Model, Cmd Msg )
sendUpdateData letter row col appData =
    let
        cellUpdateData =
            CellUpdateData (RowCol row col) letter
    in
    ( Loaded appData
    , Cmd.batch
        [ sendCellUpdate cellUpdateData
        , scrollToClue appData
        ]
    )


changeClueIndex : AppData -> Int -> AppData
changeClueIndex appData change =
    let
        clueIndex =
            case appData.activeClueIndex of
                Nothing ->
                    0

                Just index ->
                    modBy (List.length appData.clues) (index + change)
    in
    setActiveClue appData clueIndex


selectNextClue : AppData -> AppData
selectNextClue appData =
    changeClueIndex appData 1


selectPreviousClue : AppData -> AppData
selectPreviousClue appData =
    changeClueIndex appData -1


handleKeyInput : String -> AppData -> ( Model, Cmd Msg )
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
                        |> sendScrollToClue

                BackspaceKey ->
                    let
                        ( row, col ) =
                            case appData.activeCell of
                                Nothing ->
                                    ( -1, -1 )

                                Just ( r, c ) ->
                                    ( r, c )
                    in
                    changeActiveEntry appData Nothing
                        |> sendUpdateData Nothing row col

                TabKey ->
                    selectNextClue appData
                        |> sendScrollToClue

                ShiftTabKey ->
                    selectPreviousClue appData
                        |> sendScrollToClue

                _ ->
                    appData
                        |> sendScrollToClue

        ArrowKey arrow ->
            case arrow of
                ArrowKeyRight ->
                    moveRight appData
                        |> sendScrollToClue

                ArrowKeyLeft ->
                    moveLeft appData
                        |> sendScrollToClue

                ArrowKeyUp ->
                    moveUp appData
                        |> sendScrollToClue

                ArrowKeyDown ->
                    moveDown appData
                        |> sendScrollToClue

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
                |> sendUpdateData (Just letter) row col

        UnsupportedKey ->
            appData
                |> sendScrollToClue


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
            case getCellFromRowCol appData.grid ( row, col ) of
                Just cellAtRowCol ->
                    let
                        grid =
                            updateCellEntry cellAtRowCol letter appData.grid

                        updatedAppData =
                            { appData | grid = grid }
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
            case getCellFromRowCol appData.grid ( row + rowChange, col + colChange ) of
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
            case Array.get clueIndex (Array.fromList appData.clues) of
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
            case Array.get clueIndex (Array.fromList appData.clues) of
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
    case getCellFromRowCol appData.grid ( rowNum, colNum ) of
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
                            appData.clues
                    , activeCell = Just ( rowNum, colNum )
                }

        Nothing ->
            appData


selectCellAndScroll : AppData -> Int -> Int -> ( Model, Cmd Msg )
selectCellAndScroll appData rowNum colNum =
    selectCell appData rowNum colNum
        |> sendScrollToClue


renderHeaderCell : ( String, Maybe Int ) -> Html Msg
renderHeaderCell ( letter, num ) =
    let
        solution =
            case letter of
                "" ->
                    "."

                _ ->
                    ""
    in
    renderCell
        (Cell
            solution
            -1
            -1
            num
            Nothing
            Nothing
            (Just letter)
        )
        Nothing
        Nothing


renderHeaderRow : Html Msg
renderHeaderRow =
    div
        [ class "cluegrid-header-row" ]
        ([ ( "C", Just 3 )
         , ( "L", Nothing )
         , ( "U", Nothing )
         , ( "E", Nothing )
         , ( "", Nothing )
         , ( "G", Just 7 )
         , ( "R", Nothing )
         , ( "I", Nothing )
         , ( "D", Nothing )
         ]
            |> List.map (\letter -> renderHeaderCell letter)
        )


renderAppData : AppData -> Html Msg
renderAppData appData =
    div
        [ class "cluegrid-container" ]
        [ renderGrid appData.grid appData.activeClueIndex appData.activeCell
        , renderCluesData appData.clues appData.grid appData.activeClueIndex
        ]
