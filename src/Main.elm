module Main exposing (..)

import Array
import Browser
import Browser.Events
import Debug exposing (log)
import ElmEscapeHtml exposing (unescape)
import Html
    exposing
        ( Html
        , button
        , div
        , li
        , p
        , pre
        , strong
        , text
        )
import Html.Attributes
    exposing
        ( class
        , classList
        )
import Html.Events
    exposing
        ( keyCode
        , on
        , onClick
        , preventDefaultOn
        , stopPropagationOn
        )
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
        , map7
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


type alias CellClueStarts =
    { acrossStart : Bool
    , downStart : Bool
    }


type alias Cell =
    { solution : String
    , row : Int
    , col : Int
    , gridNumber : Maybe Int
    , acrossClueIndex : Maybe Int
    , downClueIndex : Maybe Int
    , entry : Maybe String

    -- , cellClueStarts : CellClueStarts
    }


invalidCell =
    Cell "" -1 -1 Nothing Nothing Nothing (Just "")



-- (CellClueStarts False False)


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



-- (field "across_start" bool)


type ClueDirection
    = Across
    | Down


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
    , gridNumber : Int
    , clue_text : String
    }


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


type CluegridElement
    = CrosswordElement
    | CluesElement


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
    , activeElement : CluegridElement
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
        , expect = Http.expectJson FetchedData parseJSON
        }
    )


type ArrowKeyDirection
    = ArrowKeyUp
    | ArrowKeyDown
    | ArrowKeyLeft
    | ArrowKeyRight


type ControlKey
    = SpaceBar
    | EnterKey
    | BackspaceKey


type KeyboardInput
    = LetterKey String
    | ArrowKey ArrowKeyDirection
    | ControlKey ControlKey
    | UnsupportedKey


type Msg
    = FetchedData (Result Http.Error CluegridData)
    | CellClicked Int Int
    | KeyPressed String
    | ClueClicked Int


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
    case model of
        Loading ->
            case msg of
                FetchedData data ->
                    case data of
                        Ok cluegridData ->
                            ( Loaded (AppData cluegridData Nothing Nothing CrosswordElement), Cmd.none )

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
                    let
                        updatedAppData =
                            { appData | activeElement = CrosswordElement }
                    in
                    ( Loaded (selectCell updatedAppData rowNum colNum), Cmd.none )

                ClueClicked clueIndex ->
                    let
                        updatedAppData =
                            { appData | activeElement = CluesElement }
                    in
                    ( Loaded (setActiveClue updatedAppData clueIndex), Cmd.none )

                -- TODO (13 Dec 2019 sam): This means that the crossword data can
                -- only be fetched once. Will have to change this if we decide that
                -- we want to change the data.
                _ ->
                    ( Loaded appData, Cmd.none )

        Failure ->
            ( Failure, Cmd.none )


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


handleKeyInput : String -> AppData -> AppData
handleKeyInput key appData =
    let
        keyInput =
            keyToKeyboardInput key
    in
    case appData.activeElement of
        CrosswordElement ->
            case keyInput of
                ControlKey control ->
                    case control of
                        EnterKey ->
                            toggleActiveClue appData

                        BackspaceKey ->
                            changeActiveEntry appData Nothing

                        _ ->
                            appData

                ArrowKey arrow ->
                    case arrow of
                        ArrowKeyRight ->
                            moveRight appData

                        ArrowKeyLeft ->
                            moveLeft appData

                        ArrowKeyUp ->
                            moveUp appData

                        ArrowKeyDown ->
                            moveDown appData

                LetterKey letter ->
                    changeActiveEntry appData (Just letter)

                UnsupportedKey ->
                    appData

        CluesElement ->
            case appData.activeClueIndex of
                Just clueIndex ->
                    case keyInput of
                        ArrowKey arrow ->
                            case arrow of
                                ArrowKeyUp ->
                                    setActiveClue appData (clueIndex - 1)

                                ArrowKeyDown ->
                                    setActiveClue appData (clueIndex + 1)

                                _ ->
                                    appData

                        LetterKey letter ->
                            changeActiveEntry appData (Just letter)

                        _ ->
                            appData

                Nothing ->
                    appData


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
                    case letter of
                        Just _ ->
                            moveNext (updateCellEntry cellAtRowCol letter appData)

                        Nothing ->
                            updateCellEntry cellAtRowCol letter appData

                Nothing ->
                    appData

        Nothing ->
            appData


isCellEqual : Cell -> Cell -> Bool
isCellEqual cell1 cell2 =
    cell1.row == cell2.row && cell1.col == cell2.col


updateCellInRow : Cell -> List Cell -> Maybe String -> List Cell
updateCellInRow cellToUpdate row letter =
    List.map
        (\cell ->
            if isCellEqual cell cellToUpdate then
                { cell | entry = letter }

            else
                cell
        )
        row


updateCellEntry : Cell -> Maybe String -> AppData -> AppData
updateCellEntry cellToUpdate letter appData =
    let
        newGrid =
            List.map
                (\row -> updateCellInRow cellToUpdate row letter)
                appData.cluegridData.grid

        cluegridData =
            appData.cluegridData
    in
    -- TODO (08 Dec 2019 sam): See how to update nested record like this.
    -- {appData | cluegridData.grid = newGrid }
    AppData
        { cluegridData | grid = newGrid }
        appData.activeClueIndex
        appData.activeCell
        appData.activeElement


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

    else
        UnsupportedKey


decodeKeyboardInput : Decoder KeyboardInput
decodeKeyboardInput =
    -- FIXME (08 Dec 2019 sam): Decoder doesn't seem to be working. Currently, I'm
    -- converting string to KeyboardInput in the update function instead...
    map keyToKeyboardInput (field "code" string)


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


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


renderRow : List Cell -> Maybe Int -> Maybe ( Int, Int ) -> Html Msg
renderRow row activeClueIndex activeCell =
    div
        [ class "cluegrid-crossword-row" ]
        (List.map (\cell -> renderCell cell activeClueIndex activeCell)
            row
        )


getCellFromRowCol : List (List Cell) -> ( Int, Int ) -> Maybe Cell
getCellFromRowCol cells ( row, col ) =
    case Array.get row (Array.fromList cells) of
        Nothing ->
            Nothing

        Just correctRow ->
            case Array.get col (Array.fromList correctRow) of
                Nothing ->
                    Nothing

                Just cell ->
                    Just cell


isRowColEqual : Cell -> Int -> Int -> Bool
isRowColEqual cell row col =
    cell.row == row && cell.col == col


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


updateActiveClue : Maybe Int -> Cell -> Maybe ( Int, Int ) -> List Clue -> Maybe Int
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
            [ ( "cluegrid-crossword-cell", True )
            , ( "cluegrid-crossword-cell-is-blank", crosswordCellisBlank cell )
            , ( "cluegrid-crossword-cell-is-active", isActiveCell cell activeCell )
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


isActiveClue : Clue -> Maybe Int -> List Clue -> Bool
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


getCurrentSolution : String -> Int -> Int -> ClueDirection -> CluegridData -> String
getCurrentSolution result row col direction cluegridData =
    case getCellFromRowCol cluegridData.grid ( row, col ) of
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
                        entry ++ getCurrentSolution result row (col + 1) direction cluegridData

                    Down ->
                        entry ++ getCurrentSolution result (row + 1) col direction cluegridData


getClueCurrentSolution : Clue -> CluegridData -> String
getClueCurrentSolution clue cluegridData =
    case getCellFromRowCol cluegridData.grid ( clue.startRow, clue.startCol ) of
        Just cell ->
            getCurrentSolution "" clue.startRow clue.startCol clue.direction cluegridData

        Nothing ->
            ""


renderClue : Clue -> Int -> Maybe Int -> CluegridData -> Html Msg
renderClue clue clueIndex activeClueIndex cluegridData =
    div
        [ classList
            [ ( "cluegrid-clue", True )
            , ( "cluegrid-clue-is-active", isActiveClue clue activeClueIndex cluegridData.clues )
            ]
        , onClick (ClueClicked clueIndex)
        ]
        [ strong [ class "cluegrid-clue-number" ] [ text (String.fromInt clue.gridNumber) ]
        , div [ class "cluegrid-clue-spacer" ] []
        , strong [ class "cluegrid-clue-direction" ] [ text (clueDirectionToText clue.direction) ]
        , div [ class "cluegrid-clue-spacer" ] []
        , div [ class "cluegrid-clue-text" ] [ text (renderClueText clue) ]
        , div [ class "cluegrid-clue-current-solution" ] [ text (getClueCurrentSolution clue cluegridData) ]
        ]


renderCluesData : CluegridData -> Maybe Int -> Html Msg
renderCluesData cluegridData activeClueIndex =
    div
        [ class "cluegrid-clues-container" ]
        (List.append
            [ strong [ class "cluegrid-clues-header" ] [ text "CLUES" ] ]
            [ div [ class "cluegrid-clues-cluelist" ]
                (List.indexedMap (\index clue -> renderClue clue index activeClueIndex cluegridData) cluegridData.clues)
            ]
        )


alwaysStopPropagation : Msg -> ( Msg, Bool )
alwaysStopPropagation msg =
    ( msg, False )


dontScroll : Msg -> Html.Attribute Msg
dontScroll msg =
    preventDefaultOn "keydown" (map alwaysStopPropagation (succeed msg))


onKeyDown : msg -> Html.Attribute msg
onKeyDown msg =
    on "keydown" (succeed msg)


renderAppData : AppData -> Html Msg
renderAppData appData =
    div
        [ class "cluegrid-fullscreen-container" ]
        [ div [ class "cluegrid-container" ]
            [ div
                [ class "cluegrid-crossword-container"
                ]
                (List.map
                    (\row -> renderRow row appData.activeClueIndex appData.activeCell)
                    appData.cluegridData.grid
                )
            , renderCluesData appData.cluegridData appData.activeClueIndex
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Loaded appData ->
            renderAppData appData

        Failure ->
            div
                [ class "cluegrid-fullscreen-container" ]
                [ text "Could not fetch data ‾\\_(ツ)_/‾" ]

        Loading ->
            div
                [ class "cluegrid-fullscreen-container" ]
                [ text "loading data..." ]
