module Main exposing (..)

import Array
import Browser
import Browser.Events
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
import Html.Events
    exposing
        ( keyCode
        , on
        , onClick
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
                    ( Success (selectCell appData rowNum colNum), Cmd.none )

                Loading ->
                    ( Loading, Cmd.none )

                Failure ->
                    ( Failure, Cmd.none )

        KeyPressed key ->
            let
                _ =
                    Debug.log "keyPressed" key

                keyInput =
                    keyToKeyboardInput key
            in
            case model of
                Success appData ->
                    case keyInput of
                        UnsupportedKey ->
                            ( Success appData, Cmd.none )

                        ControlKey control ->
                            case control of
                                EnterKey ->
                                    ( Success (toggleActiveClue appData), Cmd.none )

                                _ ->
                                    ( Success appData, Cmd.none )

                        ArrowKey arrow ->
                            case arrow of
                                ArrowKeyRight ->
                                    ( Success (moveRight appData), Cmd.none )

                                ArrowKeyLeft ->
                                    ( Success (moveLeft appData), Cmd.none )

                                ArrowKeyUp ->
                                    ( Success (moveUp appData), Cmd.none )

                                ArrowKeyDown ->
                                    ( Success (moveDown appData), Cmd.none )

                        LetterKey letter ->
                            let
                                _ =
                                    Debug.log "letter pressed" letter
                            in
                            ( Success (changeActiveEntry appData letter), Cmd.none )

                Loading ->
                    ( Loading, Cmd.none )

                Failure ->
                    ( Failure, Cmd.none )


selectCell : AppData -> Int -> Int -> AppData
selectCell appData rowNum colNum =
    case getCellFromRowCol appData.cluegridData.grid ( rowNum, colNum ) of
        Just cellAtRowCol ->
            AppData
                appData.cluegridData
                (updateActiveClue
                    appData.activeClueIndex
                    cellAtRowCol
                    appData.activeCell
                    appData.cluegridData.clues
                )
                (Just ( rowNum, colNum ))

        Nothing ->
            appData


toggleActiveClue : AppData -> AppData
toggleActiveClue appData =
    case appData.activeCell of
        Just ( row, col ) ->
            selectCell appData row col

        Nothing ->
            appData


changeActiveEntry : AppData -> String -> AppData
changeActiveEntry appData letter =
    case appData.activeCell of
        Just ( row, col ) ->
            case getCellFromRowCol appData.cluegridData.grid ( row, col ) of
                Just cellAtRowCol ->
                    updateCellEntry cellAtRowCol letter appData

                Nothing ->
                    appData

        Nothing ->
            appData


isCellEqual : Cell -> Cell -> Bool
isCellEqual cell1 cell2 =
    cell1.row == cell2.row && cell1.col == cell2.col


updateCellInRow : Cell -> List Cell -> String -> List Cell
updateCellInRow cellToUpdate row letter =
    List.map
        (\cell ->
            if isCellEqual cell cellToUpdate then
                { cell | entry = Just letter }

            else
                cell
        )
        row


updateCellEntry : Cell -> String -> AppData -> AppData
updateCellEntry cellToUpdate letter appData =
    let
        newGrid =
            List.map
                (\row -> updateCellInRow cellToUpdate row letter)
                appData.cluegridData.grid

        cluegridData =
            appData.cluegridData
    in
    moveNext
        (AppData
            { cluegridData | grid = newGrid }
            appData.activeClueIndex
            appData.activeCell
        )


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
                            (AppData appData.cluegridData
                                appData.activeClueIndex
                                (Just ( row + rowChange, col + colChange ))
                            )
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
        [ Browser.Events.onKeyDown
            (map KeyPressed
                (field "code" string)
            )
        ]


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


getCellFromRowCol : List (List Cell) -> ( Int, Int ) -> Maybe Cell
getCellFromRowCol cells ( row, col ) =
    case Array.get row (Array.fromList cells) of
        Nothing ->
            -- TODO (07 Dec 2019 sam): What to do here?
            Nothing

        Just correctRow ->
            case Array.get col (Array.fromList correctRow) of
                Nothing ->
                    -- TODO (07 Dec 2019 sam): What to do here?
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
            [ text
                (case cell.entry of
                    Just entry ->
                        entry

                    Nothing ->
                        ""
                )
            ]
        ]


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    on "keyup" (map tagger keyCode)


renderAppData : AppData -> Html Msg
renderAppData appData =
    div
        [ class "crossword-container"
        ]
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
