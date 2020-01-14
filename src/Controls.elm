port module Controls exposing (checkActiveClue, handleKeyInput, handleLandingSocketMessage, handlePuzzleSocketMessage, renderAppData, renderHeaderRow, selectCell, selectCellAndScroll, sendRequestAllCells, sendSocketMessage, setActiveClue, solveActiveClue, updateCellData, updateOtherClue)

import Array
import Browser.Dom as Dom
import Browser.Navigation
import Cell exposing (crosswordCellisBlank, getCellFromRowCol, isRowColEqual, renderCell, renderGrid, updateCellEntry)
import Clue exposing (getClueId, renderCluesData)
import Data exposing (decodeAppData)
import Datatypes exposing (ActiveClueIndex, AppData, ArrowKeyDirection(..), Cell, CellUpdateData, ChannelDetails, ClueDirection(..), Clues, ControlKey(..), CrossWordListingInfo, KeyboardInput(..), LandingData(..), ModalContents(..), Model(..), Msg(..), PuzzleData(..), RecieveSocketMessage, RowCol, SendSocketMessage, cellUpdateDataDecoder, channelDetailsDecoder, crosswordListingDecoder)
import Html exposing (Html, a, br, div, i, img, text)
import Html.Attributes exposing (class, classList, href, src)
import Html.Events exposing (onClick, stopPropagationOn)
import Http
import Json.Decode as Decode exposing (decodeValue, field, int, nullable)
import Json.Encode as Encode
import Task
import Url
import Url.Builder


port sendSocketMessage : SendSocketMessage -> Cmd msg


sendRequestAllCells : Cmd Msg
sendRequestAllCells =
    sendSocketMessage (SendSocketMessage "request_all_cells" Encode.null)


updateOtherClue : ActiveClueIndex -> AppData -> AppData
updateOtherClue otherClueIndex appData =
    { appData | otherClueIndex = otherClueIndex }


updateCellData : CellUpdateData -> AppData -> AppData
updateCellData cellUpdateData appData =
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
    Cmd.batch
        [ getClueId clueIndex
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
        , sendClueIndexUpdate appData.activeClueIndex
        ]


sendCellUpdate : CellUpdateData -> Cmd msg
sendCellUpdate cellUpdateData =
    let
        entry =
            case cellUpdateData.letter of
                Nothing ->
                    Encode.null

                Just letter ->
                    Encode.string letter
    in
    sendSocketMessage
        -- TODO (13 Jan 2020 sam): This seems to sometimes seems to send data as nil
        -- See if we can figure this out. I'm reporting this from a backend perspective
        -- Not seen it from the frontend as of yet.
        (SendSocketMessage
            "update_entry"
            (Encode.object
                [ ( "cell"
                  , Encode.object
                        [ ( "row", Encode.int cellUpdateData.cell.row )
                        , ( "col", Encode.int cellUpdateData.cell.col )
                        ]
                  )
                , ( "letter", entry )
                ]
            )
        )


sendClueIndexUpdate : ActiveClueIndex -> Cmd msg
sendClueIndexUpdate activeClueIndex =
    let
        index =
            case activeClueIndex of
                Nothing ->
                    Encode.null

                Just i ->
                    Encode.int i
    in
    sendSocketMessage
        (SendSocketMessage
            "update_active_clue"
            index
        )


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


sendScrollToClue : ( ChannelDetails, AppData ) -> ( Model, Cmd Msg )
sendScrollToClue ( channelDetails, appData ) =
    ( PuzzlePage (Loaded ( channelDetails, appData )), scrollToClue appData )


sendUpdateData : Maybe String -> Int -> Int -> ( ChannelDetails, AppData ) -> ( Model, Cmd Msg )
sendUpdateData letter row col ( channelDetails, appData ) =
    let
        cellUpdateData =
            CellUpdateData (RowCol row col) letter
    in
    ( PuzzlePage (Loaded ( channelDetails, appData ))
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


handleKeyInput : String -> ( ChannelDetails, AppData ) -> ( Model, Cmd Msg )
handleKeyInput key ( channelDetails, appData ) =
    let
        keyInput =
            keyToKeyboardInput key
    in
    case keyInput of
        ControlKey control ->
            case control of
                EnterKey ->
                    sendScrollToClue ( channelDetails, toggleActiveClue appData )

                BackspaceKey ->
                    let
                        ( row, col ) =
                            case appData.activeCell of
                                Nothing ->
                                    ( -1, -1 )

                                Just ( r, c ) ->
                                    ( r, c )
                    in
                    ( channelDetails, changeActiveEntry appData Nothing )
                        |> sendUpdateData Nothing row col

                TabKey ->
                    ( channelDetails, selectNextClue appData )
                        |> sendScrollToClue

                ShiftTabKey ->
                    ( channelDetails, selectPreviousClue appData )
                        |> sendScrollToClue

                EscapeKey ->
                    ( PuzzlePage (Loaded ( channelDetails, { appData | modal = Empty } )), Cmd.none )

                _ ->
                    ( channelDetails, appData )
                        |> sendScrollToClue

        ArrowKey arrow ->
            case arrow of
                ArrowKeyRight ->
                    ( channelDetails, moveRight appData )
                        |> sendScrollToClue

                ArrowKeyLeft ->
                    ( channelDetails, moveLeft appData )
                        |> sendScrollToClue

                ArrowKeyUp ->
                    ( channelDetails, moveUp appData )
                        |> sendScrollToClue

                ArrowKeyDown ->
                    ( channelDetails, moveDown appData )
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
            ( channelDetails, changeActiveEntry appData (Just letter) )
                |> sendUpdateData (Just letter) row col

        UnsupportedKey ->
            ( channelDetails, appData )
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


moveCellWithoutJump : AppData -> Int -> Int -> AppData
moveCellWithoutJump appData rowChange colChange =
    case appData.activeCell of
        Nothing ->
            appData

        Just ( row, col ) ->
            case getCellFromRowCol appData.grid ( row + rowChange, col + colChange ) of
                Nothing ->
                    appData

                Just cell ->
                    if crosswordCellisBlank cell then
                        appData

                    else
                        selectCell appData (row + rowChange) (col + colChange)


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
                            moveCellWithoutJump appData 0 1

                        Down ->
                            moveCellWithoutJump appData 1 0

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
                            moveCellWithoutJump appData 0 -1

                        Down ->
                            moveCellWithoutJump appData -1 0

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

    else if code == "Escape" then
        ControlKey EscapeKey

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


selectCellAndScroll : ( ChannelDetails, AppData ) -> Int -> Int -> ( Model, Cmd Msg )
selectCellAndScroll ( channelDetails, appData ) rowNum colNum =
    sendScrollToClue ( channelDetails, selectCell appData rowNum colNum )


isPartOfClue : Int -> Cell -> Bool
isPartOfClue index cell =
    let
        isAcross =
            case cell.acrossClueIndex of
                Just acrossIndex ->
                    acrossIndex == index

                Nothing ->
                    False

        isDown =
            case cell.downClueIndex of
                Just downIndex ->
                    downIndex == index

                Nothing ->
                    False
    in
    isAcross || isDown


getSolution : Cell -> CellUpdateData
getSolution cell =
    CellUpdateData
        (RowCol cell.row cell.col)
        (Just cell.solution)


checkCorrect : Cell -> CellUpdateData
checkCorrect cell =
    let
        soln =
            case cell.entry of
                Nothing ->
                    Nothing

                Just letter ->
                    if letter == cell.solution then
                        Just letter

                    else
                        Nothing
    in
    CellUpdateData
        (RowCol cell.row cell.col)
        soln


solveActiveClue : ( ChannelDetails, AppData ) -> ( Model, Cmd Msg )
solveActiveClue ( channelDetails, appData ) =
    case appData.activeClueIndex of
        Nothing ->
            ( PuzzlePage (Loaded ( channelDetails, appData )), Cmd.none )

        Just index ->
            let
                clueCells =
                    appData.grid
                        |> Array.foldl Array.append (Array.fromList [])
                        |> Array.filter (\cell -> isPartOfClue index cell)

                updateData =
                    clueCells
                        |> Array.map (\cell -> getSolution cell)

                newData =
                    updateData
                        |> Array.foldl updateCellData appData
            in
            ( PuzzlePage (Loaded ( channelDetails, newData ))
            , Cmd.batch
                (updateData
                    |> Array.map (\cellUpdate -> sendCellUpdate cellUpdate)
                    |> Array.toList
                )
            )


checkActiveClue : ( ChannelDetails, AppData ) -> ( Model, Cmd Msg )
checkActiveClue ( channelDetails, appData ) =
    case appData.activeClueIndex of
        Nothing ->
            ( PuzzlePage (Loaded ( channelDetails, appData )), Cmd.none )

        Just index ->
            let
                updateData =
                    appData.grid
                        |> Array.foldl Array.append (Array.fromList [])
                        |> Array.filter (\cell -> isPartOfClue index cell)
                        |> Array.map (\cell -> checkCorrect cell)

                newData =
                    updateData
                        |> Array.foldl updateCellData appData
            in
            ( PuzzlePage (Loaded ( channelDetails, newData ))
            , Cmd.batch
                (updateData
                    |> Array.map (\cellUpdate -> sendCellUpdate cellUpdate)
                    |> Array.toList
                )
            )


handleLandingSocketMessage : RecieveSocketMessage -> LandingData -> ( Model, Cmd Msg )
handleLandingSocketMessage socketMessage landingData =
    let
        pageData =
            case landingData of
                LoadingListing k ->
                    k

                FailedJoining k ->
                    k

                LoadedListing ( _, k ) ->
                    k
    in
    case socketMessage.message of
        "crossword_listing" ->
            case decodeValue crosswordListingDecoder socketMessage.data of
                Ok crosswordListing ->
                    ( LandingPage (LoadedListing ( crosswordListing, pageData )), Cmd.none )

                Err e ->
                    ( LandingPage landingData, Cmd.none )

        "channel_full" ->
            ( LandingPage (FailedJoining pageData), Cmd.none )

        "channel_details" ->
            case decodeValue channelDetailsDecoder socketMessage.data of
                Ok channelDetails ->
                    ( PuzzlePage (Loading { channelDetails | pageData = Just pageData })
                    , Cmd.batch
                        [ Http.get
                            { url = channelDetails.link
                            , expect = Http.expectJson FetchedData decodeAppData
                            }
                        , Browser.Navigation.pushUrl pageData.key channelDetails.channelName
                        ]
                    )

                Err e ->
                    ( LandingPage landingData, Cmd.none )

        _ ->
            ( LandingPage landingData, Cmd.none )



-- ( LandingPage (LandingData []), Cmd.none )


handlePuzzleSocketMessage : RecieveSocketMessage -> ( ChannelDetails, AppData ) -> ( Model, Cmd Msg )
handlePuzzleSocketMessage socketMessage ( channelDetails, appData ) =
    case socketMessage.message of
        "update_other_clue" ->
            case decodeValue (nullable int) socketMessage.data of
                Ok otherIndex ->
                    ( PuzzlePage (Loaded ( channelDetails, updateOtherClue otherIndex appData )), Cmd.none )

                Err e ->
                    ( PuzzlePage (Loaded ( channelDetails, appData )), Cmd.none )

        "update_entry" ->
            case decodeValue cellUpdateDataDecoder socketMessage.data of
                Ok cellUpdateData ->
                    ( PuzzlePage (Loaded ( channelDetails, updateCellData cellUpdateData appData )), Cmd.none )

                Err e ->
                    ( PuzzlePage (Loaded ( channelDetails, appData )), Cmd.none )

        _ ->
            ( PuzzlePage (Loaded ( channelDetails, appData )), Cmd.none )


renderHeaderRow : Html Msg
renderHeaderRow =
    div
        [ class "cluegrid-header-container" ]
        [ div
            [ class "cluegrid-header-row" ]
            [ div [ class "cluegrid-header-logo-container", onClick GoHome ]
                [ img [ class "cluegrid-header-logo logo", src "cluegrid_logo.png" ] []
                ]
            ]
        , div
            [ class "cluegrid-header-buttons" ]
            [ div [ class "cluegrid-header-button", onClick SolveActiveClue ]
                [ text "SOLVE CLUE" ]
            , div [ class "cluegrid-header-button", onClick CheckActiveClue ]
                [ text "CHECK CLUE" ]
            , div [ class "cluegrid-header-button", onClick SetModalInfo ]
                [ text "INFO" ]
            , div [ class "cluegrid-header-button cluegrid-button-invite", onClick SetModalInvite ]
                [ text "INVITE" ]
            ]
        ]


hideModal : ModalContents -> Bool
hideModal modal =
    case modal of
        Empty ->
            True

        _ ->
            False


renderModal : ( ChannelDetails, AppData ) -> Html Msg
renderModal ( channelDetails, appData ) =
    -- NOTE (10 Jan 2020 sam): Structured this way so that we can animate modal
    -- closing. Would have to change it to some other way if we want to support
    -- multiple different modals
    let
        _ =
            Debug.log "channelName" channelDetails.channelName

        currentUrl =
            -- TODO (14 Jan 2020 sam): The url is a bit of a scam right now...
            -- There is a hard-coded assumption that the port will be fixed,
            -- same for protocol etc. Just don't feel like bothering. I was
            -- initially using pageData.url. But that causes a bug if we come
            -- to an existing puzzle, and then go back and create anew one...
            case channelDetails.pageData of
                Nothing ->
                    ""

                Just pageData ->
                    pageData.url.host
                        ++ "/"
                        ++ channelDetails.channelName

        modalInfo =
            case appData.modal of
                Info ->
                    [ div [ class "cluegrid-modal-info-title" ] [ text appData.cluegridInfo.title ]
                    , div [ class "cluegrid-modal-info cluegrid-modal-bold" ] [ text ("Author: " ++ appData.cluegridInfo.author) ]
                    , div [ class "cluegrid-modal-info cluegrid-modal-bold" ] [ text ("Editor: " ++ appData.cluegridInfo.editor) ]
                    , div [ class "cluegrid-modal-info" ] [ text appData.cluegridInfo.copyright ]
                    ]

                Invite ->
                    [ div
                        [ class "cluegrid-modal-invite"
                        , stopPropagationOn "click" (Decode.succeed ( NoOp, False ))
                        ]
                        [ text "invite a friend with the link"
                        , br [] []
                        , text currentUrl
                        , br [] []
                        , br [] []
                        , div [ class "cluegrid-modal-info" ] [ text "or just copy the link in your browser" ]
                        ]
                    ]

                _ ->
                    []
    in
    div
        [ class "cluegrid-modal-background"
        , classList [ ( "cluegrid-modal-hidden", hideModal appData.modal ) ]
        , onClick CloseModal
        ]
        [ div [ class "cluegrid-modal-container" ]
            [ div [ class "cluegrid-modal-header" ] [ text "cluegrid" ]
            , div [ class "cluegrid-modal-vert-spacer" ] []
            , div [ class "cluegrid-modal-vert-spacer" ] []
            , div [ class "cluegrid-modal-center" ] modalInfo
            , div [ class "cluegrid-modal-vert-spacer" ] []
            , div [ class "cluegrid-modal-vert-spacer" ] []
            , div [ class "cluegrid-modal-vert-spacer" ] []
            , div [ class "cluegrid-modal-info" ]
                [ a [ href "https://github.com/samhattangady/cluegrid/" ] [ text "built" ]
                , text " with "
                , a [ href "https://elm-lang.org" ] [ text "elm" ]
                ]
            , div [ class "cluegrid-modal-info" ]
                [ text "data from "
                , a [ href "https://www.xwordinfo.com/" ] [ text "xwordinfo" ]
                ]
            , div [ class "cluegrid-modal-info" ]
                [ text "created by "
                , a [ href "https://samhattangady.com" ] [ text "chapliboy" ]
                ]
            ]
        ]


renderAppData : ( ChannelDetails, AppData ) -> Html Msg
renderAppData ( channelDetails, appData ) =
    div
        [ class "cluegrid-container" ]
        [ renderGrid appData.grid appData.activeClueIndex appData.otherClueIndex appData.activeCell
        , renderCluesData appData.clues appData.grid appData.activeClueIndex
        , renderModal ( channelDetails, appData )
        ]
