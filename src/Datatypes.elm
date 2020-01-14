-- I don't really know if this is the best way of organising files / data
-- What I wanted to do was call `onClick` in my renderCell function, which
-- I wanted to be in the `Cell` module. For that, I needed to have access
-- to `Msg` in `Cell`. But I had no way to import that without having circular
-- inputs. So I thought I'd create a separate `Message` module. But then the
-- `FetchedData` message needed CluegridData. And the same circular dependencies
-- issue popped up. I couldn't just bring CluegridData without all the other
-- things, so I just decided to bring all the data types into this file. I
-- don't believe this is ideal, but it's fairly clean. It doesn't really allow
-- us to make private setters/getters/creators or whatever, but atleast I got
-- it to work without it having to be in a single file. Will need to see where
-- or how this approach breaks, or whether it holds up well.
--                                                          Sam (14 Dec 2019)


module Datatypes exposing
    ( ActiveCell
    , ActiveClueIndex
    , AppData
    , ArrowKeyDirection(..)
    , Cell
    , CellUpdateData
    , ChannelDetails
    , ChannelName
    , Clue
    , ClueDirection(..)
    , CluegridInfo
    , Clues
    , ControlKey(..)
    , CrossWordListingInfo
    , Grid
    , KeyboardInput(..)
    , LandingData(..)
    , ModalContents(..)
    , Model(..)
    , Msg(..)
    , PageData
    , PuzzleData(..)
    , RecieveSocketMessage
    , RowCol
    , SendSocketMessage
    , cellUpdateDataDecoder
    , channelDetailsDecoder
    , crosswordListingDecoder
    )

import Array exposing (Array)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url exposing (Url)


type Model
    = LandingPage LandingData
    | PuzzlePage PuzzleData


type LandingData
    = LoadingListing PageData
    | FailedJoining PageData
    | LoadedListing ( List CrossWordListingInfo, PageData )


type alias PageData =
    { url : Url
    , key : Key
    }


type alias CrossWordListingInfo =
    { title : String
    , link : String
    }


crosswordListingDecoder : Decode.Decoder (List CrossWordListingInfo)
crosswordListingDecoder =
    Decode.list
        (Decode.map2 CrossWordListingInfo
            (Decode.field "title" Decode.string)
            (Decode.field "link" Decode.string)
        )


type PuzzleData
    = Loading ChannelDetails
    | Failure ChannelDetails
    | Loaded ( ChannelDetails, AppData )


type Msg
    = FetchedData (Result Http.Error AppData)
    | KeyPressed String
    | CellClicked Int Int
    | ClueClicked Int
    | SetScroll
    | HandleSocketMessage RecieveSocketMessage
    | CloseModal
    | SolveActiveClue
    | CheckActiveClue
    | SetModalInfo
    | SetModalInvite
    | RequestCreateRoom String
    | GoHome
    | NoOp


type alias ChannelName =
    String


type alias ClueIndex =
    Int


type alias RecieveSocketMessage =
    { message : String
    , data : Decode.Value
    }


type alias SendSocketMessage =
    { message : String
    , data : Encode.Value
    }


type alias ChannelDetails =
    { channelName : String
    , link : String
    , pageData : Maybe PageData
    }


channelDetailsDecoder : Decode.Decoder ChannelDetails
channelDetailsDecoder =
    Decode.map3 ChannelDetails
        (Decode.field "channel_name" Decode.string)
        (Decode.field "link" Decode.string)
        (Decode.succeed Nothing)


type alias Clue =
    { startCol : Int
    , startRow : Int
    , solution : String
    , direction : ClueDirection
    , gridNumber : Int
    , clue_text : String
    }


type alias Clues =
    List Clue


type ClueDirection
    = Across
    | Down


type alias Cell =
    { solution : String
    , row : Int
    , col : Int
    , gridNumber : Maybe Int
    , acrossClueIndex : Maybe Int
    , downClueIndex : Maybe Int
    , entry : Maybe String
    , oldEntry : Maybe String
    }


type alias RowCol =
    { row : Int, col : Int }


type alias CellUpdateData =
    { cell : RowCol
    , letter : Maybe String
    }


cellUpdateDataDecoder : Decode.Decoder CellUpdateData
cellUpdateDataDecoder =
    Decode.map2 CellUpdateData
        (Decode.map2 RowCol
            (Decode.at [ "cell", "row" ] Decode.int)
            (Decode.at [ "cell", "col" ] Decode.int)
        )
        (Decode.field "letter" (Decode.nullable Decode.string))


type alias ActiveClueIndex =
    Maybe Int


type alias ActiveCell =
    Maybe ( Int, Int )


type alias Grid =
    Array (Array Cell)


type alias CluegridInfo =
    { date : String
    , title : String
    , author : String
    , editor : String
    , copyright : String
    }


type ModalContents
    = Empty
    | Info
    | Invite


type alias AppData =
    { clues : Clues
    , grid : Grid
    , cluegridInfo : CluegridInfo
    , activeClueIndex : ActiveClueIndex
    , activeCell : ActiveCell
    , modal : ModalContents
    , otherClueIndex : ActiveClueIndex
    }


type ArrowKeyDirection
    = ArrowKeyUp
    | ArrowKeyDown
    | ArrowKeyLeft
    | ArrowKeyRight


type ControlKey
    = SpaceBar
    | EnterKey
    | BackspaceKey
    | TabKey
    | ShiftTabKey
    | EscapeKey


type KeyboardInput
    = LetterKey String
    | ArrowKey ArrowKeyDirection
    | ControlKey ControlKey
    | UnsupportedKey
