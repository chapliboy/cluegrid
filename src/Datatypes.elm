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
    ( AppData
    , ArrowKeyDirection(..)
    , Cell
    , CellUpdateData
    , ChannelName
    , Clue
    , ClueDirection(..)
    , CluegridData
    , CluegridInfo
    , CluegridSize
    , Clues
    , ControlKey(..)
    , Grid
    , KeyboardInput(..)
    , Model(..)
    , Msg(..)
    , RowCol
    )

import Array exposing (Array)
import Http


type Model
    = Loading
    | Failure
    | Loaded AppData


type Msg
    = FetchedData (Result Http.Error CluegridData)
    | KeyPressed String
    | CellClicked Int Int
    | ClueClicked Int
    | CellUpdate CellUpdateData
    | SetScroll


type alias ChannelName =
    String


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


type alias CluegridData =
    { clues : Clues
    , grid : Grid
    , size : CluegridSize
    , info : CluegridInfo
    }


type alias Cell =
    { solution : String
    , row : Int
    , col : Int
    , gridNumber : Maybe Int
    , acrossClueIndex : Maybe Int
    , downClueIndex : Maybe Int
    , entry : Maybe String
    }


type alias RowCol =
    { row : Int, col : Int }


type alias CellUpdateData =
    { cell : RowCol
    , letter : Maybe String
    }


type alias Grid =
    Array (Array Cell)


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


type alias AppData =
    { cluegridData : CluegridData
    , activeClueIndex : Maybe Int
    , activeCell : Maybe ( Int, Int )
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


type KeyboardInput
    = LetterKey String
    | ArrowKey ArrowKeyDirection
    | ControlKey ControlKey
    | UnsupportedKey
