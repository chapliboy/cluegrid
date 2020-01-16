module Data exposing (decodeAppData)

import Cell exposing (decodeGrid)
import Clue exposing (decodeClues)
import Datatypes exposing (AppData, CluegridInfo, ModalContents(..), TouchActiveElement(..), TouchModeData)
import Json.Decode exposing (Decoder, field, int, map2, map5, map8, string, succeed)


decodeAppData : Decoder AppData
decodeAppData =
    map8 AppData
        (field "clues" decodeClues)
        (field "grid" decodeGrid)
        (map5 CluegridInfo
            (field "info" (field "date" string))
            (field "info" (field "title" string))
            (field "info" (field "author" string))
            (field "info" (field "editor" string))
            (field "info" (field "copyright" string))
        )
        (succeed Nothing)
        (succeed Nothing)
        (succeed Empty)
        (succeed Nothing)
        (map2 TouchModeData
            (succeed False)
            (succeed ClueActive)
        )
