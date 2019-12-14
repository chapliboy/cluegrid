module Data exposing (decodeCluegridData)

import Cell exposing (decodeGrid)
import Clue exposing (decodeClues)
import Datatypes exposing (CluegridData, CluegridInfo, CluegridSize)
import Json.Decode exposing (Decoder, field, int, map2, map4, map5, string)


decodeCluegridData : Decoder CluegridData
decodeCluegridData =
    map4 CluegridData
        (field "clues" decodeClues)
        (field "grid" decodeGrid)
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
