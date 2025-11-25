module Located exposing (..)

import Parser.Advanced exposing ((|.), (|=))


type Located a
    = Located Span a


type alias Span =
    { start : Point
    , end : Point
    }


type alias Point =
    { row : Int
    , column : Int
    }


parse : Parser.Advanced.Parser context problem a -> Parser.Advanced.Parser context problem (Located a)
parse parser =
    Parser.Advanced.succeed
        (\( sr, sc ) a ( er, ec ) ->
            Located
                { start = { row = sr, column = sc }
                , end = { row = er, column = ec }
                }
                a
        )
        |= Parser.Advanced.getPosition
        |= parser
        |= Parser.Advanced.getPosition
