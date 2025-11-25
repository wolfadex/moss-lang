module Source exposing (..)

import Parser.Advanced exposing ((|.), (|=))
import Parser.Advanced.Workaround


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


type Word
    = WString String
    | WWord String
    | WInt Int
    | WFloat Float
    | WRecord (List ( Located Word, Located Word ))
    | WGet String
    | WSet String
    | WUri Uri
    | WNamed String
    | WQuote (List (Located Word))
    | WVariable String
    | WNamespacedWord (Located String) (Located String)
    | WComment String
    | WDocComment String


type Uri
    = FilePath (Located String)
    | UnknownUri (Located String) (Located String)


type alias Parser a =
    Parser.Advanced.Parser () Problem a


type Problem
    = Error_Todo
    | EndOfFile
    | CodePointWrongSize Int
    | CodePointOutOfBounds Int
    | TokenExpected String
    | ExpectedDigit
    | ExpectedFloat
    | ExpectedInt
    | ExpectedNameStart
    | ExpectedNamedEnd


type alias DeadEnd =
    Parser.Advanced.DeadEnd () Problem


parse : String -> Result (List DeadEnd) (List (Located Word))
parse input =
    Parser.Advanced.run parseInput input


parseInput : Parser (List (Located Word))
parseInput =
    Parser.Advanced.succeed identity
        |. Parser.Advanced.spaces
        |= Parser.Advanced.loop [] parseWords
        |. Parser.Advanced.end EndOfFile


parseWords : List (Located Word) -> Parser (Parser.Advanced.Step (List (Located Word)) (List (Located Word)))
parseWords reverseWords =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\word -> Parser.Advanced.Loop (word :: reverseWords))
            |= parseWord
            |. Parser.Advanced.spaces
        , Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseWords))
        ]


parseWord : Parser (Located Word)
parseWord =
    Parser.Advanced.succeed identity
        |= (Parser.Advanced.oneOf
                [ -- | WString String
                  parseString
                    |> Parser.Advanced.map WString

                -- | WInt Int
                -- | WFloat Float
                , parseNumber

                -- | WRecord (List ( Located Word, Located Word ))
                , parseRecord
                    |> Parser.Advanced.map WRecord

                -- | WQuote (List (Located Word))
                , parseQuote
                    |> Parser.Advanced.map WQuote

                -- | WComment String
                -- | WDocComment String
                , parseComment

                -- | WVariable String
                , parseVariable
                    |> Parser.Advanced.map WVariable

                -- | WGet String
                , parseGet
                    |> Parser.Advanced.map WGet

                -- | WSet String
                , parseSet
                    |> Parser.Advanced.map WSet

                -- | WWord String
                -- | WNamespacedWord (Located Word) (Located Word)
                -- | WUri Uri
                -- | WNamed String
                , parseWordVariants
                ]
                |> parseLocated
           )
        |. Parser.Advanced.spaces


parseVariable : Parser String
parseVariable =
    Parser.Advanced.succeed ()
        |. token ":"
        |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isLower char) ExpectedNameStart
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
        |> Parser.Advanced.getChompedString


parseGet : Parser String
parseGet =
    Parser.Advanced.succeed ()
        |. token "."
        |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isLower char) ExpectedNameStart
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
        |> Parser.Advanced.getChompedString


parseSet : Parser String
parseSet =
    Parser.Advanced.succeed ()
        |. token "^"
        |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isLower char) ExpectedNameStart
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
        |> Parser.Advanced.getChompedString


parseWordVariants : Parser Word
parseWordVariants =
    let
        locWord =
            Parser.Advanced.succeed
                (\( sr, sc ) a ( er, ec ) ->
                    Located
                        { start = { row = sr, column = sc }
                        , end = { row = er, column = ec }
                        }
                        a
                )
                |= Parser.Advanced.getPosition
                |= (Parser.Advanced.succeed ()
                        |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isLower char) ExpectedNameStart
                        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
                        |> Parser.Advanced.getChompedString
                   )
                |= Parser.Advanced.getPosition
    in
    locWord
        |> Parser.Advanced.andThen
            (\word ->
                let
                    (Located _ w) =
                        word
                in
                Parser.Advanced.oneOf
                    [ Parser.Advanced.succeed ()
                        |. token ":"
                        |> Parser.Advanced.andThen
                            (\() ->
                                Parser.Advanced.oneOf
                                    [ Parser.Advanced.succeed WUri
                                        |. token "//"
                                        |= parseUri word
                                    , Parser.Advanced.succeed (WNamed w)
                                    ]
                            )
                    , Parser.Advanced.succeed (WNamespacedWord word)
                        |. token "."
                        |= locWord
                    , Parser.Advanced.succeed (WWord w)
                    ]
            )


parseUri : Located String -> Parser Uri
parseUri ((Located _ scheme) as locScheme) =
    case scheme of
        "file" ->
            parseFilePath

        _ ->
            parseUnknownUri locScheme


parseFilePath : Parser Uri
parseFilePath =
    Parser.Advanced.succeed FilePath
        |= (Parser.Advanced.loop [] parseFilePathHelper
                |> parseLocated
           )


parseFilePathHelper : List String -> Parser (Parser.Advanced.Step (List String) String)
parseFilePathHelper reverseChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Loop ("\\ " :: reverseChunks))
            |. token "\\ "
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
            |. token " "
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
            |. token "\t"
            |. Parser.Advanced.spaces
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
            |. token "\n"
            |. Parser.Advanced.spaces
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (chunk :: reverseChunks))
            |= (Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompWhile (\char -> char /= ' ' && char /= '\\')
                    |> Parser.Advanced.getChompedString
               )
        ]


parseUnknownUri : Located String -> Parser Uri
parseUnknownUri scheme =
    Parser.Advanced.succeed (UnknownUri scheme)
        |= parseUriPath


parseUriPath : Parser (Located String)
parseUriPath =
    Parser.Advanced.loop [] parseUriPathHelper
        |> parseLocated


parseUriPathHelper : List String -> Parser (Parser.Advanced.Step (List String) String)
parseUriPathHelper reverseChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Loop ("\\ " :: reverseChunks))
            |. token "\\ "
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
            |. token " "
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
            |. token "\t"
            |. Parser.Advanced.spaces
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
            |. token "\n"
            |. Parser.Advanced.spaces
        , Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (chunk :: reverseChunks))
            |= (Parser.Advanced.succeed ()
                    |. Parser.Advanced.chompWhile (\char -> char /= ' ' && char /= '\\')
                    |> Parser.Advanced.getChompedString
               )
        ]


parseRecord : Parser (List ( Located Word, Located Word ))
parseRecord =
    Parser.Advanced.succeed identity
        |. token "{"
        |. Parser.Advanced.spaces
        |= Parser.Advanced.loop [] parseRecordHelper


parseRecordHelper : List ( Located Word, Located Word ) -> Parser (Parser.Advanced.Step (List ( Located Word, Located Word )) (List ( Located Word, Located Word )))
parseRecordHelper reversePairs =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reversePairs))
            |. Parser.Advanced.spaces
            |. token "}"
        , Parser.Advanced.succeed (\key value -> Parser.Advanced.Loop (( key, value ) :: reversePairs))
            |= parseKey
            |. Parser.Advanced.spaces
            |= Parser.Advanced.lazy (\() -> parseWord)
        ]


parseKey : Parser (Located Word)
parseKey =
    Parser.Advanced.succeed WNamed
        |= (Parser.Advanced.succeed ()
                |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isLower char) ExpectedNameStart
                |. Parser.Advanced.Workaround.chompUntilBefore (Parser.Advanced.Token ":" ExpectedNamedEnd)
                |> Parser.Advanced.getChompedString
           )
        |. token ":"
        |> parseLocated


parseComment : Parser Word
parseComment =
    Parser.Advanced.succeed ()
        |. token "#"
        |> Parser.Advanced.andThen
            (\() ->
                Parser.Advanced.oneOf
                    [ Parser.Advanced.succeed ()
                        |. token "|"
                        |. Parser.Advanced.Workaround.chompUntilBefore (Parser.Advanced.Token "\n" (TokenExpected "\n"))
                        |> Parser.Advanced.getChompedString
                        |> Parser.Advanced.map WDocComment
                    , Parser.Advanced.succeed ()
                        |. Parser.Advanced.Workaround.chompUntilBefore (Parser.Advanced.Token "\n" (TokenExpected "\n"))
                        |> Parser.Advanced.getChompedString
                        |> Parser.Advanced.map WComment
                    ]
            )


parseNumber : Parser Word
parseNumber =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.chompIf Char.isDigit ExpectedDigit
        |. Parser.Advanced.chompWhile Char.isDigit
        |> Parser.Advanced.getChompedString
        |> Parser.Advanced.andThen
            (\wholeBits ->
                Parser.Advanced.oneOf
                    [ Parser.Advanced.succeed ()
                        |. token "."
                        |. Parser.Advanced.chompIf Char.isDigit ExpectedDigit
                        |. Parser.Advanced.chompWhile Char.isDigit
                        |> Parser.Advanced.getChompedString
                        |> Parser.Advanced.andThen
                            (\partialBits ->
                                case String.toFloat (wholeBits ++ "." ++ partialBits) of
                                    Nothing ->
                                        Parser.Advanced.problem ExpectedFloat

                                    Just float ->
                                        Parser.Advanced.succeed (WFloat float)
                            )
                    , case String.toInt wholeBits of
                        Nothing ->
                            Parser.Advanced.problem ExpectedInt

                        Just int ->
                            Parser.Advanced.succeed (WInt int)
                    ]
            )


parseQuote : Parser (List (Located Word))
parseQuote =
    Parser.Advanced.succeed identity
        |. token "["
        |. Parser.Advanced.spaces
        |= Parser.Advanced.loop [] parseQuoteHelper


parseQuoteHelper : List (Located Word) -> Parser (Parser.Advanced.Step (List (Located Word)) (List (Located Word)))
parseQuoteHelper reverseWords =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseWords))
            |. Parser.Advanced.spaces
            |. token "]"
        , Parser.Advanced.succeed (\word -> Parser.Advanced.Loop (word :: reverseWords))
            |= Parser.Advanced.lazy (\() -> parseWord)
        ]


parseLocated : Parser a -> Parser (Located a)
parseLocated parser =
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


parseString : Parser String
parseString =
    Parser.Advanced.succeed identity
        |. token "\""
        |= Parser.Advanced.loop [] stringHelp


stringHelp : List String -> Parser (Parser.Advanced.Step (List String) String)
stringHelp revChunks =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
            |. token "\\"
            |= Parser.Advanced.oneOf
                [ Parser.Advanced.map (\_ -> "\n") (token "n")
                , Parser.Advanced.map (\_ -> "\t") (token "t")
                , Parser.Advanced.map (\_ -> "\u{000D}") (token "r")
                , Parser.Advanced.succeed String.fromChar
                    |. token "u{"
                    |= parseUnicode
                    |. token "}"
                ]
        , token "\""
            |> Parser.Advanced.map (\_ -> Parser.Advanced.Done (String.join "" (List.reverse revChunks)))
        , Parser.Advanced.chompWhile isUninteresting
            |> Parser.Advanced.getChompedString
            |> Parser.Advanced.map (\chunk -> Parser.Advanced.Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'



-- UNICODE


parseUnicode : Parser Char
parseUnicode =
    Parser.Advanced.getChompedString (Parser.Advanced.chompWhile Char.isHexDigit)
        |> Parser.Advanced.andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        Parser.Advanced.problem (CodePointWrongSize length)

    else if 0 <= code && code <= 0x0010FFFF then
        Parser.Advanced.succeed (Char.fromCode code)

    else
        Parser.Advanced.problem (CodePointOutOfBounds code)


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)


token : String -> Parser ()
token tok =
    Parser.Advanced.token (Parser.Advanced.Token tok (TokenExpected tok))


type alias File =
    { gives : List (Located Give)
    , uses : List (Located Use)
    , definitions : List (Located Definition)
    }


type Give
    = GWord String


type alias Use =
    { uri : Located Uri
    , alias_ : Maybe (Located String)
    , keyword : Located ()
    , definitions : List (Located Definition)
    }


type alias Definition =
    { docComment : Maybe (Located (List (Located String)))
    , name : Located String
    , typeDef : Maybe (Located TypeDefinition)
    , body : Located (List (Located Word))
    }


type alias TypeDefinition =
    { from : Located (List (Located Type))
    , to : Located (List (Located Type))
    }


type Type
    = TMixed
    | TVar String
    | TConcrete String
    | TQuote (List (Located Type))
