module Source exposing (..)

import Located exposing (Located(..))
import Parser.Advanced exposing ((|.), (|=))
import Parser.Advanced.Workaround
import Set exposing (Set)


type Word
    = WString String
    | WChar String
    | WWord String
    | WInt Int
    | WFloat Float
    | WMap (List ( Located Word, Located Word ))
    | WGet String
    | WSet String
    | WUri Uri
    | WNamed String
    | WNamedEnd
    | WQuote (List (Located Word))
    | WVariable String
    | WNamespacedWord (Located String) (Located String)
    | WComment String
    | WDocComment String
    | WTypeDef TypeDefinition
    | WBuiltin String


type Uri
    = FilePath (Located String)
    | HttpPath (Located String)
    | HttpsPath (Located String)
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
    | ExpectedChar
    | ExpectedTypeVarStart
    | ExpectedUriPart


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
        |. Parser.Advanced.spaces
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

                -- | WChar String
                , parseChar
                    |> Parser.Advanced.map WChar

                -- | WInt Int
                -- | WFloat Float
                , parseNumber
                , parseNameEnd

                -- | WMap (List ( Located Word, Located Word ))
                , parseRecord
                    |> Parser.Advanced.map WMap

                -- | WQuote (List (Located Word))
                , parseQuote
                    |> Parser.Advanced.map WQuote

                -- | WTypeDef TypeDefinition
                , parseTypeDef
                    |> Parser.Advanced.map WTypeDef

                -- | WComment String
                -- | WDocComment String
                , parseComment
                , parseBuiltin
                    |> Parser.Advanced.map WBuiltin

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
                |> Located.parse
           )
        |. Parser.Advanced.spaces


parseNameEnd : Parser Word
parseNameEnd =
    Parser.Advanced.succeed WNamedEnd
        |. token ";"


parseVariable : Parser String
parseVariable =
    Parser.Advanced.succeed ()
        |. token ":"
        |. Parser.Advanced.chompIf validWordStart ExpectedNameStart
        |. Parser.Advanced.chompWhile validWordMiddle
        |> Parser.Advanced.getChompedString


parseBuiltin : Parser String
parseBuiltin =
    Parser.Advanced.succeed (\name -> "target__" ++ name)
        |. token "**target__"
        |= (Parser.Advanced.succeed ()
                |. Parser.Advanced.chompIf validWordStart ExpectedNameStart
                |. Parser.Advanced.chompWhile (\char -> validWordMiddle char && char /= '*')
                |> Parser.Advanced.getChompedString
           )
        |. token "**"
        |> Parser.Advanced.backtrackable


parseGet : Parser String
parseGet =
    Parser.Advanced.succeed ()
        |. token "."
        |. Parser.Advanced.chompIf validWordStart ExpectedNameStart
        |. Parser.Advanced.chompWhile validWordMiddle
        |> Parser.Advanced.getChompedString


parseSet : Parser String
parseSet =
    Parser.Advanced.succeed ()
        |. token "^"
        |. Parser.Advanced.chompIf validWordStart ExpectedNameStart
        |. Parser.Advanced.chompWhile validWordMiddle
        |> Parser.Advanced.getChompedString


isSpace : Char -> Bool
isSpace char =
    char == ' ' || char == '\n' || char == '\u{000D}'


reservedChars : Set Char
reservedChars =
    Set.fromList
        [ '.'
        , '^'
        , ':'
        , '['
        , ']'
        , '{'
        , '}'
        , '('
        , ')'
        , ';'
        ]


validWordStart : Char -> Bool
validWordStart char =
    if Char.isAlpha char then
        Char.isLower char

    else if Char.isDigit char then
        False

    else
        not (Set.member char reservedChars)


validWordMiddle : Char -> Bool
validWordMiddle char =
    not (isSpace char || Set.member char reservedChars)


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
                        |. Parser.Advanced.chompIf validWordStart ExpectedNameStart
                        |. Parser.Advanced.chompWhile validWordMiddle
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

        "http" ->
            parseHttp

        "https" ->
            parseHttps

        _ ->
            parseUnknownUri locScheme


parseFilePath : Parser Uri
parseFilePath =
    Parser.Advanced.succeed FilePath
        |= (Parser.Advanced.loop [] parseFilePathHelper
                |> Located.parse
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
                    |. Parser.Advanced.chompIf (\char -> not (isSpace char || char == '\\')) ExpectedUriPart
                    |> Parser.Advanced.getChompedString
               )
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
        ]


parseHttp : Parser Uri
parseHttp =
    Parser.Advanced.succeed HttpPath
        |= (Parser.Advanced.loop [] parseHttpHelper
                |> Located.parse
           )


parseHttpHelper : List String -> Parser (Parser.Advanced.Step (List String) String)
parseHttpHelper reverseChunks =
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
                    |. Parser.Advanced.chompIf (\char -> not (isSpace char || char == '\\')) ExpectedUriPart
                    |> Parser.Advanced.getChompedString
               )
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
        ]


parseHttps : Parser Uri
parseHttps =
    Parser.Advanced.succeed HttpsPath
        |= (Parser.Advanced.loop [] parseHttpsHelper
                |> Located.parse
           )


parseHttpsHelper : List String -> Parser (Parser.Advanced.Step (List String) String)
parseHttpsHelper reverseChunks =
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
                    |. Parser.Advanced.chompIf (\char -> not (isSpace char || char == '\\')) ExpectedUriPart
                    |> Parser.Advanced.getChompedString
               )
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
        ]


parseUnknownUri : Located String -> Parser Uri
parseUnknownUri scheme =
    Parser.Advanced.succeed (UnknownUri scheme)
        |= parseUriPath


parseUriPath : Parser (Located String)
parseUriPath =
    Parser.Advanced.loop [] parseUriPathHelper
        |> Located.parse


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
                    |. Parser.Advanced.chompIf (\char -> not (isSpace (Debug.log "file uri char" char) || char == '\\')) ExpectedUriPart
                    |> Parser.Advanced.getChompedString
               )
        , Parser.Advanced.succeed (Parser.Advanced.Done (String.concat (List.reverse reverseChunks)))
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
                |. Parser.Advanced.chompWhile (\char -> not (isSpace char) && char /= ':')
                |> Parser.Advanced.getChompedString
           )
        |. token ":"
        |> Located.parse


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


parseTypeDef : Parser TypeDefinition
parseTypeDef =
    Parser.Advanced.succeed TypeDefinition
        |. token "("
        |. Parser.Advanced.spaces
        |= Located.parse (parseTypeStack "->")
        |= Located.parse (parseTypeStack ")")


parseTypeStack : String -> Parser (List (Located Type))
parseTypeStack endTok =
    Parser.Advanced.loop [] (parseTypeStackHelper endTok)


parseTypeStackHelper : String -> List (Located Type) -> Parser (Parser.Advanced.Step (List (Located Type)) (List (Located Type)))
parseTypeStackHelper endTok reverseTypes =
    Parser.Advanced.oneOf
        [ Parser.Advanced.succeed (Parser.Advanced.Done (List.reverse reverseTypes))
            |. Parser.Advanced.spaces
            |. token endTok
        , Parser.Advanced.succeed (\type_ -> Parser.Advanced.Loop (type_ :: reverseTypes))
            |= Parser.Advanced.lazy (\() -> parseType)
        ]


parseType : Parser (Located Type)
parseType =
    Parser.Advanced.succeed identity
        |= (Parser.Advanced.oneOf
                [ -- | TMixed
                  parseMixedType

                -- | TVar String
                , parseTypeVar
                    |> Parser.Advanced.map TVar

                -- | TConcrete String
                , parseTypeConcrete
                    |> Parser.Advanced.map TConcrete

                -- | TQuote (List (Located Type))
                , parseTypeQuote
                    |> Parser.Advanced.map TQuote
                ]
                |> Located.parse
           )
        |. Parser.Advanced.spaces


parseTypeQuote : Parser (List (Located Type))
parseTypeQuote =
    Parser.Advanced.succeed identity
        |. token "["
        |. Parser.Advanced.spaces
        |= parseTypeStack "]"


parseTypeVar : Parser String
parseTypeVar =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isLower char) ExpectedTypeVarStart
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
        |> Parser.Advanced.getChompedString


parseTypeConcrete : Parser String
parseTypeConcrete =
    Parser.Advanced.succeed ()
        |. Parser.Advanced.chompIf (\char -> Char.isAlpha char && Char.isUpper char) ExpectedTypeVarStart
        |. Parser.Advanced.chompWhile (\char -> Char.isAlphaNum char || char == '_' || char == '-')
        |> Parser.Advanced.getChompedString


parseMixedType : Parser Type
parseMixedType =
    Parser.Advanced.succeed TMixed
        |. token "..."


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


parseChar : Parser String
parseChar =
    Parser.Advanced.succeed identity
        |. token "'"
        |= Parser.Advanced.oneOf
            [ Parser.Advanced.succeed identity
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
            , Parser.Advanced.succeed ()
                |. Parser.Advanced.chompIf (\_ -> True) ExpectedChar
                |> Parser.Advanced.getChompedString
            ]
        |. token "'"


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


type alias TypeDefinition =
    { from : Located (List (Located Type))
    , to : Located (List (Located Type))
    }


type Type
    = TMixed
    | TVar String
    | TConcrete String
    | TQuote (List (Located Type))
