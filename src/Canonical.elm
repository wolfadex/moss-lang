module Canonical exposing (..)

import Dict exposing (Dict)
import Located exposing (Located(..))
import Set exposing (Set)
import Source


type alias File =
    { gives : List Give
    , uses : List Use
    , definitions : List Definition
    }


type Give
    = GWord String


type alias Use =
    { uri : Uri
    , alias_ : Maybe String
    , name : String
    }


type Uri
    = FilePath String


type alias Definition =
    { docComment : Maybe (List String)
    , name : String
    , typeDef : Maybe Source.TypeDefinition
    , body : List Word
    }


type Word
    = WString String
    | WChar String
    | WWord String
    | WInt Int
    | WFloat Float
    | WRecord (Dict String (List Word))
    | WUri Uri
    | WQuote (List Word)
    | WVariable String
    | WNamespacedWord String String


type Error
    = EmptyFile
    | ExpectedGive (Located Source.Word)
    | ExpectedGiveQuote (Located Source.Word)
    | CantGive (Located Source.Word)
    | CantUse (Located Source.Word)
    | ExpectedUseQuote (Located Source.Word)
    | InvalidAlias Located.Span String
    | InvalidUseUri (Located String) (Located String)
    | InvalidUseFilePath Located.Span String
    | InvalidFileName Located.Span String
    | InvalidDefName (Located Source.Word)
    | ShouldntBeIndented Located.Span
    | ShouldBeIndented Located.Span
    | InvalidKey Located.Span
    | UnexpectedNamed Located.Span
    | UnexpectedComment Located.Span
    | UnexpectedDocComment Located.Span
    | UnexpectedTypeDef Located.Span
    | UnexpectedWord (Located Source.Word)
    | UnsupportedUri (Located Source.Uri)
    | ReservedWord Located.Span String


type Warning
    = NoDefinitions
    | NothingGiven Located.Span


fromSource : List (Located Source.Word) -> Result Error ( File, List Warning )
fromSource words =
    case filterComments words of
        ((Located _ (Source.WNamed "give")) as giveWord) :: ((Located givingSpan (Source.WQuote giving)) as givingWord) :: rest ->
            case expectNoIndentation giveWord of
                Just err ->
                    Err err

                Nothing ->
                    case mustBeIndented givingWord of
                        Just err ->
                            Err err

                        Nothing ->
                            Result.map2
                                (\( gives, givesWarns ) ( ( uses, usesWarns ), ( defs, defWarns ) ) ->
                                    ( { gives = gives
                                      , uses = uses
                                      , definitions = defs
                                      }
                                    , givesWarns
                                        ++ usesWarns
                                        ++ defWarns
                                        ++ (if List.isEmpty defs then
                                                [ NoDefinitions ]

                                            else
                                                []
                                           )
                                    )
                                )
                                (givesFromSource givingSpan giving)
                                (usesAndDefsFromSource rest)

        [] ->
            Err EmptyFile

        [ (Located span _) as word ] ->
            Err (ExpectedGive word)

        (Located _ (Source.WNamed "give")) :: notGiveQuote :: _ ->
            Err (ExpectedGiveQuote notGiveQuote)

        unexpectedWord :: _ ->
            Err (UnexpectedWord unexpectedWord)


filterComments : List (Located Source.Word) -> List (Located Source.Word)
filterComments =
    List.filter
        (\word ->
            case word of
                Located _ (Source.WComment _) ->
                    False

                _ ->
                    True
        )


givesFromSource : Located.Span -> List (Located Source.Word) -> Result Error ( List Give, List Warning )
givesFromSource givingSpan wantToGive =
    givesFromSourceHelper givingSpan [] [] wantToGive


givesFromSourceHelper : Located.Span -> List Warning -> List Give -> List (Located Source.Word) -> Result Error ( List Give, List Warning )
givesFromSourceHelper givingSpan warnings gives wantToGive =
    case wantToGive of
        [] ->
            case gives of
                [] ->
                    Ok ( gives, NothingGiven givingSpan :: warnings )

                _ ->
                    Ok ( List.reverse gives, warnings )

        ((Located span (Source.WWord giving)) as giveWord) :: rest ->
            case mustBeIndented giveWord of
                Just err ->
                    Err err

                Nothing ->
                    givesFromSourceHelper givingSpan warnings (GWord giving :: gives) rest

        cantGive :: rest ->
            Err (CantGive cantGive)


usesAndDefsFromSource : List (Located Source.Word) -> Result Error ( ( List Use, List Warning ), ( List Definition, List Warning ) )
usesAndDefsFromSource words =
    case words of
        ((Located _ (Source.WNamed "use")) as useWord) :: ((Located _ (Source.WQuote using)) as usingWord) :: rest ->
            case expectNoIndentation useWord of
                Just err ->
                    Err err

                Nothing ->
                    case mustBeIndented usingWord of
                        Just err ->
                            Err err

                        Nothing ->
                            Result.map2 Tuple.pair
                                (usesFromSource using)
                                (defsFromSource rest)

        (Located _ (Source.WNamed "use")) :: notUseQuote :: _ ->
            Err (ExpectedUseQuote notUseQuote)

        [] ->
            Ok ( ( [], [] ), ( [], [ NoDefinitions ] ) )

        rest ->
            Result.map (\defs -> ( ( [], [] ), defs ))
                (defsFromSource rest)


usesFromSource : List (Located Source.Word) -> Result Error ( List Use, List Warning )
usesFromSource wantToUse =
    usesFromSourceHelper [] [] wantToUse


usesFromSourceHelper : List Warning -> List Use -> List (Located Source.Word) -> Result Error ( List Use, List Warning )
usesFromSourceHelper warnings uses wantToUse =
    case wantToUse of
        [] ->
            Ok ( List.reverse uses, warnings )

        ((Located uriSpan (Source.WUri uri)) as uriWord) :: ((Located aliasSpan (Source.WWord aliasName)) as aliasWord) :: rest ->
            case mustBeIndented uriWord of
                Just err ->
                    Err err

                Nothing ->
                    case mustBeIndented aliasWord of
                        Just err ->
                            Err err

                        Nothing ->
                            case String.uncons aliasName of
                                Nothing ->
                                    Err (InvalidAlias aliasSpan aliasName)

                                Just ( firstChar, restChars ) ->
                                    if Source.validWordStart firstChar && List.all Source.validWordMiddle (String.toList restChars) then
                                        case uriToUse uri of
                                            Err err ->
                                                Err err

                                            Ok ( validUri, _, uriWarnings ) ->
                                                usesFromSourceHelper
                                                    (uriWarnings ++ warnings)
                                                    ({ uri = validUri
                                                     , alias_ = Just aliasName
                                                     , name = aliasName
                                                     }
                                                        :: uses
                                                    )
                                                    rest

                                    else
                                        Err (InvalidAlias aliasSpan aliasName)

        ((Located uriSpan (Source.WUri uri)) as uriWord) :: rest ->
            case mustBeIndented uriWord of
                Just err ->
                    Err err

                Nothing ->
                    case uriToUse uri of
                        Err err ->
                            Err err

                        Ok ( validUri, name, uriWarnings ) ->
                            usesFromSourceHelper
                                (uriWarnings ++ warnings)
                                ({ uri = validUri
                                 , alias_ = Nothing
                                 , name = name
                                 }
                                    :: uses
                                )
                                rest

        cantUse :: rest ->
            Err (CantUse cantUse)


uriToUse : Source.Uri -> Result Error ( Uri, String, List Warning )
uriToUse uri =
    case uri of
        Source.UnknownUri scheme details ->
            Err (InvalidUseUri scheme details)

        Source.FilePath (Located span path) ->
            case String.split "/" path |> List.reverse of
                [] ->
                    Err (InvalidUseFilePath span path)

                fileName :: _ ->
                    let
                        withoutExtension =
                            String.dropRight 3 fileName
                    in
                    case String.uncons withoutExtension of
                        Nothing ->
                            Err (InvalidFileName span withoutExtension)

                        Just ( first, rest ) ->
                            if Source.validWordStart first && List.all Source.validWordMiddle (String.toList rest) then
                                Ok ( FilePath path, withoutExtension, [] )

                            else
                                Err (InvalidFileName span withoutExtension)


defsFromSource : List (Located Source.Word) -> Result Error ( List Definition, List Warning )
defsFromSource words =
    defsFromSourceHelper [] [] words


defsFromSourceHelper : List Warning -> List Definition -> List (Located Source.Word) -> Result Error ( List Definition, List Warning )
defsFromSourceHelper warnings definitions toDef =
    let
        ( docComment, restWords ) =
            gatherDocs toDef
    in
    case restWords of
        [] ->
            Ok ( List.reverse definitions, warnings )

        ((Located nameSpan (Source.WNamed name)) as nameWord) :: ((Located typeSpan (Source.WTypeDef typeDef)) as typeDefWord) :: rest ->
            case checkKeywords nameSpan name of
                Just err ->
                    Err err

                Nothing ->
                    case expectNoIndentation nameWord of
                        Just err ->
                            Err err

                        Nothing ->
                            case mustBeIndented typeDefWord of
                                Just err ->
                                    Err err

                                Nothing ->
                                    case validateDefBody rest of
                                        Just err ->
                                            Err err

                                        Nothing ->
                                            case foldlHaltable foldSourceWord ( [], [] ) rest of
                                                Crashed err ->
                                                    Err err

                                                Complete ( body, bodyWarnings ) ->
                                                    Ok
                                                        ( { docComment = docComment
                                                          , name = name
                                                          , typeDef = Just typeDef
                                                          , body = body
                                                          }
                                                            :: definitions
                                                        , bodyWarnings ++ warnings
                                                        )

                                                Halted wordsToProcess ( body, bodyWarnings ) ->
                                                    defsFromSourceHelper
                                                        (bodyWarnings ++ warnings)
                                                        ({ docComment = docComment
                                                         , name = name
                                                         , typeDef = Just typeDef
                                                         , body = body
                                                         }
                                                            :: definitions
                                                        )
                                                        wordsToProcess

        ((Located nameSpan (Source.WNamed name)) as nameWord) :: rest ->
            case checkKeywords nameSpan name of
                Just err ->
                    Err err

                Nothing ->
                    case expectNoIndentation nameWord of
                        Just err ->
                            Err err

                        Nothing ->
                            case validateDefBody rest of
                                Just err ->
                                    Err err

                                Nothing ->
                                    case foldlHaltable foldSourceWord ( [], [] ) rest of
                                        Crashed err ->
                                            Err err

                                        Complete ( body, bodyWarnings ) ->
                                            Ok
                                                ( { docComment = docComment
                                                  , name = name
                                                  , typeDef = Nothing
                                                  , body = body
                                                  }
                                                    :: definitions
                                                , bodyWarnings ++ warnings
                                                )

                                        Halted wordsToProcess ( body, bodyWarnings ) ->
                                            defsFromSourceHelper
                                                (bodyWarnings ++ warnings)
                                                ({ docComment = docComment
                                                 , name = name
                                                 , typeDef = Nothing
                                                 , body = body
                                                 }
                                                    :: definitions
                                                )
                                                wordsToProcess

        invalidName :: _ ->
            Err (InvalidDefName invalidName)


foldSourceWord : Located Source.Word -> ( List Word, List Warning ) -> HaltableStep Error (Located Source.Word) ( List Word, List Warning )
foldSourceWord sourceWord ( mappedWords, warnings ) =
    case sourceWord of
        Located _ (Source.WNamed _) ->
            HaltBefore

        Located _ (Source.WDocComment _) ->
            HaltBefore

        _ ->
            case mapSourceWord sourceWord of
                Err err ->
                    Crash err

                Ok ( newWords, newWarnings ) ->
                    Continue
                        ( newWords ++ mappedWords
                        , newWarnings ++ warnings
                        )


mapSourceWord : Located Source.Word -> Result Error ( List Word, List Warning )
mapSourceWord ((Located span word) as sourceWord) =
    case word of
        Source.WString string ->
            case mustBeIndented sourceWord of
                Just err ->
                    Err err

                Nothing ->
                    Ok ( [ WString string ], [] )

        Source.WChar char ->
            case mustBeIndented sourceWord of
                Just err ->
                    Err err

                Nothing ->
                    Ok ( [ WChar char ], [] )

        Source.WWord w ->
            case mustBeIndented sourceWord of
                Just err ->
                    Err err

                Nothing ->
                    Ok ( [ WWord w ], [] )

        Source.WInt int ->
            case mustBeIndented sourceWord of
                Just err ->
                    Err err

                Nothing ->
                    Ok ( [ WInt int ], [] )

        Source.WFloat float ->
            case mustBeIndented sourceWord of
                Just err ->
                    Err err

                Nothing ->
                    Ok ( [ WFloat float ], [] )

        Source.WRecord pairs ->
            pairs
                |> listMapOrError
                    (\( Located keySpan key, value ) ->
                        let
                            mappedKey =
                                case key of
                                    Source.WString string ->
                                        case checkKeywords keySpan string of
                                            Just err ->
                                                Err err

                                            Nothing ->
                                                Ok string

                                    _ ->
                                        Err (InvalidKey keySpan)
                        in
                        Result.map2 (\mk ( mv, warns ) -> ( ( mk, List.reverse mv ), warns ))
                            mappedKey
                            (mapSourceWord value)
                    )
                |> Result.map
                    (List.unzip
                        >> (\( mappedPairs, warns ) ->
                                ( [ WRecord (Dict.fromList mappedPairs) ]
                                , List.concat warns
                                )
                           )
                    )

        Source.WGet key ->
            Ok ( [ WWord "get", WString key ], [] )

        Source.WSet key ->
            Ok ( [ WWord "set", WString key ], [] )

        Source.WUri uri ->
            case uri of
                Source.UnknownUri _ _ ->
                    Err (UnsupportedUri (Located span uri))

                Source.FilePath (Located _ path) ->
                    Ok
                        ( [ WUri (FilePath path) ]
                        , []
                        )

        Source.WNamed _ ->
            Err (UnexpectedNamed span)

        Source.WQuote words ->
            words
                |> listMapOrError mapSourceWord
                |> Result.map
                    (List.unzip
                        >> (\( mappedQuote, warns ) ->
                                ( [ WQuote (List.concat (List.reverse mappedQuote)) ]
                                , List.concat warns
                                )
                           )
                    )

        Source.WVariable var ->
            case checkKeywords span var of
                Just err ->
                    Err err

                Nothing ->
                    Ok ( [ WVariable var ], [] )

        Source.WNamespacedWord (Located _ namespace) (Located _ name) ->
            Ok ( [ WNamespacedWord namespace name ], [] )

        Source.WComment _ ->
            Err (UnexpectedComment span)

        Source.WDocComment _ ->
            Err (UnexpectedDocComment span)

        Source.WTypeDef _ ->
            Err (UnexpectedTypeDef span)


gatherDocs : List (Located Source.Word) -> ( Maybe (List String), List (Located Source.Word) )
gatherDocs words =
    gatherDocsHelper [] words


gatherDocsHelper : List String -> List (Located Source.Word) -> ( Maybe (List String), List (Located Source.Word) )
gatherDocsHelper chunks words =
    case words of
        (Located _ (Source.WDocComment chunk)) :: rest ->
            gatherDocsHelper (chunk :: chunks) rest

        rest ->
            ( case chunks of
                [] ->
                    Nothing

                _ ->
                    Just (List.reverse chunks)
            , rest
            )


validateDefBody : List (Located Source.Word) -> Maybe Error
validateDefBody bodyWords =
    case bodyWords of
        [] ->
            Nothing

        word :: rest ->
            case mustBeIndented word of
                Just err ->
                    Just err

                Nothing ->
                    validateDefBody rest


expectNoIndentation : Located a -> Maybe Error
expectNoIndentation (Located span _) =
    if span.start.column == 1 then
        Nothing

    else
        Just (ShouldntBeIndented span)


mustBeIndented : Located a -> Maybe Error
mustBeIndented (Located span _) =
    if span.start.column == 1 then
        Just (ShouldBeIndented span)

    else
        Nothing


keywords : Set String
keywords =
    Set.fromList
        [ "if"
        , "elif"
        , "else"
        , "iff"
        , "get"
        , "set"
        , "at"
        , "each"
        , "size"
        ]


checkKeywords : Located.Span -> String -> Maybe Error
checkKeywords span word =
    if Set.member word keywords then
        Just (ReservedWord span word)

    else
        Nothing


listMapOrError : (a -> Result e b) -> List a -> Result e (List b)
listMapOrError fn list =
    listMapOrErrorHelper fn [] list


listMapOrErrorHelper : (a -> Result e b) -> List b -> List a -> Result e (List b)
listMapOrErrorHelper fn resList list =
    case list of
        [] ->
            Ok (List.reverse resList)

        a :: rest ->
            case fn a of
                Err err ->
                    Err err

                Ok okA ->
                    listMapOrErrorHelper fn (okA :: resList) rest


type Haltable e a b
    = Halted (List a) b
    | Crashed e
    | Complete b


type HaltableStep e a b
    = Continue b
    | HaltBefore
    | HaltAfter b
    | Crash e


foldlHaltable : (a -> b -> HaltableStep e a b) -> b -> List a -> Haltable e a b
foldlHaltable fn b list =
    case list of
        [] ->
            Complete b

        a :: rest ->
            case fn a b of
                HaltBefore ->
                    Halted list b

                HaltAfter intermediateB ->
                    Halted rest intermediateB

                Crash e ->
                    Crashed e

                Continue nextB ->
                    foldlHaltable fn nextB rest
