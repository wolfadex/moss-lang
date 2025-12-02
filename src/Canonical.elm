module Canonical exposing (..)

import Dict exposing (Dict)
import Haltable
import Located exposing (Located(..))
import Set exposing (Set)
import Source


type alias File =
    { uses : List Use
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
    | HttpPath String
    | HttpsPath String


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
    | WMap (Dict String (List Word))
    | WUri Uri
    | WQuote (List Word)
    | WVariable String
    | WNamespacedWord String String
    | WBuiltin String


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
    | InvalidUseHttpPath Located.Span String
    | InvalidUseHttpsPath Located.Span String
    | InvalidFileName Located.Span String
    | InvalidDefName (Located Source.Word)
    | InvalidKey Located.Span
    | UnexpectedNamed Located.Span
    | UnexpectedNamedEnd Located.Span
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
    case gatherUses (filterComments words) of
        Err err ->
            Err err

        Ok ( uses, usesWarns, remainingWords ) ->
            case remainingWords of
                [] ->
                    Err EmptyFile

                _ ->
                    Result.map
                        (\( defs, defWarns ) ->
                            ( { uses = uses
                              , definitions = defs
                              }
                            , usesWarns
                                ++ defWarns
                                ++ (if List.isEmpty defs then
                                        [ NoDefinitions ]

                                    else
                                        []
                                   )
                            )
                        )
                        (defsFromSource remainingWords)


gatherUses : List (Located Source.Word) -> Result Error ( List Use, List Warning, List (Located Source.Word) )
gatherUses words =
    gatherUsesHelper [] [] words


gatherUsesHelper : List Warning -> List Use -> List (Located Source.Word) -> Result Error ( List Use, List Warning, List (Located Source.Word) )
gatherUsesHelper warnings uses words =
    case words of
        ((Located uriSpan (Source.WUri uri)) as uriWord) :: ((Located aliasSpan (Source.WWord aliasName)) as aliasWord) :: (Located _ (Source.WWord "use")) :: rest ->
            case String.uncons aliasName of
                Nothing ->
                    Err (InvalidAlias aliasSpan aliasName)

                Just ( firstChar, restChars ) ->
                    if Source.validWordStart firstChar && List.all Source.validWordMiddle (String.toList restChars) then
                        case uriToUse uri of
                            Err err ->
                                Err err

                            Ok ( validUri, _, uriWarnings ) ->
                                gatherUsesHelper
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

        ((Located uriSpan (Source.WUri uri)) as uriWord) :: (Located _ (Source.WWord "use")) :: rest ->
            case uriToUse uri of
                Err err ->
                    Err err

                Ok ( validUri, name, uriWarnings ) ->
                    gatherUsesHelper
                        (uriWarnings ++ warnings)
                        ({ uri = validUri
                         , alias_ = Nothing
                         , name = name
                         }
                            :: uses
                        )
                        rest

        _ ->
            Ok ( uses, warnings, words )


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

        Source.HttpPath (Located span path) ->
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
                                Ok ( HttpPath path, withoutExtension, [] )

                            else
                                Err (InvalidFileName span withoutExtension)

        Source.HttpsPath (Located span path) ->
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
                                Ok ( HttpsPath path, withoutExtension, [] )

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
                    case Haltable.foldlResult foldSourceWord ( [], [] ) rest of
                        Haltable.Crashed err ->
                            Err err

                        Haltable.Success ( body, bodyWarnings ) ->
                            Ok
                                ( { docComment = docComment
                                  , name = name
                                  , typeDef = Just typeDef
                                  , body = body
                                  }
                                    :: definitions
                                , bodyWarnings ++ warnings
                                )

                        Haltable.ResultHalted wordsToProcess ( body, bodyWarnings ) ->
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
                    case Haltable.foldlResult foldSourceWord ( [], [] ) rest of
                        Haltable.Crashed err ->
                            Err err

                        Haltable.Success ( body, bodyWarnings ) ->
                            Ok
                                ( { docComment = docComment
                                  , name = name
                                  , typeDef = Nothing
                                  , body = body
                                  }
                                    :: definitions
                                , bodyWarnings ++ warnings
                                )

                        Haltable.ResultHalted wordsToProcess ( body, bodyWarnings ) ->
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


foldSourceWord : Located Source.Word -> ( List Word, List Warning ) -> Result Error (Haltable.Step ( List Word, List Warning ))
foldSourceWord sourceWord ( mappedWords, warnings ) =
    case sourceWord of
        Located _ (Source.WNamed _) ->
            Ok Haltable.HaltBefore

        Located _ (Source.WDocComment _) ->
            Ok Haltable.HaltBefore

        Located _ Source.WNamedEnd ->
            Ok
                (Haltable.HaltAfter
                    ( mappedWords
                    , warnings
                    )
                )

        _ ->
            case mapSourceWord sourceWord of
                Err err ->
                    Err err

                Ok ( newWords, newWarnings ) ->
                    Ok
                        (Haltable.Continue
                            ( newWords ++ mappedWords
                            , newWarnings ++ warnings
                            )
                        )


mapSourceWord : Located Source.Word -> Result Error ( List Word, List Warning )
mapSourceWord ((Located span word) as sourceWord) =
    case word of
        Source.WString string ->
            Ok ( [ WString string ], [] )

        Source.WChar char ->
            Ok ( [ WChar char ], [] )

        Source.WWord w ->
            Ok ( [ WWord w ], [] )

        Source.WInt int ->
            Ok ( [ WInt int ], [] )

        Source.WFloat float ->
            Ok ( [ WFloat float ], [] )

        Source.WMap pairs ->
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
                                ( [ WMap (Dict.fromList mappedPairs) ]
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

                Source.HttpPath (Located _ path) ->
                    Ok
                        ( [ WUri (HttpPath path) ]
                        , []
                        )

                Source.HttpsPath (Located _ path) ->
                    Ok
                        ( [ WUri (HttpsPath path) ]
                        , []
                        )

        Source.WNamed _ ->
            Err (UnexpectedNamed span)

        Source.WNamedEnd ->
            Err (UnexpectedNamedEnd span)

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

        Source.WBuiltin name ->
            Ok ( [ WBuiltin name ], [] )


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
        , "use"
        , "read"
        , "readWith"
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
