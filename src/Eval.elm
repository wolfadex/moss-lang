module Eval exposing (..)

import Canonical
import Dict exposing (Dict)
import Haltable
import Http
import Located exposing (Located(..))
import Source
import Task


type alias Model =
    { toEval : List Word
    , mode : Mode
    , context : Context
    }


type Mode
    = Evaling
    | Compiling String (List Word)


type Word
    = WString String
    | WChar String
    | WWord String
    | WInt Int
    | WFloat Float
    | WMap (Dict String Word)
    | WGet String
    | WSet String
    | WUri Uri
    | WNamed String
    | WNamedEnd
    | WQuote (List Word)
    | WVariable String
    | WNamespacedWord String String
    | WBuiltin String


type Uri
    = FilePath String
    | HttpPath String
    | HttpsPath String


type alias Context =
    { stack : List Word
    , definitions : Definitions
    }


type alias Definitions =
    List ( ( String, String ), List Word )


init : Model
init =
    { toEval = []
    , mode = Evaling
    , context =
        { stack = []
        , definitions = []
        }
    }


builtins : Dict String (Context -> Result String (Haltable.Step ( Context, Effect Msg )))
builtins =
    Dict.fromList
        [ -- MATH
          ( "+"
          , \ctx ->
                case ctx.stack of
                    (WInt right) :: (WInt left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WInt (left + right) :: rest }, None )

                    (WFloat right) :: (WFloat left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WFloat (left + right) :: rest }, None )

                    _ ->
                        Err "expected 2 numbers"
          )
        , ( "-"
          , \ctx ->
                case ctx.stack of
                    (WInt right) :: (WInt left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WInt (left - right) :: rest }, None )

                    (WFloat right) :: (WFloat left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WFloat (left - right) :: rest }, None )

                    _ ->
                        Err "expected 2 numbers"
          )
        , ( "*"
          , \ctx ->
                case ctx.stack of
                    (WInt right) :: (WInt left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WInt (left * right) :: rest }, None )

                    (WFloat right) :: (WFloat left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WFloat (left * right) :: rest }, None )

                    _ ->
                        Err "expected 2 numbers"
          )
        , ( "/"
          , \ctx ->
                case ctx.stack of
                    (WInt right) :: (WInt left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WInt (left // right) :: rest }, None )

                    (WFloat right) :: (WFloat left) :: rest ->
                        Ok <| Haltable.Continue ( { ctx | stack = WFloat (left / right) :: rest }, None )

                    _ ->
                        Err "expected 2 numbers"
          )

        -- URIs
        , ( "use"
          , \ctx ->
                case ctx.stack of
                    (WUri uri) :: (WString alias_) :: rest ->
                        case validateAliasedUseUri uri alias_ of
                            Err () ->
                                Err "invalid uri"

                            Ok ( validUri, namespace ) ->
                                case validUri of
                                    HttpsPath url ->
                                        Ok <|
                                            Haltable.HaltAfter
                                                ( { ctx | stack = rest }
                                                , HttpRequest
                                                    { method = "GET"
                                                    , headers = []
                                                    , url = "https://" ++ url
                                                    , body = Http.emptyBody
                                                    , expect = Http.expectString (UseResponse namespace)
                                                    , timeout = Nothing
                                                    , tracker = Nothing
                                                    }
                                                )

                                    _ ->
                                        Debug.todo ""

                    (WUri uri) :: rest ->
                        case validateUseUri uri of
                            Err () ->
                                Err "invalid uri"

                            Ok ( validUri, namespace ) ->
                                case validUri of
                                    HttpsPath url ->
                                        Ok <|
                                            Haltable.HaltAfter
                                                ( { ctx | stack = rest }
                                                , HttpRequest
                                                    { method = "GET"
                                                    , headers = []
                                                    , url = "https://" ++ url
                                                    , body = Http.emptyBody
                                                    , expect = Http.expectString (UseResponse namespace)
                                                    , timeout = Nothing
                                                    , tracker = Nothing
                                                    }
                                                )

                                    _ ->
                                        Debug.todo ""

                    _ ->
                        Err "expected an URI"
          )
        , ( "read"
          , \ctx ->
                case ctx.stack of
                    (WUri (HttpsPath url)) :: rest ->
                        Ok <|
                            Haltable.HaltAfter
                                ( { ctx | stack = rest }
                                , HttpRequest
                                    { method = "GET"
                                    , headers = []
                                    , url = "https://" ++ url
                                    , body = Http.emptyBody
                                    , expect = Http.expectString HttpResponse
                                    , timeout = Nothing
                                    , tracker = Nothing
                                    }
                                )

                    _ ->
                        Err "expected an URI"
          )
        , ( "readWith"
          , \ctx ->
                case ctx.stack of
                    (WUri (HttpsPath url)) :: (WMap rec) :: rest ->
                        Ok <|
                            Haltable.HaltAfter
                                ( { ctx | stack = rest }
                                , HttpRequest
                                    { method = "GET"
                                    , headers =
                                        Dict.foldl
                                            (\key val headers ->
                                                case val of
                                                    WString s ->
                                                        Http.header key s :: headers

                                                    WInt i ->
                                                        Http.header key (String.fromInt i) :: headers

                                                    _ ->
                                                        headers
                                            )
                                            []
                                            rec
                                    , url = "https://" ++ url
                                    , body = Http.emptyBody
                                    , expect = Http.expectString HttpResponse
                                    , timeout = Nothing
                                    , tracker = Nothing
                                    }
                                )

                    _ ->
                        Err "expected an URI"
          )
        ]


validateAliasedUseUri : Uri -> String -> Result () ( Uri, String )
validateAliasedUseUri uri aliasName =
    case String.uncons aliasName of
        Nothing ->
            -- Err (InvalidAlias aliasSpan aliasName)
            Err ()

        Just ( firstChar, restChars ) ->
            if Source.validWordStart firstChar && List.all Source.validWordMiddle (String.toList restChars) then
                case uriToUse uri of
                    Err err ->
                        Err err

                    Ok ( validUri, _ ) ->
                        Ok ( validUri, aliasName )

            else
                -- Err (InvalidAlias aliasSpan aliasName)
                Err ()


validateUseUri : Uri -> Result () ( Uri, String )
validateUseUri uri =
    case uriToUse uri of
        Err err ->
            Err err

        Ok ( validUri, name ) ->
            Ok ( validUri, name )


uriToUse : Uri -> Result () ( Uri, String )
uriToUse uri =
    case uri of
        FilePath path ->
            case String.split "/" path |> List.reverse of
                [] ->
                    -- Err (InvalidUseFilePath span path)
                    Err ()

                fileName :: _ ->
                    let
                        withoutExtension =
                            String.dropRight 3 fileName
                    in
                    case String.uncons withoutExtension of
                        Nothing ->
                            -- Err (InvalidFileName span withoutExtension)
                            Err ()

                        Just ( first, rest ) ->
                            if Source.validWordStart first && List.all Source.validWordMiddle (String.toList rest) then
                                Ok ( FilePath path, withoutExtension )

                            else
                                -- Err (InvalidFileName span withoutExtension)
                                Err ()

        HttpPath path ->
            case String.split "/" path |> List.reverse of
                [] ->
                    -- Err (InvalidUseFilePath span path)
                    Err ()

                fileName :: _ ->
                    let
                        withoutExtension =
                            String.dropRight 3 fileName
                    in
                    case String.uncons withoutExtension of
                        Nothing ->
                            -- Err (InvalidFileName span withoutExtension)
                            Err ()

                        Just ( first, rest ) ->
                            if Source.validWordStart first && List.all Source.validWordMiddle (String.toList rest) then
                                Ok ( FilePath path, withoutExtension )

                            else
                                -- Err (InvalidFileName span withoutExtension)
                                Err ()

        HttpsPath path ->
            case String.split "/" path |> List.reverse of
                [] ->
                    -- Err (InvalidUseFilePath span path)
                    Err ()

                fileName :: _ ->
                    let
                        withoutExtension =
                            String.dropRight 3 fileName
                    in
                    case String.uncons withoutExtension of
                        Nothing ->
                            -- Err (InvalidFileName span withoutExtension)
                            Err ()

                        Just ( first, rest ) ->
                            if Source.validWordStart first && List.all Source.validWordMiddle (String.toList rest) then
                                Ok ( FilePath path, withoutExtension )

                            else
                                -- Err (InvalidFileName span withoutExtension)
                                Err ()


type Msg
    = Eval (List Word)
    | Continue
    | HttpResponse (Result Http.Error String)
    | UseResponse String (Result Http.Error String)


run : List (Located Source.Word) -> Model -> ( Model, Cmd Msg )
run words model =
    update (Eval (List.filterMap mapSourceWord words)) model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.mode of
        Evaling ->
            case msg of
                Continue ->
                    case model.toEval of
                        [] ->
                            ( model, Cmd.none )

                        toEval ->
                            case Haltable.foldlResult evalWord ( model.context, None ) toEval of
                                Haltable.Success ( ctx, effect ) ->
                                    runEffect
                                        { model | context = ctx }
                                        effect

                                Haltable.Crashed err ->
                                    ( model, Cmd.none )

                                Haltable.ResultHalted remainingWords ( ctx, effect ) ->
                                    runEffect
                                        { model | context = ctx, toEval = remainingWords }
                                        effect

                Eval words ->
                    case Haltable.foldlResult evalWord ( model.context, None ) words of
                        Haltable.Success ( ctx, effect ) ->
                            runEffect
                                { model | context = ctx }
                                effect

                        Haltable.Crashed err ->
                            ( model, Cmd.none )

                        Haltable.ResultHalted remainingWords ( ctx, effect ) ->
                            runEffect
                                { model | context = ctx, toEval = remainingWords ++ model.toEval }
                                effect

                HttpResponse (Err err) ->
                    Debug.todo ""

                HttpResponse (Ok data) ->
                    ( { model
                        | context =
                            let
                                context =
                                    model.context
                            in
                            { context | stack = WString data :: context.stack }
                      }
                    , Cmd.none
                    )

                UseResponse _ (Err err) ->
                    Debug.todo ""

                UseResponse namespace (Ok data) ->
                    case Source.parse data of
                        Err err ->
                            Debug.todo ""

                        Ok words ->
                            case Canonical.fromSource words of
                                Err err ->
                                    Debug.todo ""

                                Ok ( file, warnings ) ->
                                    ( { model
                                        | context =
                                            let
                                                context =
                                                    model.context
                                            in
                                            { context
                                                | definitions =
                                                    List.map
                                                        (\definition ->
                                                            ( ( namespace, definition.name ), List.map mapCanonicalWord definition.body )
                                                        )
                                                        file.definitions
                                                        ++ context.definitions
                                            }
                                      }
                                    , file.uses
                                        |> List.filterMap
                                            (\use ->
                                                case use.uri of
                                                    Canonical.FilePath path ->
                                                        Debug.todo ""

                                                    Canonical.HttpPath path ->
                                                        Debug.todo ""

                                                    Canonical.HttpsPath path ->
                                                        Http.request
                                                            { method = "GET"
                                                            , headers = []
                                                            , url = "https://" ++ path
                                                            , body = Http.emptyBody
                                                            , expect = Http.expectString (UseResponse (Maybe.withDefault use.name use.alias_))
                                                            , timeout = Nothing
                                                            , tracker = Nothing
                                                            }
                                                            |> Just
                                            )
                                        |> Cmd.batch
                                    )

        Compiling name body ->
            case msg of
                Continue ->
                    case model.toEval of
                        [] ->
                            ( model, Cmd.none )

                        toEval ->
                            case Haltable.foldl compileWord body toEval of
                                Haltable.Complete newBody ->
                                    ( { model | mode = Compiling name newBody }
                                    , Cmd.none
                                    )

                                Haltable.Halted remainingWords completeBody ->
                                    let
                                        context =
                                            model.context
                                    in
                                    ( { model
                                        | mode = Evaling
                                        , toEval = remainingWords
                                        , context = { context | definitions = ( ( "", name ), completeBody ) :: context.definitions }
                                      }
                                    , Task.perform identity (Task.succeed Continue)
                                    )

                Eval words ->
                    case Haltable.foldl compileWord body words of
                        Haltable.Complete newBody ->
                            ( { model | mode = Compiling name newBody }
                            , Cmd.none
                            )

                        Haltable.Halted remainingWords completeBody ->
                            let
                                context =
                                    model.context
                            in
                            ( { model
                                | mode = Evaling
                                , toEval = remainingWords ++ model.toEval
                                , context = { context | definitions = ( ( "", name ), completeBody ) :: context.definitions }
                              }
                            , Task.perform identity (Task.succeed Continue)
                            )

                HttpResponse (Err err) ->
                    Debug.todo ""

                HttpResponse (Ok data) ->
                    Debug.todo ""

                UseResponse _ (Err err) ->
                    Debug.todo ""

                UseResponse _ (Ok data) ->
                    Debug.todo ""


runEffect : Model -> Effect Msg -> ( Model, Cmd Msg )
runEffect model effect =
    case effect of
        None ->
            ( model, Cmd.none )

        StepInto words ->
            ( model, Task.perform Eval (Task.succeed words) )

        SwitchToCompile name ->
            ( { model | mode = Compiling name [] }, Task.perform identity (Task.succeed Continue) )

        HttpRequest request ->
            ( model
            , Http.request request
            )


type Effect msg
    = None
    | StepInto (List Word)
    | SwitchToCompile String
    | HttpRequest Request


type alias Request =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect Msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }


evalWord : Word -> ( Context, Effect Msg ) -> Result String (Haltable.Step ( Context, Effect Msg ))
evalWord word ( context, _ ) =
    case word of
        WString _ ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WChar _ ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WWord name ->
            case listDictFind ( "", name ) context.definitions of
                Nothing ->
                    case Dict.get name builtins of
                        Nothing ->
                            Debug.todo ""

                        Just builtin ->
                            builtin context

                Just def ->
                    Ok <| Haltable.HaltAfter ( context, StepInto def )

        WBuiltin name ->
            case Dict.get name builtins of
                Nothing ->
                    Debug.todo ""

                Just builtin ->
                    builtin context

        WNamespacedWord namespace name ->
            case listDictFind ( namespace, name ) context.definitions of
                Nothing ->
                    Debug.todo ""

                Just def ->
                    Ok <|
                        Haltable.HaltAfter
                            ( context, StepInto def )

        WInt _ ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WFloat _ ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WMap _ ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WGet key ->
            case context.stack of
                (WMap rec) :: rest ->
                    case Dict.get key rec of
                        Nothing ->
                            Debug.todo ""

                        Just value ->
                            Ok <| Haltable.Continue ( { context | stack = value :: context.stack }, None )

                _ ->
                    Debug.todo ""

        WSet key ->
            case context.stack of
                (WMap rec) :: newValue :: rest ->
                    Ok <|
                        Haltable.Continue
                            ( { context
                                | stack =
                                    WMap
                                        (Dict.update key
                                            (Maybe.map (\_ -> newValue))
                                            rec
                                        )
                                        :: rest
                              }
                            , None
                            )

                _ ->
                    Debug.todo ""

        WUri uri ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WNamed name ->
            Ok <| Haltable.HaltAfter ( context, SwitchToCompile name )

        WNamedEnd ->
            Ok <| Haltable.Continue ( context, None )

        WQuote _ ->
            Ok <| Haltable.Continue ( { context | stack = word :: context.stack }, None )

        WVariable name ->
            case context.stack of
                w :: rest ->
                    Ok <|
                        Haltable.Continue
                            ( { context
                                | stack = rest
                                , definitions =
                                    if String.startsWith ":_" name then
                                        context.definitions

                                    else
                                        ( ( "", String.dropLeft 1 name ), [ w ] ) :: context.definitions
                              }
                            , None
                            )

                _ ->
                    Debug.todo ""


compileWord : Word -> List Word -> Haltable.Step (List Word)
compileWord word body =
    case word of
        WString _ ->
            Haltable.Continue (word :: body)

        WChar _ ->
            Haltable.Continue (word :: body)

        WWord _ ->
            Haltable.Continue (word :: body)

        WBuiltin _ ->
            Haltable.Continue (word :: body)

        WNamespacedWord _ _ ->
            Haltable.Continue (word :: body)

        WInt _ ->
            Haltable.Continue (word :: body)

        WFloat _ ->
            Haltable.Continue (word :: body)

        WMap _ ->
            Haltable.Continue (word :: body)

        WGet _ ->
            Haltable.Continue (word :: body)

        WSet _ ->
            Haltable.Continue (word :: body)

        WUri _ ->
            Haltable.Continue (word :: body)

        WNamed name ->
            Debug.todo ""

        WNamedEnd ->
            Haltable.HaltAfter body

        WQuote _ ->
            Haltable.Continue (word :: body)

        WVariable _ ->
            Haltable.Continue (word :: body)


listDictFind : key -> List ( key, value ) -> Maybe value
listDictFind key values =
    case values of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if key == k then
                Just v

            else
                listDictFind key rest


listDictFindBy : (key -> Bool) -> List ( key, value ) -> Maybe value
listDictFindBy fn values =
    case values of
        [] ->
            Nothing

        ( k, v ) :: rest ->
            if fn k then
                Just v

            else
                listDictFindBy fn rest


mapSourceWord : Located Source.Word -> Maybe Word
mapSourceWord (Located _ sourceWord) =
    case sourceWord of
        Source.WString string ->
            Just (WString string)

        Source.WChar char ->
            Just (WChar char)

        Source.WWord word ->
            Just (WWord word)

        Source.WInt int ->
            Just (WInt int)

        Source.WFloat float ->
            Just (WFloat float)

        Source.WMap pairs ->
            pairs
                |> List.filterMap
                    (\( Located _ key, value ) ->
                        case key of
                            Source.WNamed k ->
                                case mapSourceWord value of
                                    Nothing ->
                                        Nothing

                                    Just val ->
                                        Just ( k, val )

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
                |> WMap
                |> Just

        Source.WGet key ->
            Just (WGet key)

        Source.WSet key ->
            Just (WSet key)

        Source.WUri uri ->
            case uri of
                Source.FilePath (Located _ path) ->
                    Just (WUri (FilePath path))

                Source.HttpPath (Located _ path) ->
                    Just (WUri (HttpPath path))

                Source.HttpsPath (Located _ path) ->
                    Just (WUri (HttpsPath path))

                Source.UnknownUri _ _ ->
                    Nothing

        Source.WNamed name ->
            Just (WNamed name)

        Source.WNamedEnd ->
            Just WNamedEnd

        Source.WQuote words ->
            Just (WQuote (List.filterMap mapSourceWord words))

        Source.WVariable var ->
            Just (WVariable var)

        Source.WBuiltin name ->
            Just (WBuiltin name)

        Source.WNamespacedWord (Located _ namespace) (Located _ name) ->
            Just (WNamespacedWord namespace name)

        Source.WComment _ ->
            Nothing

        Source.WDocComment _ ->
            Nothing

        Source.WTypeDef _ ->
            Nothing


mapCanonicalWord : Canonical.Word -> Word
mapCanonicalWord canonicalWord =
    case canonicalWord of
        Canonical.WString string ->
            WString string

        Canonical.WChar char ->
            WChar char

        Canonical.WWord word ->
            WWord word

        Canonical.WInt int ->
            WInt int

        Canonical.WFloat float ->
            WFloat float

        Canonical.WMap dict ->
            dict
                |> Dict.foldl
                    (\key value acc ->
                        case List.map mapCanonicalWord value of
                            [ val ] ->
                                Dict.insert key val acc

                            _ ->
                                acc
                    )
                    Dict.empty
                |> WMap

        Canonical.WUri uri ->
            case uri of
                Canonical.FilePath path ->
                    WUri (FilePath path)

                Canonical.HttpPath path ->
                    WUri (HttpPath path)

                Canonical.HttpsPath path ->
                    WUri (HttpsPath path)

        Canonical.WQuote words ->
            WQuote (List.map mapCanonicalWord words)

        Canonical.WVariable var ->
            WVariable var

        Canonical.WBuiltin name ->
            WBuiltin name

        Canonical.WNamespacedWord namespace name ->
            WNamespacedWord namespace name
