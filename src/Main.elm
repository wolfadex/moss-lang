module Main exposing (..)

-- import CodeMirror

import Browser
import Canonical
import Editor exposing (Editor)
import EditorModel
import EditorMsg
import Eval
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Located exposing (Located(..))
import Source
import TextEditor


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { code : String
    , evalModel : Eval.Model

    -- , editor : Editor
    -- , textEditor : TextEditor.Model
    }


init : () -> ( Model, Cmd Msg )
init () =
    -- let
    --     ( editor, editorCmd ) =
    --         Editor.init
    --             { width = windowWidth 1200
    --             , height = windowHeight 700
    --             , fontSize = 16
    --             , verticalScrollOffset = 3
    --             , viewMode = EditorModel.Light
    --             , debugOn = True
    --             , viewLineNumbersOn = True
    --             , wrapOption = EditorMsg.DontWrap
    --             }
    -- in
    ( { code = ""
      , evalModel = Eval.init

      -- , editor = editor
      -- , textEditor = TextEditor.initWith """Hello
      -- World!"""
      }
    , Cmd.none
      -- Cmd.map EditorMessage editorCmd
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = CodeChanged String
    | UserWantsToRunCode
      -- | EditorMessage EditorMsg.EMsg
      -- | TextEditorMessage TextEditor.Msg
    | EvalMessage Eval.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeChanged code ->
            ( { model | code = code }
            , Cmd.none
            )

        UserWantsToRunCode ->
            case Source.parse model.code of
                Ok code ->
                    let
                        ( evalModel, effect ) =
                            Eval.run code model.evalModel
                    in
                    ( { model
                        | evalModel = evalModel
                        , code = ""
                      }
                    , Cmd.map EvalMessage effect
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        EvalMessage emsg ->
            let
                ( evalModel, effect ) =
                    Eval.update emsg model.evalModel
            in
            ( { model | evalModel = evalModel }
            , Cmd.map EvalMessage effect
            )



-- EditorMessage emsg ->
--     Editor.update emsg model.editor
--         |> Tuple.mapBoth
--             (\editor -> { model | editor = editor })
--             (Cmd.map EditorMessage)
-- TextEditorMessage tmsg ->
--     TextEditor.update
--         { toModel = \textEditor -> { model | textEditor = textEditor }
--         , toMsg = TextEditorMessage
--         }
--         tmsg
--         model.textEditor


view : Model -> Browser.Document Msg
view model =
    { title = "Moss"
    , body =
        [ Html.text "Moss on a 🪨"
        , Html.br [] []

        -- , Editor.view model.editor
        --     |> Html.map EditorMessage
        -- , TextEditor.view
        --     { toMsg = TextEditorMessage
        --     }
        --     model.textEditor
        , Html.textarea
            [ Html.Events.onInput CodeChanged
            , Html.Attributes.value model.code
            , Html.Events.on "keydown" decodeTextKeydown
            ]
            []
        , Html.br [] []
        , Html.code []
            [ model.evalModel
                |> Debug.toString
                |> Html.text
            ]
        ]
    }


type alias Key =
    { key : String
    , alt : Bool
    , shift : Bool
    , ctrl : Bool
    , meta : Bool
    , isComposing : Bool
    }


decodeTextKeydown : Json.Decode.Decoder Msg
decodeTextKeydown =
    Json.Decode.map6 Key
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "altKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)
        (Json.Decode.field "isComposing" Json.Decode.bool)
        |> Json.Decode.andThen
            (\key ->
                if key.isComposing then
                    Json.Decode.fail "we're composing"

                else
                    case key.key of
                        "Return" ->
                            if key.meta then
                                Json.Decode.succeed UserWantsToRunCode

                            else
                                Json.Decode.fail "ignore"

                        "Enter" ->
                            if key.meta then
                                Json.Decode.succeed UserWantsToRunCode

                            else
                                Json.Decode.fail "ignore"

                        _ ->
                            Json.Decode.fail "ignore"
            )


windowWidth : Float -> Float
windowWidth appWidth =
    min (0.5 * appWidth) 900


windowHeight : Float -> Float
windowHeight appHeight =
    appHeight - 70



-- sourceErrorToDiagnostic : Source.DeadEnd -> CodeMirror.Diagnostic
-- sourceErrorToDiagnostic deadEnd =
--     -- { row = Int
--     -- , col = Int
--     -- , problem = problem
--     -- , contextStack = List { row = Int, col = Int, context = context }
--     -- }
--     { from = CodeMirror.LineCol { line = deadEnd.row, col = deadEnd.col }
--     , to = Nothing
--     , severity = CodeMirror.Error
--     , message = Just (Debug.toString deadEnd.problem)
--     }
-- canonicalErrorToDiagnostic : Canonical.Error -> CodeMirror.Diagnostic
-- canonicalErrorToDiagnostic error =
--     case error of
--         Canonical.EmptyFile ->
--             { from = CodeMirror.LineCol { line = 1, col = 1 }
--             , to = Nothing
--             , severity = CodeMirror.Warning
--             , message = Just "No code to run"
--             }
--         Canonical.ExpectedGive (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Nothing -- Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Expected give:"
--             }
--         Canonical.ExpectedGiveQuote (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Expected give: [word]"
--             }
--         Canonical.CantGive (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Can't be given ..."
--             }
--         Canonical.CantUse (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Can't use ..."
--             }
--         Canonical.ExpectedUseQuote (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Expected use: [file://path]"
--             }
--         Canonical.InvalidAlias span alias_ ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just ("Invalid alias: " ++ alias_)
--             }
--         Canonical.InvalidUseUri (Located schemeSpan scheme) (Located pathSpan path) ->
--             { from = CodeMirror.LineCol { line = schemeSpan.start.row, col = schemeSpan.start.column }
--             , to = Just <| CodeMirror.LineCol { line = pathSpan.end.row, col = pathSpan.end.column }
--             , severity = CodeMirror.Error
--             , message = Just (scheme ++ "://" ++ path ++ " can't be used here")
--             }
--         Canonical.InvalidUseFilePath span path ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just ("Invalid file path " ++ path)
--             }
--         Canonical.InvalidFileName span name ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just ("Invalid file name " ++ name)
--             }
--         Canonical.InvalidDefName (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Invalid definition name"
--             }
--         Canonical.ShouldntBeIndented span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Shouldn't be indented"
--             }
--         Canonical.ShouldBeIndented span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Should be indented"
--             }
--         Canonical.InvalidKey span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Invalid key"
--             }
--         Canonical.UnexpectedNamed span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Unexpected name"
--             }
--         Canonical.UnexpectedComment span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Unexpected comment"
--             }
--         Canonical.UnexpectedDocComment span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Unexpected doc comment"
--             }
--         Canonical.UnexpectedTypeDef span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Unexpected type def"
--             }
--         Canonical.UnexpectedWord (Located span word) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Unexpected word ..."
--             }
--         Canonical.UnsupportedUri (Located span uri) ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just "Unsupported URI"
--             }
--         Canonical.ReservedWord span word ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
--             , severity = CodeMirror.Error
--             , message = Just (word ++ " can't be redefined")
--             }
-- canonicalWarningToDiagnostic : Canonical.Warning -> CodeMirror.Diagnostic
-- canonicalWarningToDiagnostic warning =
--     case warning of
--         Canonical.NoDefinitions ->
--             { from = CodeMirror.LineCol { line = 1, col = 1 }
--             , to = Nothing
--             , severity = CodeMirror.Warning
--             , message = Just "No definitions"
--             }
--         Canonical.NothingGiven span ->
--             { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column - 1 }
--             , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column - 1 }
--             , severity = CodeMirror.Warning
--             , message = Just "No definitions"
--             }
-- mossLang : CodeMirror.LanguageDef
-- mossLang =
--     { tokens =
--         [ CodeMirror.tokenRule "#.*" "comment"
--         , CodeMirror.tokenRule "\"[^\"]*\"" "string"
--         , CodeMirror.tokenRule "\\b(if|elif|else|iff|each|size|at)\\b" "keyword"
--         , CodeMirror.tokenRule "\\b\\d+\\b" "number"
--         , CodeMirror.tokenRule "[a-zA-Z_-]\\w*" "variableName"
--         , CodeMirror.tokenRule "\\b(True|False)\\b" "bool"
--         ]
--     , completions =
--         [ CodeMirror.completion "if" "keyword"
--         , CodeMirror.completion "iff" "keyword"
--         , CodeMirror.completion "elif" "keyword"
--         , CodeMirror.completion "else" "keyword"
--         , CodeMirror.completion "each" "keyword"
--         , CodeMirror.completion "size" "keyword"
--         , CodeMirror.completion "at" "keyword"
--         ]
--     }


resultMerge : Result a a -> a
resultMerge result =
    case result of
        Ok a ->
            a

        Err a ->
            a
