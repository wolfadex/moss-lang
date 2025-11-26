module Main exposing (..)

import Browser
import Canonical
import CodeMirror
import Html exposing (Html)
import Located exposing (Located(..))
import Source


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
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { code = ""
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = CodeChanged String
    | UserWantsToRunCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeChanged code ->
            ( { model | code = code }
            , Cmd.none
            )

        UserWantsToRunCode ->
            ( model
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Moss"
    , body =
        [ Html.text "Moss on a 🪨"
        , CodeMirror.view
            [ CodeMirror.value model.code
            , CodeMirror.language mossLang
            , CodeMirror.theme CodeMirror.Dark
            , CodeMirror.onChange CodeChanged
            , CodeMirror.onSubmit UserWantsToRunCode
            , model.code
                |> Source.parse
                |> Result.mapError (List.map sourceErrorToDiagnostic)
                |> Result.andThen
                    (Canonical.fromSource
                        >> Result.mapError (canonicalErrorToDiagnostic >> List.singleton)
                    )
                |> Result.map (\( _, warnings ) -> List.map canonicalWarningToDiagnostic warnings)
                |> resultMerge
                |> CodeMirror.diagnostics
            ]
        , Html.code []
            [ model.code
                |> Source.parse
                |> Result.mapError Debug.toString
                |> Result.andThen
                    (Canonical.fromSource
                        >> Result.mapError Debug.toString
                    )
                |> Debug.toString
                |> Html.text
            ]
        ]
    }


sourceErrorToDiagnostic : Source.DeadEnd -> CodeMirror.Diagnostic
sourceErrorToDiagnostic deadEnd =
    -- { row = Int
    -- , col = Int
    -- , problem = problem
    -- , contextStack = List { row = Int, col = Int, context = context }
    -- }
    { from = CodeMirror.LineCol { line = deadEnd.row, col = deadEnd.col }
    , to = Nothing
    , severity = CodeMirror.Error
    , message = Just (Debug.toString deadEnd.problem)
    }


canonicalErrorToDiagnostic : Canonical.Error -> CodeMirror.Diagnostic
canonicalErrorToDiagnostic error =
    case error of
        Canonical.EmptyFile ->
            { from = CodeMirror.LineCol { line = 1, col = 1 }
            , to = Nothing
            , severity = CodeMirror.Warning
            , message = Just "No code to run"
            }

        Canonical.ExpectedGive (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Nothing -- Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Expected give:"
            }

        Canonical.ExpectedGiveQuote (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Expected give: [word]"
            }

        Canonical.CantGive (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Can't be given ..."
            }

        Canonical.CantUse (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Can't use ..."
            }

        Canonical.ExpectedUseQuote (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Expected use: [file://path]"
            }

        Canonical.InvalidAlias span alias_ ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just ("Invalid alias: " ++ alias_)
            }

        Canonical.InvalidUseUri (Located schemeSpan scheme) (Located pathSpan path) ->
            { from = CodeMirror.LineCol { line = schemeSpan.start.row, col = schemeSpan.start.column }
            , to = Just <| CodeMirror.LineCol { line = pathSpan.end.row, col = pathSpan.end.column }
            , severity = CodeMirror.Error
            , message = Just (scheme ++ "://" ++ path ++ " can't be used here")
            }

        Canonical.InvalidUseFilePath span path ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just ("Invalid file path " ++ path)
            }

        Canonical.InvalidFileName span name ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just ("Invalid file name " ++ name)
            }

        Canonical.InvalidDefName (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Invalid definition name"
            }

        Canonical.ShouldntBeIndented span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Shouldn't be indented"
            }

        Canonical.ShouldBeIndented span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Should be indented"
            }

        Canonical.InvalidKey span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Invalid key"
            }

        Canonical.UnexpectedNamed span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Unexpected name"
            }

        Canonical.UnexpectedComment span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Unexpected comment"
            }

        Canonical.UnexpectedDocComment span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Unexpected doc comment"
            }

        Canonical.UnexpectedTypeDef span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Unexpected type def"
            }

        Canonical.UnexpectedWord (Located span word) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Unexpected word ..."
            }

        Canonical.UnsupportedUri (Located span uri) ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just "Unsupported URI"
            }

        Canonical.ReservedWord span word ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column }
            , severity = CodeMirror.Error
            , message = Just (word ++ " can't be redefined")
            }


canonicalWarningToDiagnostic : Canonical.Warning -> CodeMirror.Diagnostic
canonicalWarningToDiagnostic warning =
    case warning of
        Canonical.NoDefinitions ->
            { from = CodeMirror.LineCol { line = 1, col = 1 }
            , to = Nothing
            , severity = CodeMirror.Warning
            , message = Just "No definitions"
            }

        Canonical.NothingGiven span ->
            { from = CodeMirror.LineCol { line = span.start.row, col = span.start.column - 1 }
            , to = Just <| CodeMirror.LineCol { line = span.end.row, col = span.end.column - 1 }
            , severity = CodeMirror.Warning
            , message = Just "No definitions"
            }


mossLang : CodeMirror.LanguageDef
mossLang =
    { tokens =
        [ CodeMirror.tokenRule "#.*" "comment"
        , CodeMirror.tokenRule "\"[^\"]*\"" "string"
        , CodeMirror.tokenRule "\\b(if|elif|else|iff|each|size|at)\\b" "keyword"
        , CodeMirror.tokenRule "\\b\\d+\\b" "number"
        , CodeMirror.tokenRule "[a-zA-Z_-]\\w*" "variableName"
        , CodeMirror.tokenRule "\\b(True|False)\\b" "bool"
        ]
    , completions =
        [ CodeMirror.completion "if" "keyword"
        , CodeMirror.completion "iff" "keyword"
        , CodeMirror.completion "elif" "keyword"
        , CodeMirror.completion "else" "keyword"
        , CodeMirror.completion "each" "keyword"
        , CodeMirror.completion "size" "keyword"
        , CodeMirror.completion "at" "keyword"
        ]
    }


resultMerge : Result a a -> a
resultMerge result =
    case result of
        Ok a ->
            a

        Err a ->
            a
