module Main exposing (..)

import Browser
import CodeMirror
import Html exposing (Html)


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
            ]
        ]
    }


mossLang : CodeMirror.LanguageDef
mossLang =
    { tokens =
        [ CodeMirror.tokenRule "#.*" "comment"
        , CodeMirror.tokenRule "\"[^\"]*\"" "string"
        , CodeMirror.tokenRule "\\b(if|elif|else|iff|each|size|at)\\b" "keyword"
        , CodeMirror.tokenRule "\\b\\d+\\b" "number"
        , CodeMirror.tokenRule "[a-zA-Z_-]\\w*" "variableName"
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
