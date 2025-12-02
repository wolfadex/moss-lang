module Main exposing (..)

import Browser
import Canonical
import Eval
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
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
    , evalModel : Eval.Model
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { code = ""
      , evalModel = Eval.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = CodeChanged String
    | UserWantsToRunCode
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


view : Model -> Browser.Document Msg
view model =
    { title = "Moss"
    , body =
        [ Html.text "Moss on a 🪨"
        , Html.br [] []
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
