module Main exposing (..)

import Browser
import Canonical
import Dict
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
        [ Html.div
            [ Html.Attributes.class "editorChat" ]
            [ Html.h1
                [ Html.Attributes.style "grid-row" "1"
                , Html.Attributes.style "grid-column" "1 / 4"
                ]
                [ Html.text "Moss on a 🪨" ]
            , Html.code []
                [ model.evalModel.context.definitions
                    |> Debug.toString
                    |> Html.text
                ]
            , Html.textarea
                [ Html.Events.onInput CodeChanged
                , Html.Attributes.value model.code
                , Html.Events.on "keydown" decodeTextKeydown
                ]
                []
            , Html.code []
                [ model.evalModel.context.stack
                    |> List.map viewStackItem
                    |> Html.ol []
                ]
            ]
        ]
    }


viewStackItem : Eval.Word -> Html Msg
viewStackItem word =
    Html.li
        []
        [ prettyPrintWordAbbreviated word ]


prettyPrintWordAbbreviated : Eval.Word -> Html Msg
prettyPrintWordAbbreviated word =
    case word of
        Eval.WString string ->
            Html.text ("\"" ++ string ++ "\"")

        Eval.WChar char ->
            Html.text ("'" ++ char ++ "'")

        Eval.WWord w ->
            Html.text w

        Eval.WInt int ->
            Html.text (String.fromInt int)

        Eval.WFloat float ->
            Html.text (String.fromFloat float)

        Eval.WMap dict ->
            case Dict.toList dict of
                [] ->
                    Html.text "{}"

                [ ( key, value ) ] ->
                    Html.span [] [ Html.text ("{ " ++ key ++ ": "), prettyPrintWordAbbreviated value, Html.text " }" ]

                ( key, value ) :: rest ->
                    Html.details []
                        [ Html.summary []
                            [ Html.span [] [ Html.text ("{ " ++ key ++ ": "), prettyPrintWordAbbreviated value, Html.text ", ... }" ] ]
                        , (( key, value ) :: rest)
                            |> List.map
                                (\( k, v ) ->
                                    Html.li []
                                        [ Html.span [] [ Html.text (k ++ ": "), prettyPrintWordAbbreviated v ] ]
                                )
                            |> Html.ul []
                        ]

        Eval.WGet _ ->
            Html.text ""

        Eval.WSet _ ->
            Html.text ""

        Eval.WUri uri ->
            Html.text ""

        Eval.WNamed _ ->
            Html.text ""

        Eval.WNamedEnd ->
            Html.text ""

        Eval.WQuote words ->
            case words of
                [] ->
                    Html.text "[]"

                [ w ] ->
                    Html.span [] [ Html.text "[", prettyPrintWordAbbreviated w, Html.text "]" ]

                w :: rest ->
                    Html.details []
                        [ Html.summary []
                            [ Html.span [] [ Html.text "[", prettyPrintWordAbbreviated w, Html.text " ...]" ] ]
                        , (w :: rest)
                            |> List.map
                                (\w_ ->
                                    Html.li []
                                        [ prettyPrintWordAbbreviated w_ ]
                                )
                            |> Html.ul []
                        ]

        Eval.WVariable var ->
            Html.text var

        Eval.WNamespacedWord namespace name ->
            Html.text (namespace ++ "." ++ name)

        Eval.WBuiltin w ->
            Html.text w


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
