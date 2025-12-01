module TextEditor exposing (..)

import Array exposing (Array)
import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Task


type alias Model =
    { longestLine : Int
    , text : Array (Array String)
    , cursor : Cursor
    }


type alias Cursor =
    { line : Int
    , column : Int
    }


init : Model
init =
    { longestLine = 0
    , text = Array.empty
    , cursor = { line = 0, column = 0 }
    }


initWith : String -> Model
initWith initialText =
    let
        lines =
            String.lines initialText
    in
    { longestLine = List.foldl (\line longest -> String.length line |> max longest) 0 lines
    , text =
        lines
            |> List.map (String.split "" >> Array.fromList)
            |> Array.fromList
    , cursor = { line = 0, column = 0 }
    }


type Msg
    = NoOp
    | EditorClicked
    | CellClicked { line : Int, column : Int }
    | UserInput (Maybe String) InputType Bool
    | UserNavigation Key
    | UserKeydown Key
    | UserPasted String


update :
    { toModel : Model -> model
    , toMsg : Msg -> msg
    }
    -> Msg
    -> Model
    -> ( model, Cmd msg )
update options msg model =
    case msg of
        NoOp ->
            ( options.toModel model, Cmd.none )

        EditorClicked ->
            ( options.toModel model
            , Browser.Dom.focus "editor-focus"
                |> Task.attempt (\_ -> options.toMsg NoOp)
            )

        CellClicked { line, column } ->
            ( { model
                | cursor =
                    { line = line
                    , column = column
                    }
              }
                |> options.toModel
            , Cmd.none
            )

        UserInput data inputType isComposing ->
            ( model
                |> options.toModel
            , Cmd.none
            )

        UserKeydown key ->
            let
                updatedText =
                    case Array.get model.cursor.line model.text of
                        Nothing ->
                            model.text

                        Just line ->
                            Array.set model.cursor.line
                                (Array.append
                                    (Array.append
                                        (Array.slice 0 model.cursor.column line)
                                        (Array.fromList [ key.key ])
                                    )
                                    (Array.slice model.cursor.column (Array.length line) line)
                                )
                                model.text
            in
            ( { model
                | text = updatedText
                , cursor =
                    let
                        cursor =
                            model.cursor
                    in
                    { cursor | column = cursor.column + 1 }
                , longestLine =
                    updatedText
                        |> Array.get model.cursor.line
                        |> Maybe.map (Array.length >> max model.longestLine)
                        |> Maybe.withDefault model.longestLine
              }
                |> options.toModel
            , Cmd.none
            )

        UserPasted data ->
            ( model
                |> options.toModel
            , Cmd.none
            )

        UserNavigation key ->
            ( model
                |> options.toModel
            , Cmd.none
            )


view : { toMsg : Msg -> msg } -> Model -> Html msg
view options model =
    model.text
        |> Array.toIndexedList
        |> List.concatMap
            (\( line, chars ) ->
                Html.span
                    [ Html.Attributes.style "background" "rgb(220, 220, 220)"
                    , Html.Attributes.style "grid-column" "1"
                    , Html.Attributes.style "grid-row" (String.fromInt (line + 1))
                    ]
                    [ Html.text (String.fromInt (line + 1))
                    ]
                    :: List.map
                        (\( column, char ) ->
                            Html.span
                                [ Html.Attributes.style "grid-column" (String.fromInt (column + 2))
                                , Html.Attributes.style "grid-row" (String.fromInt (line + 1))
                                , Html.Events.custom "click" (decodeCellClick options.toMsg line column)

                                -- , Html.Attributes.style "border" "0.5px solid black"
                                ]
                                [ Html.text char ]
                        )
                        (Array.toIndexedList chars)
            )
        |> (++)
            [ Html.textarea
                [ Html.Attributes.class "textarea"
                , Html.Attributes.id "editor-focus"

                -- , Html.Events.custom "input" (decodeTextInput options.toMsg)
                , Html.Events.custom "keydown" (decodeTextKeydown options.toMsg)
                , Html.Events.custom "paste" (decodeTextPaste options.toMsg)

                -- , Html.Attributes.value "AZ"
                -- , Html.Attributes.property "selectionStart" (Json.Encode.int 1)
                -- , Html.Attributes.property "selectionEnd" (Json.Encode.int 1)
                ]
                []
            , Html.span
                [ Html.Attributes.style "grid-column" (String.fromInt (model.cursor.column + 2))
                , Html.Attributes.style "grid-row" (String.fromInt (model.cursor.line + 1))
                , Html.Attributes.class "blinking-cursor"
                ]
                []
            ]
        |> Html.div
            [ Html.Attributes.style "grid-template-columns" ("max-content repeat(" ++ String.fromInt (model.longestLine + 1) ++ ", max-content)")
            , Html.Attributes.class "editor"
            , Html.Events.onClick (options.toMsg EditorClicked)
            ]


decodeCellClick : (Msg -> msg) -> Int -> Int -> Json.Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
decodeCellClick toMsg line column =
    Json.Decode.map3
        (\x y width ->
            let
                _ =
                    Debug.log "(x, y, width)" ( x, y, width )
            in
            { message = toMsg (CellClicked { line = line, column = column })
            , stopPropagation = True
            , preventDefault = True
            }
        )
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)
        (Json.Decode.at [ "originalTarget", "___getBoundingClientRect", "width" ] Json.Decode.float)


decodeTextInput : (Msg -> msg) -> Json.Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
decodeTextInput toMsg =
    Json.Decode.map3
        (\data inputType isComposing ->
            let
                _ =
                    Debug.log "( data, inputType, isComposing )" ( data, inputType, isComposing )
            in
            { message = toMsg (UserInput data inputType isComposing)
            , stopPropagation = True
            , preventDefault = True
            }
        )
        (Json.Decode.maybe (Json.Decode.field "data" Json.Decode.string))
        (Json.Decode.field "inputType" decodeInputType)
        (Json.Decode.field "isComposing" Json.Decode.bool)


type InputType
    = InsertText
    | InsertCompositionText
    | DeleteContentBackward
    | DeleteContentForward


decodeInputType : Json.Decode.Decoder InputType
decodeInputType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\inputType ->
                case inputType of
                    "insertText" ->
                        Json.Decode.succeed InsertText

                    "insertCompositionText" ->
                        Json.Decode.succeed InsertCompositionText

                    "deleteContentBackward" ->
                        Json.Decode.succeed DeleteContentBackward

                    "deleteContentForward" ->
                        Json.Decode.succeed DeleteContentForward

                    _ ->
                        let
                            _ =
                                Debug.log "unknown inputType" inputType
                        in
                        Json.Decode.fail ("Unknown inputType: " ++ inputType)
            )


type alias Key =
    { key : String
    , alt : Bool
    , shift : Bool
    , ctrl : Bool
    , meta : Bool
    , isComposing : Bool
    }


decodeTextPaste : (Msg -> msg) -> Json.Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
decodeTextPaste toMsg =
    Json.Decode.map
        (\data ->
            { message = toMsg (UserPasted data)
            , stopPropagation = True
            , preventDefault = True
            }
        )
        (Json.Decode.at [ "clipboardData", "___getDataText" ] Json.Decode.string)


decodeTextKeydown : (Msg -> msg) -> Json.Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
decodeTextKeydown toMsg =
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
                        "ArrowRight" ->
                            Json.Decode.succeed
                                { message = toMsg (UserNavigation key)
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        "ArrowLeft" ->
                            Json.Decode.succeed
                                { message = toMsg (UserNavigation key)
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        "ArrowUp" ->
                            Json.Decode.succeed
                                { message = toMsg (UserNavigation key)
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        "ArrowDown" ->
                            Json.Decode.succeed
                                { message = toMsg (UserNavigation key)
                                , stopPropagation = True
                                , preventDefault = True
                                }

                        _ ->
                            let
                                _ =
                                    Debug.log "maybe new text key?" key
                            in
                            Json.Decode.succeed
                                { message = toMsg (UserKeydown key)
                                , stopPropagation = True
                                , preventDefault = True
                                }
            )



--
--
--
