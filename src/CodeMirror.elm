module CodeMirror exposing
    ( Completion
    , Diagnostic
    , LanguageDef
    , Position(..)
    , Severity(..)
    , Theme(..)
    , TokenRule
    , completion
    , cursor
    , diagnostics
    , id
    , language
    , onChange
    , onSubmit
    , readonly
    , selection
    , theme
    , tokenRule
    , value
    , view
    )

import Html exposing (Html, node)
import Html.Attributes exposing (attribute, property)
import Html.Events exposing (on)
import Json.Decode as D
import Json.Encode as E


type Theme
    = Light
    | Dark


type alias LanguageDef =
    { tokens : List TokenRule
    , completions : List Completion
    }


type alias TokenRule =
    { pattern : String
    , token : String
    }


type alias Completion =
    { label : String
    , type_ : String
    , info : Maybe String
    , detail : Maybe String
    }


type Severity
    = Error
    | Warning


type alias Diagnostic =
    { from : Position
    , to : Maybe Position
    , severity : Severity
    , message : Maybe String
    }


tokenRule : String -> String -> TokenRule
tokenRule pattern token =
    { pattern = pattern, token = token }


completion : String -> String -> Completion
completion label type_ =
    { label = label, type_ = type_, info = Nothing, detail = Nothing }


view : List (Html.Attribute msg) -> Html msg
view attrs =
    node "code-mirror" attrs []


value : String -> Html.Attribute msg
value v =
    property "value" (E.string v)


id : String -> Html.Attribute msg
id =
    Html.Attributes.id


theme : Theme -> Html.Attribute msg
theme t =
    attribute "theme" <|
        case t of
            Light ->
                "light"

            Dark ->
                "dark"


readonly : Bool -> Html.Attribute msg
readonly r =
    if r then
        attribute "readonly" ""

    else
        property "readonly" E.null


language : LanguageDef -> Html.Attribute msg
language def =
    property "language" (encodeLanguageDef def)


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    on "change" (D.map toMsg (D.at [ "detail" ] D.string))


onSubmit : msg -> Html.Attribute msg
onSubmit msg =
    on "submit" (D.succeed msg)


diagnostics : List Diagnostic -> Html.Attribute msg
diagnostics diags =
    property "diagnostics" (E.list encodeDiagnostic diags)


type Position
    = Offset Int
    | LineCol { line : Int, col : Int }


cursor : Position -> Html.Attribute msg
cursor pos =
    property "cursor" (encodePosition pos)


selection : { from : Position, to : Position } -> Html.Attribute msg
selection sel =
    property "selection" (E.object [ ( "from", encodePosition sel.from ), ( "to", encodePosition sel.to ) ])


encodePosition : Position -> E.Value
encodePosition pos =
    case pos of
        Offset n ->
            E.int n

        LineCol { line, col } ->
            E.object [ ( "line", E.int line ), ( "col", E.int col ) ]


encodeLanguageDef : LanguageDef -> E.Value
encodeLanguageDef def =
    E.object
        [ ( "tokens", E.list encodeTokenRule def.tokens )
        , ( "completions", E.list encodeCompletion def.completions )
        ]


encodeTokenRule : TokenRule -> E.Value
encodeTokenRule rule =
    E.object
        [ ( "pattern", E.string rule.pattern )
        , ( "token", E.string rule.token )
        ]


encodeCompletion : Completion -> E.Value
encodeCompletion c =
    E.object
        [ ( "label", E.string c.label )
        , ( "type", E.string c.type_ )
        , ( "info", Maybe.withDefault E.null (Maybe.map E.string c.info) )
        , ( "detail", Maybe.withDefault E.null (Maybe.map E.string c.detail) )
        ]


encodeDiagnostic : Diagnostic -> E.Value
encodeDiagnostic d =
    E.object
        [ ( "from", encodePosition d.from )
        , ( "to", Maybe.withDefault E.null (Maybe.map encodePosition d.to) )
        , ( "message", Maybe.withDefault E.null (Maybe.map E.string d.message) )
        , ( "severity"
          , E.string <|
                case d.severity of
                    Error ->
                        "error"

                    Warning ->
                        "warning"
          )
        ]
