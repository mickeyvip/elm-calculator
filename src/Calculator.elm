module Calculator exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide


type Action
    = Clear
    | Delete


type Msg
    = ClickDigit Int
    | ClickAction Action
    | ClickOperator Operator
    | ClickEquals
    | ZoomIn
    | ZoomOut
    | CharacterKey Char
    | ControlKey String



-- MODEL


type alias Model =
    { operand1 : Float
    , operand2 : Maybe Float
    , operator : Maybe Operator
    , zoomFactor : Float
    }


initialModel : Model
initialModel =
    { operand1 = 0.0
    , operand2 = Nothing
    , operator = Nothing
    , zoomFactor = 1.0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickDigit digit ->
            let
                newModel =
                    if model.operator == Nothing then
                        { model | operand1 = model.operand1 * 10 + toFloat digit }

                    else
                        case model.operand2 of
                            Nothing ->
                                { model | operand2 = Just (toFloat digit) }

                            Just op2 ->
                                { model | operand2 = Just (op2 * 10 + toFloat digit) }
            in
            ( newModel, Cmd.none )

        ClickAction Clear ->
            ( initialModel, Cmd.none )

        ClickAction Delete ->
            let
                newOperand1 =
                    model.operand1
                        / 10
                        |> floor
                        |> toFloat
            in
            ( { model | operand1 = newOperand1 }, Cmd.none )

        ClickOperator op ->
            ( { model | operator = Just op }, Cmd.none )

        ClickEquals ->
            let
                op2 =
                    Maybe.withDefault 0 model.operand2

                op1 =
                    model.operand1

                result =
                    Maybe.map
                        (\op ->
                            case op of
                                Plus ->
                                    op1 + op2

                                Minus ->
                                    op1 - op2

                                Multiply ->
                                    op1 * op2

                                Divide ->
                                    op1 / op2
                        )
                        model.operator
            in
            case result of
                Just res ->
                    ( { initialModel | operand1 = res }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ZoomIn ->
            ( { model | zoomFactor = model.zoomFactor + 0.1 }, Cmd.none )

        ZoomOut ->
            ( { model | zoomFactor = model.zoomFactor - 0.1 }, Cmd.none )

        CharacterKey char ->
            if Char.isDigit char then
                let
                    digitNumber =
                        char
                            |> String.fromChar
                            |> String.toInt
                            |> Maybe.withDefault 0
                in
                update (ClickDigit digitNumber) model

            else
                case char of
                    '+' ->
                        update (ClickOperator Plus) model

                    '-' ->
                        update (ClickOperator Minus) model

                    '*' ->
                        update (ClickOperator Multiply) model

                    '/' ->
                        update (ClickOperator Divide) model

                    'c' ->
                        update (ClickAction Clear) model

                    'C' ->
                        update (ClickAction Clear) model

                    _ ->
                        ( model, Cmd.none )

        ControlKey string ->
            let
                _ =
                    Debug.log "ControlKey" string
            in
            case string of
                "Enter" ->
                    update ClickEquals model

                _ ->
                    ( model, Cmd.none )


operatorDivide =
    '÷'


operatorMultiply =
    '×'


operatorPlus =
    '+'


operatorMinus =
    '−'


operatorEquals =
    '='


actionClear =
    'C'


actionDelete =
    '⌫'


operatorToString : Operator -> String
operatorToString operator =
    String.fromChar <|
        case operator of
            Plus ->
                operatorPlus

            Minus ->
                operatorMinus

            Multiply ->
                operatorMultiply

            Divide ->
                operatorDivide


charToHtml : Char -> Html msg
charToHtml operatorChar =
    operatorChar
        |> String.fromChar
        |> text


view : Model -> Html Msg
view model =
    let
        operand1String =
            String.fromFloat model.operand1

        operatorString =
            model.operator
                |> Maybe.map operatorToString
                |> Maybe.withDefault ""

        operand2String =
            model.operand2
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault ""
    in
    div [ class "calculator", style "zoom" (String.fromFloat model.zoomFactor) ]
        [ table []
            [ thead []
                [ tr []
                    [ th [ colspan 5, class "display" ]
                        [ text <| operand1String ++ operatorString ++ operand2String ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [ class "delete", onClick (ClickAction Clear) ] [ charToHtml actionClear ]
                    , td [ onClick (ClickDigit 7) ] [ text "7" ]
                    , td [ onClick (ClickDigit 8) ] [ text "8" ]
                    , td [ onClick (ClickDigit 9) ] [ text "9" ]
                    , td [ class "operator", onClick (ClickOperator Divide) ] [ charToHtml operatorDivide ]
                    ]
                , tr []
                    [ td [ onClick ZoomIn ]
                        [ svg []
                            [ use [ xlinkHref "public/icons.svg#icon-zoomin" ] [] ]
                        ]
                    , td [ onClick (ClickDigit 4) ] [ text "4" ]
                    , td [ onClick (ClickDigit 5) ] [ text "5" ]
                    , td [ onClick (ClickDigit 6) ] [ text "6" ]
                    , td [ class "operator", onClick (ClickOperator Multiply) ] [ charToHtml operatorMultiply ]
                    ]
                , tr []
                    [ td [ onClick ZoomOut ]
                        [ svg []
                            [ use [ xlinkHref "public/icons.svg#icon-zoomout" ] [] ]
                        ]
                    , td [ onClick (ClickDigit 1) ] [ text "1" ]
                    , td [ onClick (ClickDigit 2) ] [ text "2" ]
                    , td [ onClick (ClickDigit 3) ] [ text "3" ]
                    , td [ class "operator", onClick (ClickOperator Minus) ] [ charToHtml operatorMinus ]
                    ]
                , tr []
                    [ td [ class "delete", onClick (ClickAction Delete) ] [ charToHtml actionDelete ]
                    , td [ onClick (ClickDigit 0) ] [ text "0" ]
                    , td [] [ text "." ]
                    , td [ class "operator", onClick ClickEquals ] [ charToHtml operatorEquals ]
                    , td [ class "operator", onClick (ClickOperator Plus) ] [ charToHtml operatorPlus ]
                    ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            let
                _ =
                    Debug.log "char" char
            in
            CharacterKey char

        _ ->
            let
                _ =
                    Debug.log "keyValue" keyValue
            in
            ControlKey keyValue


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
