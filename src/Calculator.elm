module Calculator exposing (..)

import Browser
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, style)
import Html.Events exposing (onClick)
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


update : Msg -> Model -> Model
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
            newModel

        ClickAction Clear ->
            initialModel

        ClickAction Delete ->
            let
                newOperand1 =
                    model.operand1
                        / 10
                        |> floor
                        |> toFloat
            in
            { model | operand1 = newOperand1 }

        ClickOperator op ->
            { model | operator = Just op }

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
                    { initialModel | operand1 = res }

                Nothing ->
                    model

        ZoomIn ->
            { model | zoomFactor = model.zoomFactor + 0.1 }

        ZoomOut ->
            { model | zoomFactor = model.zoomFactor - 0.1 }


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


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
