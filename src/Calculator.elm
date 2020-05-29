module Calculator exposing (..)

import Browser
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, rowspan)
import Html.Events exposing (onClick)


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


type alias Model =
    { operand1 : Float
    , operand2 : Maybe Float
    , operator : Maybe Operator
    }


initialModel : Model
initialModel =
    { operand1 = 0.0
    , operand2 = Nothing
    , operator = Nothing
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


charToHtml : Char -> Html msg
charToHtml operatorChar =
    operatorChar
        |> String.fromChar
        |> text


view : Model -> Html Msg
view model =
    let
        displayOperand =
            if model.operator == Nothing then
                model.operand1

            else
                case model.operand2 of
                    Nothing ->
                        model.operand1

                    Just op2 ->
                        op2
    in
    div [ class "calculator" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [ colspan 5, class "display" ]
                        [ text (String.fromFloat displayOperand) ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [ rowspan 2, class "delete", onClick (ClickAction Clear) ] [ charToHtml actionClear ]
                    , td [ onClick (ClickDigit 7) ] [ text "7" ]
                    , td [ onClick (ClickDigit 8) ] [ text "8" ]
                    , td [ onClick (ClickDigit 9) ] [ text "9" ]
                    , td [ class "operator", onClick (ClickOperator Divide) ] [ charToHtml operatorDivide ]
                    ]
                , tr []
                    [ td [ onClick (ClickDigit 4) ] [ text "4" ]
                    , td [ onClick (ClickDigit 5) ] [ text "5" ]
                    , td [ onClick (ClickDigit 6) ] [ text "6" ]
                    , td [ class "operator", onClick (ClickOperator Multiply) ] [ charToHtml operatorMultiply ]
                    ]
                , tr []
                    [ td [ rowspan 2, class "delete", onClick (ClickAction Delete) ] [ charToHtml actionDelete ]
                    , td [ onClick (ClickDigit 1) ] [ text "1" ]
                    , td [ onClick (ClickDigit 2) ] [ text "2" ]
                    , td [ onClick (ClickDigit 3) ] [ text "3" ]
                    , td [ class "operator", onClick (ClickOperator Minus) ] [ charToHtml operatorMinus ]
                    ]
                , tr []
                    [ td [ onClick (ClickDigit 0) ] [ text "0" ]
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
