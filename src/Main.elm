module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)



---- MODEL ----


type Turn
    = X
    | O


toString : Turn -> String
toString turn =
    case turn of
        X ->
            "X"

        O ->
            "O"


type Status
    = InProgress
    | Win
    | Tie


type alias Model =
    { turn : Turn
    , spaces : List (Maybe Turn)
    , status : Status
    }


init : Model
init =
    { turn = X
    , spaces = List.repeat 9 Nothing
    , status = InProgress
    }



---- UPDATE ----


type Msg
    = PickSpace Int


pickSpace : Int -> Turn -> List (Maybe Turn) -> List (Maybe Turn)
pickSpace index turn spaces =
    List.take index spaces ++ (Just turn :: List.drop (index + 1) spaces)


update : Msg -> Model -> Model
update msg model =
    case msg of
        PickSpace index ->
            let
                updatedSpaces =
                    pickSpace index model.turn model.spaces

                updatedTurn =
                    case model.turn of
                        X ->
                            O

                        O ->
                            X
            in
            { model | spaces = updatedSpaces, turn = updatedTurn }



---- VIEW ----


viewSpace : Int -> Maybe Turn -> Html Msg
viewSpace index turn =
    div [ class "space" ]
        [ case turn of
            Just X ->
                button [ disabled True ] [ text (toString X) ]

            Just O ->
                button [ disabled True ] [ text (toString O) ]

            Nothing ->
                button [ onClick (PickSpace index) ] [ text "" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "TIC TAC TOE" ]
        , div [ class "board" ]
            (List.indexedMap viewSpace model.spaces)
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
