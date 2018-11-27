module Main exposing (Model, Msg(..), drawObjects, drawQuarterBoard, drawQuarterObjects, init, main, sample1, update, view)

import Array exposing (..)
import Board
import Browser
import Color exposing (Color)
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, src)
import Maybe exposing (..)
import SelectList
import Tile exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes as SvgAt exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | On
    | Off


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        On ->
            ( model, Cmd.none )

        Off ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


sample1 : Html Msg
sample1 =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ Html.text "Your Elm App is working!" ]
        , p []
            [ (Board.Point 0 0 (Array.fromList [ "nothing" ])).point
                |> Array.toIndexedList
                |> Dict.fromList
                |> Dict.get 0
                |> Maybe.withDefault "nothing"
                |> Html.text
            ]
        ]


drawObjects : Html Msg
drawObjects =
    svg
        [ width (px 300), height (px 300), viewBox 0 0 300 300 ]
        [ Board.drawBoard Board.max

        --        , Board.drawRectsRed 2 2
        --        , Board.drawRectsBlue 2 0
        --        , Board.drawRectsGreen 3 4
        --        , Board.drawRectsYellow 5 2
        , Board.drawPoints 5 5
        , Board.drawPoints 2 3
        ]


drawQuarterObjects : Int -> List (Svg msg)
drawQuarterObjects i =
    case i of
        0 ->
            []

        _ ->
            List.foldr (::) (Board.arrangeCubeList i (Board.max - 1)) <| drawQuarterObjects (i - 1)


drawQuarterBoard : Int -> Html Msg
drawQuarterBoard i =
    let
        count =
            List.range 0 i
                |> SelectList.fromList
                |> Maybe.withDefault (SelectList.singleton 0)
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
    in
    case count of
        0 ->
            svg [] []

        _ ->
            svg [ width (px 400), height (px 400), viewBox 0 0 400 400 ] <|
                drawQuarterObjects i



---- MODEL ----


view : Model -> Html Msg
view model =
    div []
        [ sample1
        , hr [] []
        , drawObjects
        , hr [] []
        , drawQuarterBoard 20
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
