module Main exposing (Model, Msg(..), buttonChangeTile, drawObjects, drawQuarterBoard, drawQuarterObjects, init, main, update, view)

import Array exposing (..)
import Board
import Browser
import Color exposing (Color)
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)
import SelectList
import Tile exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes as SvgAt exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)



---- MODEL ----


type alias Model =
    { rowCount : Int
    , lineCount : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { rowCount = 0
      , lineCount = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | MoveRow
    | MoveLine
    | Reset


{-| TO DO: Something explanations
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveRow ->
            ( { model | rowCount = model.rowCount + 1 }, Cmd.none )

        MoveLine ->
            ( { model | lineCount = model.lineCount + 1 }, Cmd.none )

        Reset ->
            init

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


buttonChangeTile : Html Msg
buttonChangeTile =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ Html.text "Your Elm App is working!" ]
        , button [ onClick MoveRow ] [ Html.text "縦に動く" ]
        , button [ onClick MoveLine ] [ Html.text "横に動く" ]
        , button [ onClick Reset ] [ Html.text "リセット" ]
        ]


drawObjects : Int -> Int -> Html Msg
drawObjects i j =
    svg
        [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ]
        [ Board.drawBoard Board.max
        , Board.drawRectsRed (modBy 6 i) (modBy 7 j)
        , Board.drawRectsBlue (modBy 6 (i + 1)) (modBy 7 (j + 1))
        , Board.drawRectsGreen (modBy 6 (i + 2)) (modBy 7 (j + 2))
        , Board.drawRectsYellow (modBy 6 (i + 3)) (modBy 7 (j + 3))

        --, drawPoints 5 5
        --, drawPoints 2 3
        ]


drawQuarterObjects : Int -> Int -> Html Msg
drawQuarterObjects i j =
    svg
        [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ]
        [ drawQuarterBoard Board.max
        , changeTileColorRed (modBy 6 i) (modBy 7 j)
        , changeTileColorBlue (modBy 6 (i + 1)) (modBy 7 (j + 1))
        , changeTileColorGreen (modBy 6 (i + 2)) (modBy 7 (j + 2))
        , changeTileColorYellow (modBy 6 (i + 3)) (modBy 7 (j + 3))
        ]


changeTileColorRed : Int -> Int -> Html Msg
changeTileColorRed n i =
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
            svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
                innerFuncTileRed (n + 1) i


changeTileColorBlue : Int -> Int -> Html Msg
changeTileColorBlue n i =
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
            svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
                innerFuncTileBlue (n + 1) i


changeTileColorYellow : Int -> Int -> Html Msg
changeTileColorYellow n i =
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
            svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
                innerFuncTileYellow (n + 1) i


changeTileColorGreen : Int -> Int -> Html Msg
changeTileColorGreen n i =
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
            svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
                innerFuncTileGreen (n + 1) i


innerFuncTileRed : Int -> Int -> List (Svg msg)
innerFuncTileRed n i =
    let
        count =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 208 16 76)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 0.9)
                , strokeWidth (pt 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


innerFuncTileBlue : Int -> Int -> List (Svg msg)
innerFuncTileBlue n i =
    let
        count =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 0 92 175)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


innerFuncTileYellow : Int -> Int -> List (Svg msg)
innerFuncTileYellow n i =
    let
        count =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 239 187 36)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


innerFuncTileGreen : Int -> Int -> List (Svg msg)
innerFuncTileGreen n i =
    let
        count =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            Board.outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 27 129 62)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 0.9)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


drawBaseCube : Int -> List (Svg msg)
drawBaseCube i =
    case i of
        0 ->
            []

        _ ->
            List.foldr (::) (Board.arrangeCubeList (Board.max - 2) i |> List.reverse) <| drawBaseCube (i - 1)


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
            svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
                drawBaseCube (i - 1)



---- MODEL ----


view : Model -> Html Msg
view model =
    div []
        [ buttonChangeTile
        , hr [] []
        , drawObjects model.lineCount model.rowCount
        , drawQuarterObjects model.lineCount (model.rowCount + 1)
        , hr [] []
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
