module Main exposing (Model, Msg(..), Point, checkOffSelectList, createList, createSelectList, drawBoard, drawLine, drawLineList, drawObjects, drawPoints, drawRectsBlue, drawRectsGreen, drawRectsRed, drawRectsYellow, drawRow, drawRowList, getIndexElement, getIndexList, getIndexListRev, getPointXList, getPointYList, init, main, max, outputLine, outputLineRev, outputRow, sample1, selectElement, swap, update, view)

import Array exposing (..)
import Browser
import Color exposing (Color)
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, src)
import Maybe exposing (..)
import Monocle.Lens exposing (Lens, compose)
import SelectList
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
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
            [ (Point 0 0 (Array.fromList [ "nothing" ])).point
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
        [ drawBoard max
        , drawRectsRed 2 2
        , drawRectsBlue 2 0
        , drawRectsGreen 3 4
        , drawRectsYellow 5 2

        --, (Debug.log "drawPoint" drawPoints 0 1)
        --, drawPoints 5 5
        --, drawPoints 2 3
        ]


drawBoard : Int -> Svg msg
drawBoard i =
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
            svg [ width (px 300), height (px 300), viewBox 0 0 300 300 ] <|
                List.foldr (::) (drawRowList (max + 1)) (drawLineList (max + 1))


view : Model -> Html Msg
view model =
    div []
        [ sample1
        , hr [] []
        , drawObjects
        , hr [] []
        , drawObjects
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



---- MODEL ----
--  Point Type it means Point (x, y)


type alias Point a =
    { x : Int
    , y : Int
    , point : Array a
    }



-- max means the max number of lines


max =
    8



-- this function creates row of matrix


createList : Int -> Int -> List (Point String)
createList i j =
    case j of
        0 ->
            Point 0 0 (Array.fromList [ "nothing" ]) :: []

        _ ->
            Point i j (Array.fromList [ "nothing" ]) :: createList i (j - 1)


createSelectList : Int -> SelectList.SelectList (Point String)
createSelectList n =
    SelectList.fromLists [] (Point 0 0 (Array.fromList [ "nothing" ])) (createList n (max - 1) |> List.reverse)
        |> SelectList.attempt SelectList.delete



-- not used


checkOffSelectList : SelectList.SelectList (List String)
checkOffSelectList =
    let
        a =
            createSelectList 3
    in
    a |> SelectList.map .point |> SelectList.map Array.toList



-- not used


selectElement : Int -> List String
selectElement i =
    checkOffSelectList |> SelectList.attempt (SelectList.selectBy i) |> SelectList.selected



-- not used


getIndexElement : Int -> Int
getIndexElement i =
    checkOffSelectList |> SelectList.attempt (SelectList.selectBy i) |> SelectList.index



-- not used


getIndexListRev : Int -> List Int
getIndexListRev n =
    case n of
        0 ->
            getIndexElement n :: []

        _ ->
            getIndexElement n :: getIndexListRev (n - 1)



-- not used


getIndexList : Int -> List Int
getIndexList n =
    let
        a =
            n - 1
    in
    getIndexListRev a |> List.reverse


getPointXList : Int -> SelectList.SelectList Float
getPointXList n =
    createSelectList n
        |> SelectList.map .x
        |> SelectList.map (\m -> m * 10)
        |> SelectList.map toFloat


getPointYList : Int -> SelectList.SelectList Float
getPointYList n =
    createSelectList n
        |> SelectList.map .y
        |> SelectList.map (\m -> m * 50)
        |> SelectList.map toFloat


outputLineRev : Int -> Float -> List ( Float, Float )
outputLineRev num px =
    let
        py =
            max

        head =
            getPointYList py |> SelectList.selectHead
    in
    if num == (head |> SelectList.index) then
        []

    else
        ( px
        , getPointYList py
            |> SelectList.attempt (SelectList.selectBy (num - 1))
            |> SelectList.selected
        )
            :: outputLineRev (num - 1) px


outputLine : Int -> Float -> List ( Float, Float )
outputLine num px =
    outputLineRev num px |> List.reverse


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


outputRow : Int -> Float -> List ( Float, Float )
outputRow num py =
    outputLine num py |> List.map swap


drawRow : Int -> Svg msg
drawRow i =
    polyline
        [ fill FillNone
        , stroke Color.black
        , points <| outputRow max (i * 50 |> toFloat)
        ]
        []


drawRowList : Int -> List (Svg msg)
drawRowList i =
    case i of
        0 ->
            drawRow 0 :: []

        _ ->
            drawRow i :: drawRowList (i - 1)


drawLine : Int -> Svg msg
drawLine i =
    polyline
        [ fill FillNone
        , stroke Color.black
        , points <| outputLine (max + 1) (i * 50 |> toFloat)
        ]
        []


drawLineList : Int -> List (Svg msg)
drawLineList i =
    case i of
        0 ->
            drawLine 0 :: []

        _ ->
            drawLine i :: drawLineList (i - 1)



-- not used


drawPoints : Int -> Int -> Svg msg
drawPoints i j =
    circle
        [ cx <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , cy <|
            px <|
                (getPointYList max |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , r (px 10)
        ]
        []


drawRectsRed : Int -> Int -> Svg msg
drawRectsRed i j =
    rect
        [ x <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList max |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , width (px 50)
        , height (px 50)
        , rx (px 15)
        , ry (px 15)
        , fill (Fill (Color.rgb255 208 16 76))
        , fillOpacity (Opacity <| 0.9)
        , stroke (Color.rgb255 208 16 76)
        , strokeWidth (pt 2.0)
        ]
        []


drawRectsBlue : Int -> Int -> Svg msg
drawRectsBlue i j =
    rect
        [ x <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList max |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , width (px 50)
        , height (px 50)
        , rx (px 15)
        , ry (px 15)
        , fill (Fill (Color.rgb255 0 92 175))
        , fillOpacity (Opacity <| 0.9)
        , stroke (Color.rgb255 0 92 175)
        , strokeWidth (pt 2.0)
        ]
        []


drawRectsGreen : Int -> Int -> Svg msg
drawRectsGreen i j =
    rect
        [ x <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList max |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , width (px 50)
        , height (px 50)
        , rx (px 15)
        , ry (px 15)
        , fill (Fill (Color.rgb255 27 129 62))
        , fillOpacity (Opacity <| 0.9)
        , stroke (Color.rgb255 27 129 62)
        , strokeWidth (pt 2.0)
        ]
        []


drawRectsYellow : Int -> Int -> Svg msg
drawRectsYellow i j =
    rect
        [ x <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList max |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , width (px 50)
        , height (px 50)
        , rx (px 15)
        , ry (px 15)
        , fill (Fill (Color.rgb255 239 187 36))
        , fillOpacity (Opacity <| 0.9)
        , stroke (Color.rgb255 239 187 36)
        , strokeWidth (pt 2.0)
        ]
        []
