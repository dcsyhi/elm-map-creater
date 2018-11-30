module Board exposing(..)

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
import Tile exposing (..)
import TypedSvg exposing (..)
import TypedSvg.Attributes as SvgAt exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)

-- the number of maximum matrix size
max = 8

-- 座標を作成するための型

type alias Point a =
    -- this means Point (x,y) and additional type
    { x : Int
    , y : Int
    , point : Array a
    }

-- マス目を作るためにまず座標のリストを作成する


createList : Int -> Int -> List (Point String)
createList i j =
    case j of
        0 ->
            Point 0 0 (Array.fromList [ "nothing" ]) :: []

        _ ->
            Point i j (Array.fromList [ "nothing" ]) :: createList i (j - 1)



-- リストをSelectList化


createSelectList : Int -> SelectList.SelectList (Point String)
createSelectList n =
    SelectList.fromLists []
        (Point 0 0 (Array.fromList [ "nothing" ]))
        (createList n (max - 1) |> List.reverse)
        |> SelectList.attempt SelectList.delete


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.map2 Tuple.pair xs ys


-- クォータービューを実装するために座標変換後のリストを作成する


createXYList : Int -> List ( Float, Float )
createXYList n =
    let
        a =
            createSelectList n
                |> SelectList.attempt SelectList.delete
                |> SelectList.toList
                |> List.map .x
                |> List.map toFloat

        b =
            createSelectList n
                |> SelectList.attempt SelectList.delete
                |> SelectList.toList
                |> List.map .y
                |> List.map toFloat
    in
    zip a b



-- クォータービューのSelectListを作成する


outputQuarterLine : Int -> SelectList.SelectList ( Float, Float )
outputQuarterLine n =
    createXYList n
        |> List.map (\( x, y ) -> ( quarterX x y, quarterY x y ))
        |> SelectList.fromList
        |> Maybe.withDefault (SelectList.singleton ( 0.0, 0.0 ))


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



drawLine : Int -> Svg msg
drawLine i =
    polyline
        [ SvgAt.fill FillNone
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


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


outputRow : Int -> Float -> List ( Float, Float )
outputRow num py =
    outputLine num py |> List.map swap



drawRow : Int -> Svg msg
drawRow i =
    polyline
        [ SvgAt.fill FillNone
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
            svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
                List.foldr (::) (drawRowList (max + 1)) (drawLineList (max + 1))


-- Tile.elmで作成した関数を使ってタイルチップを配置（実際には単純に３つの平面を合成しただけ）
stackCube : Int -> Int -> List (Svg msg)
stackCube n i =
    let
        count =
            stackQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [polygon
                [ SvgAt.fill (Fill <| Color.rgb255 252 250 242 )
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                 [ SvgAt.fill (Fill <| Color.rgb255 252 250 242 )
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
             , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 252 250 242 )
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]

stackCubeList : Int -> Int -> List(Svg msg)
stackCubeList n i =
   case n of
      0 -> []
      _ -> List.foldr(::) (stackCube n i) (stackCubeList (n-1) i)


arrangeCube : Int -> Int -> List (Svg msg)
arrangeCube n i =
    let
        count =
            outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterLine n
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [polygon
                [ SvgAt.fill FillNone
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity <| Opacity 0.0
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                 [ SvgAt.fill FillNone
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity <| Opacity 0.0
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
             , polygon
                [ SvgAt.fill FillNone
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity <| Opacity 0.0
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]
            
            
arrangeCubeList : Int -> Int -> List(Svg msg)
arrangeCubeList n i =
   case n of
      0 -> []
      _ -> List.foldr(::) (arrangeCube n i) (arrangeCubeList (n-1) i)




-- マス目に色のついたタイルを置くための関数（赤、黄、緑、青の４色を用意）

drawRectsRed : Int -> Int -> Svg msg
drawRectsRed i j =
    rect
        [ x <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList max |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , SvgAt.width (px 50)
        , SvgAt.height (px 50)
        , SvgAt.fill (Fill (Color.rgb255 208 16 76))
        , stroke Color.black
        , strokeWidth (pt 1.0)
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
        , SvgAt.width (px 50)
        , SvgAt.height (px 50)
        , SvgAt.fill (Fill (Color.rgb255 0 92 175))
        , stroke Color.black
        , strokeWidth (pt 1.0)
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
        , SvgAt.width (px 50)
        , SvgAt.height (px 50)
        , SvgAt.fill (Fill (Color.rgb255 27 129 62))
        , fillOpacity (Opacity <| 0.9)
        , stroke Color.black
        , strokeWidth (pt 1.0)
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
        , SvgAt.width (px 50)
        , SvgAt.height (px 50)
        , SvgAt.fill (Fill (Color.rgb255 239 187 36))
        , stroke Color.black
        , strokeWidth (pt 1.0)
        ]
        []


-- マス目に点を打つための関数


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
        
        
stackQuarterLine : Int -> SelectList.SelectList ( Float, Float )
stackQuarterLine n =
    createXYList n
        |> List.map (\( x, y ) -> ( quarterX x y , quarterY x y))
        |> List.map (\( x, y) -> ( x, (y + h /  2)))
        |> SelectList.fromList
        |> Maybe.withDefault (SelectList.singleton ( 0.0, 0.0 ))


