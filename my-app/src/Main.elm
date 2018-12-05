module Main exposing (Model, Msg(..), Order, Point, buttonChangeTile, changeTileColorBlue, changeTileColorGreen, changeTileColorRed, changeTileColorYellow, createList, createSelectList, createXYList, drawBoard, drawColumn, drawColumnList, drawObjects, drawPoints, drawQuarterBoard, drawQuarterObjects, drawRectsBlue, drawRectsGreen, drawRectsRed, drawRectsYellow, drawRow, drawRowList, drawStackCube, getPointXList, getPointYList, h, inTileBlue, inTileGreen, inTileRed, inTileYellow, init, leftSide, main, maxSize, offsetX, offsetY, orderCube, outputColumn, outputColumnRev, outputQuarterColumn, outputRow, quarterX, quarterY, rightSide, stackCube, stackCubeList, stackQuarterColumn, swap, top, update, view, w, zip)

import Array exposing (..)
import Browser
import Color exposing (Color)
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)
import SelectList
import TypedSvg exposing (..)
import TypedSvg.Attributes as SvgAt exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)



---- MODEL ----


type alias Model =
    { rowCount : Int
    , columnCount : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { rowCount = 0
      , columnCount = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | MoveRow
    | MoveColumn
    | Reset


{-| TO DO: Something explanations
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveRow ->
            ( { model | rowCount = model.rowCount + 1 }, Cmd.none )

        MoveColumn ->
            ( { model | columnCount = model.columnCount + 1 }, Cmd.none )

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
        , button [ onClick MoveColumn ] [ Html.text "横に動く" ]
        , button [ onClick Reset ] [ Html.text "リセット" ]
        ]


drawObjects : Int -> Int -> Html Msg
drawObjects i j =
    svg
        [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ]
        [ drawBoard maxSize
        , drawRectsRed (modBy 6 i) (modBy 7 j)
        , drawRectsBlue (modBy 6 (i + 1)) (modBy 7 (j + 1))
        , drawRectsGreen (modBy 6 (i + 2)) (modBy 7 (j + 2))
        , drawRectsYellow (modBy 6 (i + 3)) (modBy 7 (j + 3))
        ]


type alias Order =
    { index : Int
    , element : Html Msg
    }


orderCube : Int -> Int -> List Order
orderCube i j =
    let
        countRedFirst =
            modBy (maxSize - 2) (i + 0)

        countBlueFirst =
            modBy (maxSize - 2) (i + 1)

        countYellowFirst =
            modBy (maxSize - 2) (i + 3)

        countGreenFirst =
            modBy (maxSize - 2) (i + 2)

        countRedSecond =
            modBy (maxSize - 1) (j + 0)

        countBlueSecond =
            modBy (maxSize - 1) (j + 1)

        countGreenSecond =
            modBy (maxSize - 1) (j + 2)

        countYellowSecond =
            modBy (maxSize - 1) (j + 3)

        countOrderRed =
            -countRedFirst + countRedSecond

        countOrderBlue =
            -countBlueFirst + countBlueSecond

        countOrderGreen =
            -countGreenFirst + countGreenSecond

        countOrderYellow =
            -countYellowFirst + countYellowSecond
    in
    [ Order -10 <| drawQuarterBoard maxSize 0
    , Order countOrderRed <|
        changeTileColorRed countRedFirst countRedSecond 1
    , Order countOrderRed <|
        changeTileColorRed countRedFirst countRedSecond 2
    , Order countOrderRed <|
        changeTileColorRed countRedFirst countRedSecond 3
    , Order countOrderBlue <|
        changeTileColorBlue countBlueFirst countBlueSecond 1
    , Order countOrderBlue <|
        changeTileColorBlue countBlueFirst countBlueSecond 2
    , Order countOrderBlue <|
        changeTileColorBlue countBlueFirst countBlueSecond 3
    , Order countOrderBlue <|
        changeTileColorBlue countBlueFirst countBlueSecond 4
    , Order countOrderBlue <|
        changeTileColorBlue countBlueFirst countBlueSecond 5
    , Order countOrderGreen <|
        changeTileColorGreen countGreenFirst countGreenSecond 1
    , Order countOrderGreen <|
        changeTileColorGreen countGreenFirst countGreenSecond 2
    , Order countOrderGreen <|
        changeTileColorGreen countGreenFirst countGreenSecond 3
    , Order countOrderYellow <|
        changeTileColorYellow countYellowFirst countYellowSecond 1
    , Order countOrderYellow <|
        changeTileColorYellow countYellowFirst countYellowSecond 2
    , Order countOrderYellow <|
        changeTileColorYellow countYellowFirst countYellowSecond 3
    , Order countOrderYellow <|
        changeTileColorYellow countYellowFirst countYellowSecond 4
    ]


drawQuarterObjects : Int -> Int -> Html Msg
drawQuarterObjects i j =
    svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
        List.foldr (::)
            []
            (orderCube i j |> List.sortBy .index |> List.map .element)


drawStackCube : Int -> Int -> List (Svg msg)
drawStackCube i hgt =
    case i of
        0 ->
            []

        _ ->
            List.foldr (::)
                (stackCubeList (maxSize - 2) i hgt
                    |> List.reverse
                )
            <|
                drawStackCube (i - 1) hgt


drawQuarterBoard : Int -> Int -> Html Msg
drawQuarterBoard i hgt =
    svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
        List.foldr (::) [] (drawStackCube (i - 1) hgt)



---- MODEL ----


view : Model -> Html Msg
view model =
    div []
        [ buttonChangeTile
        , hr [] []
        , drawObjects model.columnCount model.rowCount
        , hr [] []
        , drawQuarterObjects model.columnCount (model.rowCount + 1)
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



-- Board.elm
-- the number of maximum matrix size


maxSize =
    8



-- (x,y)座標と自身の情報を保持するPoint型を作成


type alias Point a =
    { x : Int
    , y : Int
    , point : Array a
    }



-- マス目を作るために座標のリストを作成する


createList : Int -> Int -> List (Point String)
createList i j =
    case j of
        0 ->
            Point 0 0 (Array.fromList [ "nothing" ]) :: []

        _ ->
            Point i j (Array.fromList [ "nothing" ]) :: createList i (j - 1)



-- リストをSelectList化（maxSizeはリストの要素の最大数-1）


createSelectList : Int -> SelectList.SelectList (Point String)
createSelectList n =
    SelectList.fromLists []
        (Point 0 0 (Array.fromList [ "nothing" ]))
        (createList n (maxSize - 1) |> List.reverse)
        -- 所望の順序と逆で出力されてしまうための対策
        |> SelectList.attempt SelectList.delete



-- 空要素を削除(うまいこと作ればこれは必要なし)
-- 格子点のx座標のリストを得る関数


getPointXList : Int -> SelectList.SelectList Float
getPointXList n =
    createSelectList n
        |> SelectList.map .x
        -- Record型なのでこれで持ってこれる
        |> SelectList.map (\m -> m * 10)
        -- pixel指定で描画するための対応処理
        |> SelectList.map toFloat



-- 格子点のy座標のリストを得る関数


getPointYList : Int -> SelectList.SelectList Float
getPointYList n =
    createSelectList n
        |> SelectList.map .y
        |> SelectList.map (\m -> m * 50)
        -- 今回のマス目作成方針の弊害
        |> SelectList.map toFloat



-- 列方向に線を描くための関数： 所望の順序と逆で出力


outputColumnRev : Int -> Float -> List ( Float, Float )
outputColumnRev columnNum orderNum =
    -- columnNum : 出力したい要素の数   orderNum : 列の何番目に線を描くか指定
    let
        head =
            getPointYList maxSize |> SelectList.selectHead
    in
    if columnNum == (head |> SelectList.index) then
        []

    else
        ( orderNum
        , getPointYList maxSize
            |> SelectList.attempt (SelectList.selectBy (columnNum - 1))
            |> SelectList.selected
        )
            :: outputColumnRev (columnNum - 1) orderNum



-- outputColumRevに対してList.inverseを適用し、出力順序を反転


outputColumn : Int -> Float -> List ( Float, Float )
outputColumn columnNum orderNum =
    outputColumnRev columnNum orderNum |> List.reverse



-- Svg化


drawColumn : Int -> Svg msg
drawColumn i =
    polyline
        [ SvgAt.fill FillNone
        , stroke Color.black
        , points <| outputColumn (maxSize + 1) (i * 50 |> toFloat)
        ]
        []



-- 再帰を使ってリスト化し、複数描けるようにする


drawColumnList : Int -> List (Svg msg)
drawColumnList i =
    case i of
        0 ->
            drawColumn 0 :: []

        _ ->
            drawColumn i :: drawColumnList (i - 1)


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


outputRow : Int -> Float -> List ( Float, Float )
outputRow rowNum orderNum =
    outputColumn rowNum orderNum |> List.map swap


drawRow : Int -> Svg msg
drawRow i =
    polyline
        [ SvgAt.fill FillNone
        , stroke Color.black
        , points <| outputRow maxSize (i * 50 |> toFloat)
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
    svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
        List.foldr (::) (drawRowList maxSize) (drawColumnList maxSize)


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


outputQuarterColumn : Int -> SelectList.SelectList ( Float, Float )
outputQuarterColumn n =
    createXYList n
        |> List.map (\( x, y ) -> ( quarterX x y, quarterY x y ))
        |> SelectList.fromList
        |> Maybe.withDefault (SelectList.singleton ( 0.0, 0.0 ))



{- stackCube:高さ方向にタイルチップを積み上げるための関数 -}


stackCube : Int -> Int -> Int -> List (Svg msg)
stackCube n i hgt =
    let
        count =
            stackQuarterColumn n hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterColumn n hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterColumn n hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 231 231 235)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 231 231 235)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 231 231 235)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]



{- 再帰を使ってstackCubeをリスト化 -}


stackCubeList : Int -> Int -> Int -> List (Svg msg)
stackCubeList n i hgt =
    case n of
        0 ->
            []

        _ ->
            List.foldr (::) (stackCube (n - 2) i hgt) (stackCubeList (n - 1) i hgt)



-- 2次元のマス目に色付きタイルを置くための関数（赤、黄、緑、青の４色を用意）


drawRectsRed : Int -> Int -> Svg msg
drawRectsRed i j =
    rect
        [ x <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList maxSize |> SelectList.selectWhileLoopBy j |> SelectList.selected)
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
                (getPointYList maxSize |> SelectList.selectWhileLoopBy j |> SelectList.selected)
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
                (getPointYList maxSize |> SelectList.selectWhileLoopBy j |> SelectList.selected)
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
                (getPointYList maxSize |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , SvgAt.width (px 50)
        , SvgAt.height (px 50)
        , SvgAt.fill (Fill (Color.rgb255 239 187 36))
        , stroke Color.black
        , strokeWidth (pt 1.0)
        ]
        []



-- クォータービューに色付きタイルを置くための関数（赤、黄、緑、青の４色を用意）


changeTileColorRed : Int -> Int -> Int -> Html Msg
changeTileColorRed n i hgt =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                inTileRed n i hgt


changeTileColorBlue : Int -> Int -> Int -> Html Msg
changeTileColorBlue n i hgt =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                inTileBlue n i hgt


changeTileColorYellow : Int -> Int -> Int -> Html Msg
changeTileColorYellow n i hgt =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                inTileYellow n i hgt


changeTileColorGreen : Int -> Int -> Int -> Html Msg
changeTileColorGreen n i hgt =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                inTileGreen n i hgt



{- 上記の関数の中身が煩雑になることを防ぐため、内部関数を用意 -}


inTileRed : Int -> Int -> Int -> List (Svg msg)
inTileRed n i hgt =
    let
        count =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterColumn (n - 1) hgt
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
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 208 16 76)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (leftSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 208 16 76)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (rightSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            ]


inTileBlue : Int -> Int -> Int -> List (Svg msg)
inTileBlue n i hgt =
    let
        count =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterColumn (n - 1) hgt
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
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 0 92 175)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (leftSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 0 92 175)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (rightSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            ]


inTileYellow : Int -> Int -> Int -> List (Svg msg)
inTileYellow n i hgt =
    let
        count =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterColumn (n - 1) hgt
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
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 239 187 36)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (leftSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 239 187 36)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (rightSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            ]


inTileGreen : Int -> Int -> Int -> List (Svg msg)
inTileGreen n i hgt =
    let
        count =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterColumn (n - 1) hgt
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterColumn (n - 1) hgt
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
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 27 129 62)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (leftSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 27 129 62)
                , stroke Color.black
                , strokeLinejoin StrokeLinejoinRound
                , fillOpacity (Opacity <| 1.0)
                , strokeWidth (pt 1.0)
                , points <|
                    (rightSide
                        |> List.map (\( a, b ) -> ( a + x, b + y ))
                    )
                ]
                []
            ]



{- drawPoints:マス目に点を打つための関数(使っていない) -}


drawPoints : Int -> Int -> Svg msg
drawPoints i j =
    circle
        [ cx <|
            px <|
                (getPointXList (i * 5) |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , cy <|
            px <|
                (getPointYList maxSize |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , r (px 10)
        ]
        []


stackQuarterColumn : Int -> Int -> SelectList.SelectList ( Float, Float )
stackQuarterColumn n hgt =
    createXYList n
        |> List.map (\( x, y ) -> ( quarterX x y, quarterY x y ))
        |> List.map (\( x, y ) -> ( x, y - (hgt |> toFloat) * h / 2 ))
        |> SelectList.fromList
        |> Maybe.withDefault (SelectList.singleton ( 0.0, 0.0 ))



-- 以下、タイルチップの実装
{- h:タイルチップの高さ(pixel単位)
   w:タイルチップの幅(pixel単位)
   offsetX:原点をX方向に指定値だけ移動させる
   offsetY:原点をY方向に指定値だけ移動させる
-}


h =
    44


w =
    44


offsetX =
    50


offsetY =
    130



{- quarterX:直交座標(x,y)をクォータービュー座標(X)に変換する関数
   quarterY:直交座標(x,y)をクォータービュー座標(Y)に変換する関数
-}


quarterX : Float -> Float -> Float
quarterX x y =
    offsetX + 0.5 * w * (x + y)


quarterY : Float -> Float -> Float
quarterY x y =
    offsetY + 0.25 * h * (-x + y)



{- クォータービューの3つの面を描画するために交点を配置 -}


top : List ( Float, Float )
top =
    [ ( 0, h / 4 ), ( w / 2, 0 ), ( w, h / 4 ), ( w / 2, h / 2 ) ]


leftSide : List ( Float, Float )
leftSide =
    [ ( 0, h / 4 ), ( w / 2, h / 2 ), ( w / 2, h ), ( 0, 3 * h / 4 ) ]


rightSide : List ( Float, Float )
rightSide =
    [ ( w / 2, h / 2 ), ( w, h / 4 ), ( w, 3 * h / 4 ), ( w / 2, h ) ]
