module Main exposing (Model, Msg(..), Order, Point, boardHeight, boardWidth, buttonMoveTile, createBuilding, createList, createSelectList, createXYList, drawBaseCube, drawBoard, drawColumn, drawColumnList, drawObjects, drawPoints, drawQuarterBoard, drawQuarterObjects, drawRectBlue, drawRectGreen, drawRectRed, drawRectYellow, drawRow, drawRowList, drawStackCube, getPointXList, getPointYList, h, inTileBlue, inTileGreen, inTileRed, inTileYellow, init, leftSide, main, maxSize, offsetX, offsetY, orderCube, outputColumn, outputColumnRev, outputQuarterColumn, outputRow, quarterX, quarterY, rightSide, stackCube, stackCubeList, stackQuarterList, stackTileColorBlue, stackTileColorGreen, stackTileColorRed, stackTileColorYellow, swap, tileHeight, tileWidth, top, update, view, w, zip)

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



---- グローバル変数 ------
{- maxSizeは原点を含む要素の最大数 -}


maxSize : Int
maxSize =
    8


boardWidth : Int
boardWidth =
    maxSize


boardHeight : Int
boardHeight =
    maxSize



{- マス目の一辺の長さ(x方向) -}


tileWidth =
    50



{- マス目の一辺の長さ(y方向) -}


tileHeight =
    50



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


buttonMoveTile : Html Msg
buttonMoveTile =
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
        [ SvgAt.width <| px <| 300
        , SvgAt.height <| px <| 300
        , viewBox 0 0 300 300
        ]
        [ drawBoard maxSize
        , drawRectRed (modBy (maxSize - 2) i) (modBy (maxSize - 2) j)
        , drawRectBlue (modBy (maxSize - 2) (i + 1)) (modBy (maxSize - 2) (j + 1))
        , drawRectGreen (modBy (maxSize - 2) (i + 2)) (modBy (maxSize - 2) (j + 2))
        , drawRectYellow (modBy (maxSize - 2) (i + 3)) (modBy (maxSize - 2) (j + 3))
        ]


type alias Order =
    { index : Int
    , element : Html Msg
    }


orderCube : Int -> Int -> List Order
orderCube i j =
    let
        countRedFirst =
            modBy (boardWidth - 2) (i + 0)

        countBlueFirst =
            modBy (boardWidth - 2) (i + 1)

        countGreenFirst =
            modBy (boardWidth - 2) (i + 2)

        countYellowFirst =
            modBy (boardWidth - 2) (i + 3)

        countRedSecond =
            modBy (boardHeight - 2) (j + 0)

        countBlueSecond =
            modBy (boardHeight - 2) (j + 1)

        countGreenSecond =
            modBy (boardHeight - 2) (j + 2)

        countYellowSecond =
            modBy (boardHeight - 2) (j + 3)

        countOrderRed =
            -countRedFirst + countRedSecond

        countOrderBlue =
            -countBlueFirst + countBlueSecond

        countOrderGreen =
            -countGreenFirst + countGreenSecond

        countOrderYellow =
            -countYellowFirst + countYellowSecond
    in
    [ Order -100 <| drawQuarterBoard maxSize maxSize 0
    , Order countOrderRed <|
        stackTileColorRed countRedFirst countRedSecond 1

    --    , Order countOrderRed <|
    --        stackTileColorRed countRedFirst countRedSecond 2
    --    , Order countOrderRed <|
    --        stackTileColorRed countRedFirst countRedSecond 3
    --    , Order countOrderBlue <|
    ]


createBuilding : Int -> Int -> List (Html Msg)
createBuilding i j =
    case i of
        0 ->
            List.foldr (::)
                [ stackTileColorRed 0 (j - 1) 1 ]
            <|
                createBuilding 0 (j - 1)

        _ ->
            case j of
                0 ->
                    []

                _ ->
                    List.foldr (::)
                        [ stackTileColorRed (i - 1) (j - 1) 1 ]
                    <|
                        createBuilding i (j - 1)


drawQuarterObjects : Int -> Int -> Html Msg
drawQuarterObjects i j =
    svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
        List.foldr (::)
            []
            (orderCube i (j + 1) |> List.sortBy .index |> List.map .element)



---- MODEL ----


view : Model -> Html Msg
view model =
    div []
        [ buttonMoveTile
        , hr [] []
        , drawObjects model.columnCount model.rowCount
        , hr [] []
        , drawQuarterObjects model.columnCount model.rowCount
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



{- (x,y)座標と自身の情報を保持するPoint型を作成 -}


type alias Point a =
    { x : Int
    , y : Int
    , point : Array a
    }



{- マス目を作るために座標のリストを作成する -}


createList : Int -> Int -> List (Point String)
createList i j =
    case j of
        0 ->
            Point 0 0 (Array.fromList [ "nothing" ]) :: []

        _ ->
            Point i j (Array.fromList [ "nothing" ]) :: createList i (j - 1)



{- リストをSelectList化（maxSizeは原点を含む要素の最大数） -}


createSelectList : Int -> SelectList.SelectList (Point String)
createSelectList n =
    SelectList.fromLists []
        (Point 0 0 (Array.fromList [ "nothing" ]))
        -- 所望の順序と逆で出力されてしまうため反転させる
        (createList n (maxSize - 1) |> List.reverse)
        -- 空要素を削除(うまいこと作ればこれは必要なし)
        |> SelectList.attempt SelectList.delete



{- 格子点のx座標のリストを得る関数 -}


getPointXList : Int -> SelectList.SelectList Float
getPointXList n =
    createSelectList n
        |> SelectList.map .x
        |> SelectList.map (\m -> m * tileWidth)
        |> SelectList.map toFloat



{- 格子点のy座標のリストを得る関数 -}


getPointYList : Int -> SelectList.SelectList Float
getPointYList n =
    createSelectList n
        |> SelectList.map .y
        |> SelectList.map (\m -> m * tileHeight)
        |> SelectList.map toFloat



{- 列方向に線を描くための関数： 所望の順序と逆で出力される -}


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



{- outputColumRevに対してList.inverseを適用し、出力順序を反転 -}


outputColumn : Int -> Float -> List ( Float, Float )
outputColumn columnNum orderNum =
    outputColumnRev columnNum orderNum |> List.reverse



{- outputColumnをSvg化する -}


drawColumn : Int -> Svg msg
drawColumn i =
    polyline
        [ SvgAt.fill FillNone
        , stroke Color.black
        , points <| outputColumn (maxSize + 1) (i * tileWidth |> toFloat)
        ]
        []



{- 再帰を使ってリスト化し、複数描けるようにする -}


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
        , points <| outputRow maxSize (i * tileHeight |> toFloat)
        ]
        []


drawRowList : Int -> List (Svg msg)
drawRowList i =
    case i of
        0 ->
            drawRow 0 :: []

        _ ->
            drawRow i :: drawRowList (i - 1)



{- List.foldr (::)`で畳み込みを行いつつ`drawColumnList`と`drawRowList`をまとめてSvg化する -}


drawBoard : Int -> Svg msg
drawBoard i =
    svg [ SvgAt.width (px 300), SvgAt.height (px 300), viewBox 0 0 300 300 ] <|
        List.foldr (::) (drawRowList maxSize) (drawColumnList maxSize)



-- クォータービューを実装するために座標変換後のリストを作成する


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.map2 Tuple.pair xs ys


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
stackCube n i stackNum =
    let
        count =
            stackQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterList n stackNum
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
stackCubeList n i stackNum =
    case n of
        0 ->
            []

        _ ->
            List.foldr (::) (stackCube (n - 2) i stackNum) (stackCubeList (n - 1) i stackNum)



-- 2次元のマス目に色付きタイルを置くための関数（赤、黄、緑、青の４色を用意）


drawRectRed : Int -> Int -> Svg msg
drawRectRed i j =
    rect
        [ x <|
            px <|
                (getPointXList i |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList j |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , SvgAt.width (px tileWidth)
        , SvgAt.height (px tileHeight)
        , SvgAt.fill (Fill (Color.rgb255 208 16 76))
        , stroke Color.black
        , strokeWidth (pt 1.0)
        ]
        []


drawRectBlue : Int -> Int -> Svg msg
drawRectBlue i j =
    rect
        [ x <|
            px <|
                (getPointXList i |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList j |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , SvgAt.width (px tileWidth)
        , SvgAt.height (px tileHeight)
        , SvgAt.fill (Fill (Color.rgb255 0 92 175))
        , stroke Color.black
        , strokeWidth (pt 1.0)
        ]
        []


drawRectGreen : Int -> Int -> Svg msg
drawRectGreen i j =
    rect
        [ x <|
            px <|
                (getPointXList i |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList j |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , SvgAt.width (px tileWidth)
        , SvgAt.height (px tileHeight)
        , SvgAt.fill (Fill (Color.rgb255 27 129 62))
        , fillOpacity (Opacity <| 0.9)
        , stroke Color.black
        , strokeWidth (pt 1.0)
        ]
        []


drawRectYellow : Int -> Int -> Svg msg
drawRectYellow i j =
    rect
        [ x <|
            px <|
                (getPointXList i |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , y <|
            px <|
                (getPointYList j |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , SvgAt.width (px tileWidth)
        , SvgAt.height (px tileHeight)
        , SvgAt.fill (Fill (Color.rgb255 239 187 36))
        , stroke Color.black
        , strokeWidth (pt 1.0)
        ]
        []



-- クォータービューに色付きタイルを置くための関数（赤、黄、緑、青の４色を用意）


stackTileColorRed : Int -> Int -> Int -> Html Msg
stackTileColorRed n i stackNum =
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
                inTileRed n i stackNum


stackTileColorBlue : Int -> Int -> Int -> Html Msg
stackTileColorBlue n i stackNum =
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
                inTileBlue n i stackNum


stackTileColorYellow : Int -> Int -> Int -> Html Msg
stackTileColorYellow n i stackNum =
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
                inTileYellow n i stackNum


stackTileColorGreen : Int -> Int -> Int -> Html Msg
stackTileColorGreen n i stackNum =
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
                inTileGreen n i stackNum



{- 上記の関数の中身が煩雑になることを防ぐため、内部関数を用意 -}


inTileRed : Int -> Int -> Int -> List (Svg msg)
inTileRed n i stackNum =
    let
        count =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterList (n - 1) stackNum
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
inTileBlue n i stackNum =
    let
        count =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterList (n - 1) stackNum
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
inTileYellow n i stackNum =
    let
        count =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterList (n - 1) stackNum
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
inTileGreen n i stackNum =
    let
        count =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            stackQuarterList (n - 1) stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            stackQuarterList (n - 1) stackNum
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


stackQuarterList : Int -> Int -> SelectList.SelectList ( Float, Float )
stackQuarterList n stackNum =
    createXYList n
        |> List.map (\( x, y ) -> ( quarterX x y, quarterY x y ))
        |> List.map (\( x, y ) -> ( x, y - (stackNum |> toFloat) * h / 2 ))
        |> SelectList.fromList
        |> Maybe.withDefault (SelectList.singleton ( 0.0, 0.0 ))


drawStackCube : Int -> Int -> List (Svg msg)
drawStackCube i stackNum =
    case i of
        0 ->
            []

        _ ->
            List.foldr (::)
                (stackCubeList (maxSize - 2) i stackNum
                    |> List.reverse
                )
            <|
                drawStackCube (i - 1) stackNum


drawBaseCube : Int -> Int -> Int -> List (Svg msg)
drawBaseCube i j stackNum =
    case i of
        0 ->
            List.foldr (::)
                (stackCubeList 0 (j - 1) stackNum
                    |> List.reverse
                )
            <|
                drawBaseCube 0 (j - 1) stackNum

        _ ->
            case j of
                0 ->
                    []

                _ ->
                    List.foldr (::)
                        (stackCubeList (i - 1) (j - 1) stackNum
                            |> List.reverse
                        )
                    <|
                        drawBaseCube i (j - 1) stackNum


drawQuarterBoard : Int -> Int -> Int -> Html Msg
drawQuarterBoard i j stackNum =
    case ( i, j ) of
        ( 0, 0 ) ->
            svg [] []

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                List.foldr (::) [] (drawBaseCube (i - 1) (j - 1) stackNum)



-- 以下、タイルチップの実装
{- h:タイルチップの高さ(pixel単位)
   w:タイルチップの幅(pixel単位)
   offsetX:原点をX方向に指定値だけ移動させる
   offsetY:原点をY方向に指定値だけ移動させる
-}


h : Float
h =
    44


w : Float
w =
    44


offsetX : Float
offsetX =
    50


offsetY : Float
offsetY =
    200



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



{- drawPoints:マス目に点を打つための関数(使っていない) -}


drawPoints : Int -> Int -> Svg msg
drawPoints i j =
    circle
        [ cx <|
            px <|
                (getPointXList i |> SelectList.selectWhileLoopBy i |> SelectList.selected)
        , cy <|
            px <|
                (getPointYList j |> SelectList.selectWhileLoopBy j |> SelectList.selected)
        , r (px 10)
        ]
        []
