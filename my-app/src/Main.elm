module Main exposing (Model, Msg(..), Order, Point, arrangeStackTiles_1, arrangeStackTiles_10, arrangeStackTiles_11, arrangeStackTiles_12, arrangeStackTiles_13, arrangeStackTiles_14, arrangeStackTiles_15, arrangeStackTiles_16, arrangeStackTiles_17, arrangeStackTiles_18, arrangeStackTiles_19, arrangeStackTiles_2, arrangeStackTiles_20, arrangeStackTiles_21, arrangeStackTiles_3, arrangeStackTiles_4, arrangeStackTiles_5, arrangeStackTiles_6, arrangeStackTiles_7, arrangeStackTiles_8, arrangeStackTiles_9, boardHeight, boardWidth, chapterTitle, createList, createSelectList, createTile, createTileList, createXYList, drawBaseTile, drawBoard, drawColumn, drawColumnList, drawObjects, drawPoints, drawQuarterBoard, drawQuarterObjects, drawRectBlue, drawRectGreen, drawRectRed, drawRectYellow, drawRow, drawRowList, getPointXList, getPointYList, h, init, innerTileBlue, innerTileBuilding, innerTileDoor, innerTileGreen, innerTileLand, innerTileRed, innerTileYellow, leftSide, main, maxElement, offsetX, offsetY, orderTile, outputColumn, outputColumnRev, outputQuarterList, outputRow, quarterX, quarterY, rightSide, stackBuildings, stackDoors, stackTileBlue, stackTileBuilding, stackTileDoor, stackTileGreen, stackTileLand, stackTileRed, stackTileYellow, stackTiles, swap, tileHeight, tileWidth, top, townOfBarumamussa, update, view, w, zip)

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
{- maxElementは原点を含む要素の最大数 -}


maxElement : Int
maxElement =
    17


boardWidth : Int
boardWidth =
    22


boardHeight : Int
boardHeight =
    maxElement



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


chapterTitle : Html Msg
chapterTitle =
    div []
        [ h2 [] [ Html.text "Chapter-1." ]
        , h2 [] [ Html.text "僕にその手を汚せというのか" ]
        , hr [] []
        ]


drawObjects : Int -> Int -> Html Msg
drawObjects i j =
    svg
        [ SvgAt.width <| px <| 300
        , SvgAt.height <| px <| 300
        , viewBox 0 0 300 300
        ]
        [ drawBoard maxElement
        , drawRectRed (modBy (maxElement - 1) i) (modBy (maxElement - 1) j)
        , drawRectBlue (modBy (maxElement - 1) (i + 1)) (modBy (maxElement - 1) (j + 1))
        , drawRectGreen (modBy (maxElement - 1) (i + 2)) (modBy (maxElement - 1) (j + 2))
        , drawRectYellow (modBy (maxElement - 1) (i + 3)) (modBy (maxElement - 1) (j + 3))
        ]


type alias Order =
    { index : Int
    , element : Html Msg
    }


orderTile : Int -> Int -> List Order
orderTile i j =
    let
        countRedFirst =
            modBy (boardWidth - 1) i

        countBlueFirst =
            modBy (boardWidth - 1) (i + 1)

        countGreenFirst =
            modBy (boardWidth - 1) (i + 2)

        countYellowFirst =
            modBy (boardWidth - 1) (i + 3)

        countRedSecond =
            modBy (boardHeight - 1) j

        countBlueSecond =
            modBy (boardHeight - 1) (j + 1)

        countGreenSecond =
            modBy (boardHeight - 1) (j + 2)

        countYellowSecond =
            modBy (boardHeight - 1) (i + 3)

        countOrderRed =
            -countRedFirst + countRedSecond

        countOrderBlue =
            -countBlueFirst + countBlueSecond

        countOrderGreen =
            -countGreenFirst + countGreenSecond

        countOrderYellow =
            -countYellowFirst + countYellowSecond
    in
    List.foldr (::) townOfBarumamussa <|
        [ Order -100 <| drawQuarterBoard boardWidth boardHeight 0 ]


townOfBarumamussa : List Order
townOfBarumamussa =
    List.concat <|
        [ arrangeStackTiles_1
        , arrangeStackTiles_2
        , arrangeStackTiles_3
        , arrangeStackTiles_4
        , arrangeStackTiles_5
        , arrangeStackTiles_6
        , arrangeStackTiles_7
        , arrangeStackTiles_8
        , arrangeStackTiles_9
        , arrangeStackTiles_10
        , arrangeStackTiles_11
        , arrangeStackTiles_12
        , arrangeStackTiles_13
        , arrangeStackTiles_14
        , arrangeStackTiles_15
        , arrangeStackTiles_16
        , arrangeStackTiles_17
        , arrangeStackTiles_18
        , arrangeStackTiles_19
        , arrangeStackTiles_20
        , arrangeStackTiles_21
        ]


arrangeStackTiles_1 : List Order
arrangeStackTiles_1 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 1 1 13 1
        , stackTiles 1 1 6 2
        , stackTiles 1 1 5 3
        , stackTiles 1 1 3 4
        , stackTiles 1 1 3 5
        , stackTiles 1 1 3 6
        , stackTiles 1 1 3 6
        , stackTiles 1 1 2 7
        ]


arrangeStackTiles_2 : List Order
arrangeStackTiles_2 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 2 1 14 1
        , stackTiles 2 1 6 2
        , stackTiles 2 1 12 2
        , stackTiles 2 1 8 3
        , stackTiles 2 1 6 4
        , stackTiles 2 1 5 5
        , stackTiles 2 1 5 6
        , stackTiles 2 1 3 7
        , stackTiles 2 1 1 8
        , stackTiles 2 1 1 9
        , stackTiles 2 1 1 10
        ]


arrangeStackTiles_3 : List Order
arrangeStackTiles_3 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 3 1 16 1
        , stackTiles 3 1 16 2
        , stackTiles 3 1 10 3
        , stackTiles 3 1 8 4
        , stackTiles 3 1 8 5
        , stackTiles 3 1 4 6
        , stackTiles 3 1 4 7
        , stackTiles 3 1 4 8
        , stackTiles 3 1 4 9
        , stackTiles 3 1 1 8
        , stackTiles 3 1 1 9
        , stackTiles 3 1 1 10
        , stackTiles 3 1 1 11
        , stackTiles 3 1 1 12
        , stackBuildings 3 5 7 6
        , stackBuildings 3 5 6 7
        , stackBuildings 3 5 6 8
        , stackBuildings 3 5 6 9
        , stackBuildings 3 5 6 10
        ]


arrangeStackTiles_4 : List Order
arrangeStackTiles_4 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 4 1 16 1
        , stackTiles 4 1 16 2
        , stackTiles 4 1 15 3
        , stackTiles 4 1 9 4
        , stackTiles 4 1 9 5
        , stackTiles 4 1 9 6
        , stackBuildings 4 5 7 6
        , stackBuildings 4 5 5 7
        , stackBuildings 4 5 5 8
        , stackBuildings 4 5 5 9
        , stackDoors 4 6 6 7
        , stackDoors 4 6 6 8
        , stackBuildings 4 6 6 9
        , stackTiles 4 1 4 9
        , stackTiles 4 1 3 10
        , stackTiles 4 1 3 11
        , stackTiles 4 1 3 12
        , stackTiles 4 1 3 13
        , stackBuildings 4 5 6 10
        ]


arrangeStackTiles_5 : List Order
arrangeStackTiles_5 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 5 1 16 1
        , stackTiles 5 1 16 2
        , stackTiles 5 1 15 3
        , stackTiles 5 1 13 4
        , stackTiles 5 1 12 5
        , stackTiles 5 1 11 6
        , stackTiles 5 1 10 7
        , stackTiles 5 1 9 8
        , stackTiles 5 1 8 9
        , stackTiles 5 1 7 10
        , stackTiles 5 1 6 11
        , stackTiles 5 1 5 12
        , stackTiles 5 1 4 13
        , stackTiles 5 1 1 14
        , stackTiles 5 1 1 15
        ]


arrangeStackTiles_6 : List Order
arrangeStackTiles_6 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 6 1 16 1
        , stackTiles 6 1 16 2
        , stackTiles 6 1 15 3
        , stackTiles 6 1 13 4
        , stackTiles 6 1 12 5
        , stackTiles 6 1 11 6
        , stackTiles 6 1 10 7
        , stackTiles 6 1 9 8
        , stackTiles 6 1 8 9
        , stackTiles 6 1 8 10
        , stackTiles 6 1 7 11
        , stackTiles 6 1 5 12
        , stackTiles 6 1 5 13
        , stackBuildings 6 1 4 14
        , stackBuildings 6 1 3 15
        , stackBuildings 6 1 3 16
        , stackBuildings 6 1 3 17
        , stackBuildings 6 1 1 18
        , stackBuildings 6 3 3 18
        ]


arrangeStackTiles_7 : List Order
arrangeStackTiles_7 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 7 1 16 1
        , stackTiles 7 1 16 2
        , stackTiles 7 1 16 3
        , stackTiles 7 1 16 4
        , stackTiles 7 1 15 5
        , stackTiles 7 1 14 6
        , stackTiles 7 1 14 7
        , stackTiles 7 1 13 8
        , stackTiles 7 1 13 9
        , stackTiles 7 1 13 10
        , stackTiles 7 1 13 11
        , stackTiles 7 1 5 12
        , stackTiles 7 1 5 13
        , stackBuildings 7 1 4 14
        , stackDoors 7 3 3 15
        , stackDoors 7 3 3 16
        , stackBuildings 7 3 3 17
        , stackBuildings 7 1 2 15
        , stackBuildings 7 1 2 16
        , stackBuildings 7 1 2 17
        , stackBuildings 7 1 3 18
        ]


arrangeStackTiles_8 : List Order
arrangeStackTiles_8 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 8 1 16 1
        , stackTiles 8 1 16 2
        , stackTiles 8 1 16 3
        , stackTiles 8 1 16 4
        , stackTiles 8 1 16 5
        , stackTiles 8 1 16 6
        , stackTiles 8 1 15 7
        , stackTiles 8 1 15 8
        , stackTiles 8 1 14 9
        , stackTiles 8 1 14 10
        , stackTiles 8 1 14 11
        , stackBuildings 8 11 13 12
        , stackBuildings 8 11 11 13
        , stackBuildings 8 11 11 14
        , stackBuildings 8 11 11 15
        , stackBuildings 8 11 11 16
        , stackTiles 8 1 10 12
        , stackTiles 8 1 5 13
        , stackTiles 8 8 10 13
        , stackBuildings 8 1 4 14
        , stackBuildings 8 1 3 15
        , stackBuildings 8 1 3 16
        , stackBuildings 8 1 3 17
        , stackBuildings 8 1 3 18
        ]


arrangeStackTiles_9 : List Order
arrangeStackTiles_9 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 9 1 16 1
        , stackTiles 9 1 16 2
        , stackTiles 9 1 16 3
        , stackTiles 9 1 16 4
        , stackTiles 9 1 16 5
        , stackTiles 9 1 16 6
        , stackTiles 9 1 16 7
        , stackTiles 9 1 15 8
        , stackTiles 9 1 15 9
        , stackTiles 9 1 14 10
        , stackTiles 9 1 14 11
        , stackTiles 9 1 11 12
        , stackTiles 9 1 11 13
        , stackTiles 9 8 11 14
        , stackTiles 9 5 5 14
        , stackTiles 9 1 3 14
        , stackTiles 9 1 1 16
        , stackTiles 9 1 1 17
        , stackTiles 9 1 1 18
        , stackTiles 9 1 1 19
        , stackBuildings 9 2 4 14
        , stackBuildings 9 2 4 15
        , stackBuildings 9 11 11 12
        , stackBuildings 9 11 11 13
        , stackBuildings 9 11 11 14
        , stackBuildings 9 11 11 15
        , stackBuildings 9 11 11 16
        , stackBuildings 9 12 12 12
        , stackBuildings 9 13 13 12
        , stackBuildings 9 12 12 13
        , stackBuildings 9 12 12 14
        , stackBuildings 9 12 12 15
        , stackBuildings 9 12 12 16
        , stackBuildings 9 12 12 17
        , stackBuildings 9 12 12 17
        , stackDoors 9 13 13 13
        , stackDoors 9 13 13 14
        , stackBuildings 9 13 13 15
        , stackBuildings 9 14 14 12
        , stackBuildings 9 14 14 13
        , stackBuildings 9 14 14 14
        , stackBuildings 9 14 14 15
        , stackBuildings 9 14 14 16
        , stackBuildings 9 13 13 16
        ]


arrangeStackTiles_10 : List Order
arrangeStackTiles_10 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 10 1 16 1
        , stackTiles 10 1 16 2
        , stackTiles 10 1 16 3
        , stackTiles 10 1 16 4
        , stackTiles 10 1 16 5
        , stackTiles 10 1 16 6
        , stackTiles 10 1 16 7
        , stackTiles 10 1 16 8
        , stackTiles 10 1 16 9
        , stackTiles 10 1 14 10
        , stackTiles 10 1 14 11
        , stackTiles 10 1 11 12
        , stackTiles 10 1 11 13
        , stackTiles 10 1 11 14
        , stackTiles 10 8 11 15
        , stackTiles 10 1 5 15
        , stackTiles 10 1 1 16
        , stackTiles 10 1 1 17
        , stackTiles 10 1 1 18
        , stackTiles 10 1 1 19
        , stackBuildings 10 11 11 16
        , stackBuildings 10 12 12 12
        , stackBuildings 10 12 12 13
        , stackBuildings 10 12 12 14
        , stackBuildings 10 12 12 15
        , stackBuildings 10 12 12 16
        , stackBuildings 10 12 12 17
        , stackBuildings 10 13 13 16
        , stackBuildings 10 13 13 17
        , stackBuildings 10 14 14 12
        , stackBuildings 10 14 14 13
        , stackBuildings 10 14 14 14
        , stackBuildings 10 14 14 15
        , stackBuildings 10 14 14 16
        , stackBuildings 10 14 14 17
        , stackBuildings 10 2 2 16
        , stackBuildings 10 2 2 17
        , stackBuildings 10 2 2 18
        , stackBuildings 10 2 2 19
        , stackDoors 10 3 3 16
        , stackDoors 10 3 3 17
        , stackBuildings 10 4 5 16
        , stackBuildings 10 4 4 17
        , stackBuildings 10 3 4 18
        ]


arrangeStackTiles_11 : List Order
arrangeStackTiles_11 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 11 1 16 1
        , stackTiles 11 1 16 2
        , stackTiles 11 1 16 3
        , stackTiles 11 1 16 4
        , stackTiles 11 1 16 5
        , stackTiles 11 1 16 6
        , stackTiles 11 1 16 7
        , stackTiles 11 1 16 8
        , stackTiles 11 1 16 9
        , stackTiles 11 1 16 10
        , stackTiles 11 1 14 11
        , stackTiles 11 1 10 12
        , stackTiles 11 1 10 13
        , stackTiles 11 1 10 14
        , stackTiles 11 1 10 15
        , stackBuildings 11 11 11 12
        , stackBuildings 11 11 11 13
        , stackBuildings 11 11 11 14
        , stackBuildings 11 11 11 15
        , stackBuildings 11 12 14 12
        , stackBuildings 11 12 14 13
        , stackBuildings 11 12 14 14
        , stackBuildings 11 12 14 15
        , stackBuildings 11 12 14 16
        , stackBuildings 11 12 14 17
        , stackTiles 11 1 1 16
        , stackTiles 11 1 1 17
        , stackTiles 11 1 1 18
        , stackTiles 11 1 1 19
        , stackTiles 11 5 5 16
        , stackTiles 11 5 5 17
        , stackBuildings 11 2 4 16
        , stackBuildings 11 2 4 17
        , stackBuildings 11 2 4 18
        , stackBuildings 11 2 4 19
        ]


arrangeStackTiles_12 : List Order
arrangeStackTiles_12 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 12 1 16 1
        , stackTiles 12 1 16 2
        , stackTiles 12 1 16 3
        , stackTiles 12 1 16 4
        , stackTiles 12 1 16 5
        , stackTiles 12 1 16 6
        , stackTiles 12 1 16 7
        , stackTiles 12 1 16 8
        , stackTiles 12 1 16 9
        , stackTiles 12 1 16 10
        , stackTiles 12 1 16 11
        , stackTiles 12 1 16 12
        , stackTiles 12 1 16 13
        , stackTiles 12 1 15 14
        , stackBuildings 12 15 16 14
        , stackTiles 12 1 14 15
        , stackTiles 12 1 13 16
        , stackTiles 12 1 5 17
        , stackTiles 12 1 1 16
        , stackTiles 12 1 1 17
        , stackTiles 12 1 1 18
        , stackTiles 12 1 1 19
        , stackTiles 12 1 1 20
        , stackTiles 12 1 1 21
        , stackTiles 12 1 1 22
        , stackBuildings 12 2 4 18
        , stackBuildings 12 2 4 19
        , stackBuildings 12 8 9 17
        , stackBuildings 12 8 9 18
        , stackBuildings 12 8 9 19
        ]


arrangeStackTiles_13 : List Order
arrangeStackTiles_13 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 13 1 16 1
        , stackTiles 13 1 16 2
        , stackTiles 13 1 16 3
        , stackTiles 13 1 16 4
        , stackTiles 13 1 16 5
        , stackTiles 13 1 16 6
        , stackTiles 13 1 16 7
        , stackTiles 13 1 16 8
        , stackTiles 13 1 16 9
        , stackTiles 13 1 16 10
        , stackTiles 13 1 16 11
        , stackTiles 13 1 16 12
        , stackTiles 13 1 16 13
        , stackTiles 13 1 13 14
        , stackTiles 13 1 13 15
        , stackTiles 13 1 13 16
        , stackBuildings 13 14 14 14
        , stackBuildings 13 14 14 15
        , stackBuildings 13 14 14 16
        , stackBuildings 13 14 14 17
        , stackDoors 13 15 15 14
        , stackDoors 13 15 15 15
        , stackDoors 13 15 15 16
        , stackBuildings 13 15 15 17
        , stackBuildings 13 16 16 14
        , stackBuildings 13 16 16 15
        , stackBuildings 13 16 16 16
        , stackBuildings 13 16 16 17
        , stackBuildings 13 14 16 18
        , stackTiles 13 1 7 17
        , stackBuildings 13 8 9 17
        , stackBuildings 13 8 9 18
        , stackBuildings 13 8 9 19
        , stackBuildings 13 8 9 20
        , stackDoors 13 10 10 17
        , stackDoors 13 10 10 18
        , stackBuildings 13 10 10 19
        , stackBuildings 13 10 10 20
        , stackBuildings 13 11 11 17
        , stackBuildings 13 11 11 18
        , stackBuildings 13 11 11 19
        , stackBuildings 13 11 11 20
        , stackTiles 13 1 5 18
        , stackTiles 13 1 5 19
        , stackTiles 13 1 5 20
        , stackTiles 13 1 1 21
        , stackTiles 13 1 1 22
        , stackBuildings 13 2 3 21
        , stackBuildings 13 2 3 22
        , stackBuildings 13 2 3 23
        ]


arrangeStackTiles_14 : List Order
arrangeStackTiles_14 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 14 1 13 1
        , stackTiles 14 1 13 2
        , stackTiles 14 1 13 3
        , stackTiles 14 1 13 4
        , stackTiles 14 1 13 5
        , stackTiles 14 1 13 6
        , stackTiles 14 1 13 7
        , stackTiles 14 1 13 8
        , stackTiles 14 1 13 9
        , stackTiles 14 1 13 10
        , stackTiles 14 1 13 11
        , stackTiles 14 1 13 12
        , stackTiles 14 1 13 13
        , stackTiles 14 1 13 14
        , stackTiles 14 1 13 15
        , stackTiles 14 1 13 16
        , stackTiles 14 14 16 1
        , stackTiles 14 14 16 2
        , stackTiles 14 14 16 3
        , stackTiles 14 14 16 4
        , stackTiles 14 14 16 5
        , stackTiles 14 14 16 6
        , stackTiles 14 14 16 7
        , stackTiles 14 14 16 8
        , stackTiles 14 14 16 9
        , stackTiles 14 14 16 10
        , stackTiles 14 14 16 11
        , stackTiles 14 14 16 12
        , stackTiles 14 14 16 13
        , stackTiles 14 1 7 17
        , stackTiles 14 1 7 18
        , stackTiles 14 1 5 19
        , stackTiles 14 1 5 20
        , stackTiles 14 1 1 21
        , stackTiles 14 1 1 22
        , stackBuildings 14 2 4 21
        , stackBuildings 14 2 4 22
        , stackBuildings 14 2 4 23
        , stackBuildings 14 2 4 24
        , stackBuildings 14 14 16 14
        , stackBuildings 14 14 16 15
        , stackBuildings 14 14 16 16
        , stackBuildings 14 14 16 17
        , stackBuildings 14 15 16 18
        , stackBuildings 14 8 11 17
        , stackBuildings 14 8 11 18
        , stackBuildings 14 8 11 19
        , stackBuildings 14 8 11 20
        ]


arrangeStackTiles_15 : List Order
arrangeStackTiles_15 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 15 1 16 1
        , stackTiles 15 1 16 2
        , stackTiles 15 1 16 3
        , stackTiles 15 1 16 4
        , stackTiles 15 1 16 5
        , stackTiles 15 1 16 6
        , stackTiles 15 1 16 7
        , stackTiles 15 1 16 8
        , stackTiles 15 1 16 9
        , stackTiles 15 1 16 10
        , stackTiles 15 1 16 11
        , stackTiles 15 1 16 12
        , stackTiles 15 1 16 13
        , stackTiles 15 1 13 14
        , stackTiles 15 1 13 15
        , stackTiles 15 1 13 16
        , stackTiles 15 1 13 17
        , stackBuildings 15 14 16 14
        , stackBuildings 15 14 16 15
        , stackBuildings 15 14 16 16
        , stackBuildings 15 14 16 17
        , stackBuildings 15 15 16 18
        , stackTiles 15 1 14 18
        , stackTiles 15 1 14 19
        , stackTiles 15 1 5 20
        , stackTiles 15 1 1 21
        , stackTiles 15 1 1 22
        , stackTiles 15 1 1 23
        , stackTiles 15 1 1 24
        , stackTiles 15 1 1 25
        , stackDoors 15 4 4 21
        , stackDoors 15 4 4 22
        , stackBuildings 15 4 4 23
        , stackBuildings 15 2 3 21
        , stackBuildings 15 2 3 22
        , stackBuildings 15 2 3 23
        , stackBuildings 15 2 4 24
        , stackTiles 15 6 6 20
        , stackTiles 15 8 13 20
        , stackTiles 15 8 12 21
        , stackBuildings 15 8 11 22
        ]


arrangeStackTiles_16 : List Order
arrangeStackTiles_16 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 16 1 16 1
        , stackTiles 16 1 16 2
        , stackTiles 16 1 16 3
        , stackTiles 16 1 16 4
        , stackTiles 16 1 16 5
        , stackTiles 16 1 16 6
        , stackTiles 16 1 16 7
        , stackTiles 16 1 16 8
        , stackTiles 16 1 16 9
        , stackTiles 16 1 16 10
        , stackTiles 16 1 16 11
        , stackTiles 16 1 16 12
        , stackTiles 16 1 16 13
        , stackTiles 16 1 16 14
        , stackTiles 16 1 16 15
        , stackTiles 16 1 16 16
        , stackTiles 16 1 16 17
        , stackTiles 16 1 16 18
        , stackTiles 16 1 16 19
        , stackTiles 16 1 12 20
        , stackTiles 16 8 12 21
        , stackTiles 16 1 5 20
        , stackTiles 16 1 5 21
        , stackTiles 16 1 5 22
        , stackTiles 16 1 5 23
        , stackTiles 16 1 4 24
        , stackTiles 16 1 4 25
        , stackBuildings 16 8 11 22
        , stackBuildings 16 8 8 23
        , stackBuildings 16 8 8 24
        , stackBuildings 16 8 8 25
        , stackDoors 16 9 9 23
        , stackDoors 16 9 9 24
        , stackBuildings 16 9 9 25
        , stackBuildings 16 10 10 23
        , stackBuildings 16 10 10 24
        , stackBuildings 16 10 10 25
        , stackBuildings 16 8 9 26
        , stackBuildings 16 13 15 20
        , stackBuildings 16 13 15 21
        , stackBuildings 16 13 15 22
        , stackBuildings 16 13 15 23
        , stackBuildings 16 13 13 24
        ]


arrangeStackTiles_17 : List Order
arrangeStackTiles_17 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 17 1 16 1
        , stackTiles 17 1 16 2
        , stackTiles 17 1 16 3
        , stackTiles 17 1 16 4
        , stackTiles 17 1 16 5
        , stackTiles 17 1 16 6
        , stackTiles 17 1 16 7
        , stackTiles 17 1 16 8
        , stackTiles 17 1 16 9
        , stackTiles 17 1 16 10
        , stackTiles 17 1 16 11
        , stackTiles 17 1 16 12
        , stackTiles 17 1 16 13
        , stackTiles 17 1 16 14
        , stackTiles 17 1 16 15
        , stackTiles 17 1 16 16
        , stackTiles 17 1 16 17
        , stackTiles 17 1 16 18
        , stackTiles 17 1 16 19
        , stackTiles 17 1 12 20
        , stackTiles 17 1 12 21
        , stackBuildings 17 2 5 22
        , stackBuildings 17 2 5 23
        , stackBuildings 17 2 5 24
        , stackBuildings 17 2 5 25
        , stackBuildings 17 2 4 26
        , stackTiles 17 1 1 22
        , stackTiles 17 1 1 23
        , stackTiles 17 1 1 24
        , stackTiles 17 1 1 25
        , stackDoors 17 15 15 20
        , stackDoors 17 15 15 21
        , stackBuildings 17 15 15 22
        , stackBuildings 17 13 15 23
        , stackBuildings 17 13 15 24
        , stackBuildings 17 8 11 22
        , stackBuildings 17 8 11 23
        , stackBuildings 17 8 11 23
        , stackBuildings 17 8 11 24
        , stackBuildings 17 8 11 25
        , stackBuildings 17 8 11 26
        ]


arrangeStackTiles_18 : List Order
arrangeStackTiles_18 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 18 1 16 1
        , stackTiles 18 1 16 2
        , stackTiles 18 1 16 3
        , stackTiles 18 1 16 4
        , stackTiles 18 1 16 5
        , stackTiles 18 1 16 6
        , stackTiles 18 1 16 7
        , stackTiles 18 1 16 8
        , stackTiles 18 1 16 9
        , stackTiles 18 1 16 10
        , stackTiles 18 1 16 11
        , stackTiles 18 1 16 12
        , stackTiles 18 1 16 13
        , stackTiles 18 1 16 14
        , stackTiles 18 1 16 15
        , stackTiles 18 1 16 16
        , stackTiles 18 1 16 17
        , stackTiles 18 1 16 18
        , stackTiles 18 1 16 19
        , stackTiles 18 1 12 20
        , stackTiles 18 1 12 21
        , stackTiles 18 1 12 22
        , stackBuildings 18 2 5 23
        , stackBuildings 18 2 5 24
        , stackBuildings 18 2 5 25
        , stackBuildings 18 2 4 26
        , stackTiles 18 1 1 22
        , stackTiles 18 1 1 23
        , stackTiles 18 1 1 24
        , stackTiles 18 1 1 25
        , stackBuildings 18 13 15 20
        , stackBuildings 18 13 15 21
        , stackBuildings 18 13 15 22
        , stackBuildings 18 13 15 23
        , stackBuildings 18 13 15 24
        , stackTiles 18 8 13 23
        , stackTiles 18 8 13 24
        , stackTiles 18 8 13 25
        ]


arrangeStackTiles_19 : List Order
arrangeStackTiles_19 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 19 1 16 1
        , stackTiles 19 1 16 2
        , stackTiles 19 1 16 3
        , stackTiles 19 1 16 4
        , stackTiles 19 1 16 5
        , stackTiles 19 1 16 6
        , stackTiles 19 1 16 7
        , stackTiles 19 1 16 8
        , stackTiles 19 1 16 9
        , stackTiles 19 1 16 10
        , stackTiles 19 1 16 11
        , stackTiles 19 1 16 12
        , stackTiles 19 1 16 13
        , stackTiles 19 1 16 14
        , stackTiles 19 1 16 15
        , stackTiles 19 1 16 16
        , stackTiles 19 1 16 17
        , stackTiles 19 1 16 18
        , stackTiles 19 1 16 19
        , stackTiles 19 1 16 20
        , stackTiles 19 1 16 21
        , stackTiles 19 1 16 22
        , stackTiles 19 1 16 23
        , stackBuildings 19 2 5 24
        , stackBuildings 19 2 5 25
        , stackBuildings 19 2 5 26
        , stackTiles 19 8 16 24
        , stackTiles 19 8 16 25
        , stackBuildings 19 9 9 25
        , stackTiles 19 1 1 24
        , stackTiles 19 1 1 25
        ]


arrangeStackTiles_20 : List Order
arrangeStackTiles_20 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 20 1 16 1
        , stackTiles 20 1 16 2
        , stackTiles 20 1 16 3
        , stackTiles 20 1 16 4
        , stackTiles 20 1 16 5
        , stackTiles 20 1 16 6
        , stackTiles 20 1 16 7
        , stackTiles 20 1 16 8
        , stackTiles 20 1 16 9
        , stackTiles 20 1 16 10
        , stackTiles 20 1 16 11
        , stackTiles 20 1 16 12
        , stackTiles 20 1 16 13
        , stackTiles 20 1 16 14
        , stackTiles 20 1 16 15
        , stackTiles 20 1 16 16
        , stackTiles 20 1 16 17
        , stackTiles 20 1 16 18
        , stackTiles 20 1 16 19
        , stackTiles 20 1 16 20
        , stackTiles 20 1 16 21
        , stackTiles 20 1 16 22
        , stackTiles 20 1 16 23
        , stackTiles 20 1 16 24
        , stackTiles 20 1 5 25
        , stackTiles 20 8 16 25
        , stackBuildings 20 8 8 26
        , stackBuildings 20 8 8 27
        , stackBuildings 20 8 8 28
        , stackDoors 20 9 9 26
        , stackDoors 20 9 9 27
        , stackBuildings 20 9 9 28
        , stackBuildings 20 10 10 26
        , stackBuildings 20 10 10 27
        , stackBuildings 20 10 10 28
        , stackBuildings 20 8 10 29
        , stackBuildings 20 14 15 26
        , stackBuildings 20 14 15 27
        , stackBuildings 20 14 15 28
        ]


arrangeStackTiles_21 : List Order
arrangeStackTiles_21 =
    -- stackTiles 列名 始点 終点 高さ
    List.concat <|
        [ stackTiles 21 1 16 1
        , stackTiles 21 1 16 2
        , stackTiles 21 1 16 3
        , stackTiles 21 1 16 4
        , stackTiles 21 1 16 5
        , stackTiles 21 1 16 6
        , stackTiles 21 1 16 7
        , stackTiles 21 1 16 8
        , stackTiles 21 1 16 9
        , stackTiles 21 1 16 10
        , stackTiles 21 1 16 11
        , stackTiles 21 1 16 12
        , stackTiles 21 1 16 13
        , stackTiles 21 1 16 14
        , stackTiles 21 1 16 15
        , stackTiles 21 1 16 16
        , stackTiles 21 1 16 17
        , stackTiles 21 1 16 18
        , stackTiles 21 1 16 19
        , stackTiles 21 1 16 20
        , stackTiles 21 1 16 21
        , stackTiles 21 1 16 22
        , stackTiles 21 1 16 23
        , stackTiles 21 1 16 24
        , stackTiles 21 1 16 25
        , stackBuildings 21 8 11 26
        , stackBuildings 21 8 11 27
        , stackBuildings 21 8 11 28
        , stackBuildings 21 8 11 29
        , stackBuildings 21 14 16 26
        , stackBuildings 21 14 16 27
        , stackBuildings 21 14 16 28
        ]


stackDoors : Int -> Int -> Int -> Int -> List Order
stackDoors xindex start end stackNum =
    case xindex of
        0 ->
            [ Order end <| stackTileDoor 0 end stackNum ]

        _ ->
            case end of
                0 ->
                    []

                _ ->
                    if end >= start then
                        List.foldr (::)
                            [ Order (-xindex + end) <|
                                stackTileDoor (xindex - 1) end stackNum
                            ]
                            (stackDoors xindex start (end - 1) stackNum)

                    else
                        []


stackTileDoor : Int -> Int -> Int -> Html Msg
stackTileDoor n i stackNum =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileDoor n (boardHeight - 1) stackNum

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileDoor n i stackNum


innerTileDoor : Int -> Int -> Int -> List (Svg msg)
innerTileDoor n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 121 67 15)
                , stroke <| Color.rgb255 55 60 56
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 121 67 15)
                , stroke <| Color.rgb255 55 60 56
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 121 67 15)
                , stroke <| Color.rgb255 55 60 56
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


stackTileBuilding : Int -> Int -> Int -> Html Msg
stackTileBuilding n i stackNum =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileBuilding n (boardHeight - 1) stackNum

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileBuilding n i stackNum


innerTileBuilding : Int -> Int -> Int -> List (Svg msg)
innerTileBuilding n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 120 120 120)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 204 195 161)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 204 195 161)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


stackTileLand : Int -> Int -> Int -> Html Msg
stackTileLand n i stackNum =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileLand n (boardHeight - 1) stackNum

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileLand n i stackNum


innerTileLand : Int -> Int -> Int -> List (Svg msg)
innerTileLand n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 65 70 66)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 165 112 39)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 66 96 45)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]


stackBuildings : Int -> Int -> Int -> Int -> List Order
stackBuildings xindex start end stackNum =
    case xindex of
        0 ->
            [ Order end <| stackTileBuilding 0 end stackNum ]

        _ ->
            case end of
                0 ->
                    []

                _ ->
                    if end >= start then
                        List.foldr (::)
                            [ Order (-xindex + end) <|
                                stackTileBuilding (xindex - 1) end stackNum
                            ]
                            (stackBuildings xindex start (end - 1) stackNum)

                    else
                        []


stackTiles : Int -> Int -> Int -> Int -> List Order
stackTiles xindex start end stackNum =
    case xindex of
        0 ->
            [ Order end <| stackTileLand 0 end stackNum ]

        _ ->
            case end of
                0 ->
                    []

                _ ->
                    if end >= start then
                        List.foldr (::)
                            [ Order (-xindex + end) <|
                                stackTileLand (xindex - 1) end stackNum
                            ]
                            (stackTiles xindex start (end - 1) stackNum)

                    else
                        []


drawQuarterObjects : Int -> Int -> Html Msg
drawQuarterObjects i j =
    svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
        List.foldr (::)
            []
            (orderTile i (j + 1) |> List.sortBy .index |> List.map .element)



---- MODEL ----


view : Model -> Html Msg
view model =
    if maxElement <= 5 then
        div [] [ h1 [] [ Html.text "maxElement number is too small. Please input more big number." ] ]

    else
        div []
            [ chapterTitle
            , drawQuarterObjects model.columnCount model.rowCount
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



{- リストをSelectList化（maxElementは原点を含む要素の最大数） -}


createSelectList : Int -> SelectList.SelectList (Point String)
createSelectList n =
    SelectList.fromLists []
        (Point 0 0 (Array.fromList [ "nothing" ]))
        -- 所望の順序と逆で出力されてしまうため反転させる
        (createList n maxElement |> List.reverse)
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
            getPointYList maxElement |> SelectList.selectHead
    in
    if columnNum == (head |> SelectList.index) then
        []

    else
        ( orderNum
        , getPointYList maxElement
            |> SelectList.attempt (SelectList.selectBy columnNum)
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
        , points <| outputColumn (maxElement + 1) (i * tileWidth |> toFloat)
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
        , points <| outputRow (maxElement + 1) (i * tileHeight |> toFloat)
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
        List.foldr (::) (drawRowList maxElement) (drawColumnList maxElement)



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


outputQuarterList : Int -> Int -> SelectList.SelectList ( Float, Float )
outputQuarterList n stackNum =
    createXYList n
        |> List.map (\( x, y ) -> ( quarterX x y, quarterY x y ))
        |> List.map (\( x, y ) -> ( x, y - (stackNum |> toFloat) * h / 4 ))
        |> SelectList.fromList
        |> Maybe.withDefault (SelectList.singleton ( 0.0, 0.0 ))



{- createTile:タイルチップを座標平面上に配置するための関数 -}


createTile : Int -> Int -> Int -> List (Svg msg)
createTile n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.second
    in
    case count of
        0 ->
            []

        _ ->
            [ polygon
                [ SvgAt.fill (Fill <| Color.rgb255 65 70 66)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (leftSide |> List.map (\( c, d ) -> ( c + x, d + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 165 112 39)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (rightSide |> List.map (\( e, f ) -> ( e + x, f + y )))
                ]
                []
            , polygon
                [ SvgAt.fill (Fill <| Color.rgb255 0 92 175)
                , stroke <| Color.rgb255 65 70 66
                , strokeLinejoin StrokeLinejoinRound
                , strokeWidth (px 1.0)
                , strokeOpacity (Opacity <| 1.0)
                , points <| (top |> List.map (\( a, b ) -> ( a + x, b + y )))
                ]
                []
            ]



{- 再帰を使ってcreateTileをリスト化 -}


createTileList : Int -> Int -> Int -> List (Svg msg)
createTileList n i stackNum =
    case n of
        0 ->
            []

        _ ->
            List.foldr (::) (createTile (n - 1) i stackNum) (createTileList (n - 1) i stackNum)


drawBaseTile : Int -> Int -> Int -> List (Svg msg)
drawBaseTile i j stackNum =
    case i of
        0 ->
            List.foldr (::)
                (createTileList 0 j stackNum
                    |> List.reverse
                )
            <|
                drawBaseTile 0 (j - 1) stackNum

        _ ->
            case j of
                0 ->
                    []

                _ ->
                    List.foldr (::)
                        (createTileList (i - 1) (j - 1) stackNum
                            |> List.reverse
                        )
                    <|
                        drawBaseTile i (j - 1) stackNum


drawQuarterBoard : Int -> Int -> Int -> Html Msg
drawQuarterBoard i j stackNum =
    case ( i, j ) of
        ( 0, 0 ) ->
            svg [] []

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                List.foldr (::) [] (drawBaseTile i j stackNum)



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


stackTileRed : Int -> Int -> Int -> Html Msg
stackTileRed n i stackNum =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileRed n (boardHeight - 1) stackNum

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileRed n i stackNum


stackTileBlue : Int -> Int -> Int -> Html Msg
stackTileBlue n i stackNum =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileRed n (boardHeight - 1) stackNum

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileBlue n i stackNum


stackTileYellow : Int -> Int -> Int -> Html Msg
stackTileYellow n i stackNum =
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
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileRed n (boardHeight - 1) stackNum

        _ ->
            svg [ SvgAt.width (px 1000), SvgAt.height (px 1000), viewBox 0 0 1000 1000 ] <|
                innerTileYellow n i stackNum


stackTileGreen : Int -> Int -> Int -> Html Msg
stackTileGreen n i stackNum =
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
                innerTileGreen n i stackNum



{- 上記の関数の中身が煩雑になることを防ぐため、内部関数を用意 -}


innerTileRed : Int -> Int -> Int -> List (Svg msg)
innerTileRed n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
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


innerTileBlue : Int -> Int -> Int -> List (Svg msg)
innerTileBlue n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
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


innerTileYellow : Int -> Int -> Int -> List (Svg msg)
innerTileYellow n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
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


innerTileGreen : Int -> Int -> Int -> List (Svg msg)
innerTileGreen n i stackNum =
    let
        count =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.index

        x =
            outputQuarterList n stackNum
                |> SelectList.selectWhileLoopBy i
                |> SelectList.selected
                |> Tuple.first

        y =
            outputQuarterList n stackNum
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



-- 以下、タイルチップの実装
{- h:タイルチップの高さ(pixel単位)
   w:タイルチップの幅(pixel単位)
   offsetX:原点をX方向に指定値だけ移動させる
   offsetY:原点をY方向に指定値だけ移動させる
-}


h : Float
h =
    48


w : Float
w =
    44


offsetX : Float
offsetX =
    0


offsetY : Float
offsetY =
    600



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
    [ ( 0, h / 2 ), ( w / 2, h / 4 ), ( w, h / 2 ), ( w / 2, 3 * h / 4 ) ]


leftSide : List ( Float, Float )
leftSide =
    [ ( 0, h / 2 ), ( w / 2, 3 * h / 4 ), ( w / 2, h ), ( 0, 3 * h / 4 ) ]


rightSide : List ( Float, Float )
rightSide =
    [ ( w / 2, 3 * h / 4 ), ( w, h / 2 ), ( w, 3 * h / 4 ), ( w / 2, h ) ]



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
