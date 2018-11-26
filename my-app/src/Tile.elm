module Tile exposing (Array2D, fill, foldl, get, h, indexedMap, leftSide, map, offsetX, offsetY, quarterX, quarterY, rightSide, set, toList, top, w)

import Array exposing (..)
import Maybe exposing (..)



-- Make Tile


type alias Array2D a =
    Array (Array a)


fill : Int -> Int -> a -> Array2D a
fill rowLength colLength data =
    Array.repeat rowLength (Array.repeat colLength data)


get : Int -> Int -> Array2D a -> Maybe a
get rowIndex colIndex rows =
    Array.get rowIndex rows
        |> andThen
            (\row ->
                Array.get colIndex row
            )


set : Int -> Int -> a -> Array2D a -> Array2D a
set rowIndex colIndex data rows =
    case Array.get rowIndex rows of
        Just row ->
            Array.set rowIndex (Array.set colIndex data row) rows

        Nothing ->
            rows


map : (a -> b) -> Array2D a -> Array2D b
map f rows =
    Array.map (Array.map f) rows


indexedMap : (Int -> Int -> a -> b) -> Array2D a -> Array2D b
indexedMap f rows =
    Array.indexedMap (Array.indexedMap << f) rows


foldl : (a -> b -> b) -> b -> Array2D a -> b
foldl f init rows =
    Array.foldl (\row memo -> Array.foldl f memo row) init rows


toList : Array2D a -> List (List a)
toList rows =
    Array.toList (Array.map Array.toList rows)


h =
    32 


w =
    32


offsetX =
    100


offsetY =
    100


quarterX : Float -> Float -> Float
quarterX x y =
    offsetX + 0.5 * w * (x + y)


quarterY : Float -> Float -> Float
quarterY x y =
    offsetY + 0.25 * h * (-x + y)


top : List ( Float, Float )
top =
    [ ( 0, h/4 ), ( w/2, 0 ), ( w, h/4 ), ( w/2, h/2 ) ]


leftSide : List ( Float, Float )
leftSide =
    [ ( 0, h/4 ), ( w/2, h/2 ), ( w/2, h ), ( 0, 3*h/4 ) ]


rightSide : List ( Float, Float )
rightSide =
    [ ( w/2, h/2 ), ( w, h/4 ), ( w, 3*h/4 ), ( w/2, h ) ]
