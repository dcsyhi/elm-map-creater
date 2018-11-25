module Tile exposing (Array2D, bottom, fill, foldl, get, h, indexedMap, map, offsetX, offsetY, quarterX, quarterY, set, toList, top, w)

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


top =
    fill 4 3 0
        |> set 1 1 1
        |> set 2 0 1
        |> set 3 0 1
        |> set 3 1 1


bottom =
    fill 4 3 1
        |> set 0 0 0
        |> set 0 1 0
        |> set 1 0 0
        |> set 2 1 0


h =
    32


w =
    32


offsetX =
    200


offsetY =
    200


quarterX : Float -> Float -> Float
quarterX x y =
    offsetX + 0.5*w*(x + y)

quarterY : Float -> Float -> Float
quarterY  x y =
    offsetY + 0.25*h*(-x + y)
