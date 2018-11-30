module Tile exposing (h, leftSide, offsetX, offsetY, quarterX, quarterY, rightSide, top, w)

import Array exposing (..)
import Browser
import Color exposing (Color)
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, src)
import Maybe exposing (..)
import SelectList
import TypedSvg exposing (..)
import TypedSvg.Attributes as SvgAt exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)



-- Make Tile




h =
    44


w =
    44


offsetX =
      -50


offsetY =
    150


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
        
        
