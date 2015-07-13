module Toolbar where

import Graphics.Element exposing (Element, image, color, container, midLeft, flow, right, spacer, leftAligned)
import Graphics.Input exposing (clickable)
import Text exposing (Style, style, defaultStyle)
import Color exposing (green)

-- TOOLBAR

toolbarIconSize : Int
toolbarIconSize = 24

hamburger : Element
hamburger = image toolbarIconSize toolbarIconSize "hamburger.svg"

-- Large Toolbar

lToolbarHeight : Int
lToolbarHeight = 128

lToolbarMarginLeft : Int
lToolbarMarginLeft = 16

lTitleMarginLeft : Int
lTitleMarginLeft = 72 - lToolbarMarginLeft - toolbarIconSize

lTitleMarginBottom : Int
lTitleMarginBottom = 16

lIconRowHeight : Int
lIconRowHeight = 56

lTitleSize : Int
lTitleSize = 112

headerStyle : Style
headerStyle = { defaultStyle | height <- Just 50 }


title : String -> Element
title string = leftAligned
  (Text.fromString string
    |> style headerStyle)


{-|
  toolbar takes in a width (of the screen) and a title string
--}
toolbar : (Int, Int) -> String -> Signal.Message -> Element
toolbar (width, height) string message =
  container width lToolbarHeight midLeft
  (flow right
    [ spacer lToolbarMarginLeft 1
    , hamburger
        |> clickable message
    , spacer lTitleMarginLeft 1
    , title string
    ]
  )
  |> color green
