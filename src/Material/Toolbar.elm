module Material.Toolbar where

import Graphics.Element exposing (Element, image, color, container, midLeft, flow, right, spacer, leftAligned)
import Graphics.Input exposing (clickable)
import Text exposing (Style, style, defaultStyle)
import Color exposing (green)
import Svg exposing (Svg)
import Html
import Material.Icons.Navigation exposing (menu)

-- MODEL
type Action
    = OpenNavDrawer

-- VIEW

-- Toolbar Icon

toolbarIconSize : Int
toolbarIconSize = 24

hamburger : Svg
hamburger = menu Color.white 30

-- Large Toolbar Sizing

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


titleFromString : String -> Element
titleFromString string = leftAligned
  (Text.fromString string
    |> style headerStyle)

{-|
  toolbar takes in the width of the screen and a title string
-}
toolbar : Int -> String -> Element
toolbar width title =
  container width lToolbarHeight midLeft
  (flow right
    [
      spacer lToolbarMarginLeft 1,
      Html.toElement 30 30 hamburger
        |> clickable (Signal.message toolbarMailbox.address OpenNavDrawer),
      spacer lTitleMarginLeft 1,
      titleFromString title
    ]
  )
  |> color green

-- SIGNALS

toolbarMailbox : Signal.Mailbox Action
toolbarMailbox =
  Signal.mailbox (OpenNavDrawer)

