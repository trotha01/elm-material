module Material.Toolbar where

import Graphics.Element exposing (Element, image, color, container, midLeft, flow, right, leftAligned)
import Graphics.Input exposing (clickable)
import Text exposing (Style, defaultStyle)
import Color exposing (green)
import Svg exposing (Svg)
import List exposing (map)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (width, height, size, style)
import Material.Icons.Navigation exposing (menu)
import Flex

-- MODEL
type Action
    = OpenNavDrawer

-- VIEW

-- Toolbar Icon

toolbarIconSize : Int
toolbarIconSize = 24

hamburger : Svg
hamburger = menu Color.white 30

menuOption : Html
menuOption = Html.div
    [ onClick toolbarMailbox.address OpenNavDrawer
    , style [("cursor","pointer")]
    ]
    [ hamburger ]

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

titleSize : String
titleSize = "50px"

titleFromString : String -> Html
titleFromString string = 
  Html.div [ style [( "font-size", titleSize )] ] [Html.text string]

spacer : Int -> Int -> Html
spacer w h =
  Html.div [ width w, height h ] []

{-|
  toolbar takes in the width of the screen and a title string
--}
{--}
toolbar : Int -> String -> Element
toolbar width title =
  container width lToolbarHeight midLeft
  (flow right
    [
      Html.toElement lToolbarMarginLeft 1 (spacer lToolbarMarginLeft 1),
      Html.toElement 30 30 menuOption,
      Html.toElement lTitleMarginLeft 1 (spacer lTitleMarginLeft 1),
      Html.toElement 600 50 (titleFromString title)
    ]
  )
  |> color green
  --}

{--
toolbar : Int -> String -> Html
toolbar width title =
  Html.div [ style [ ("backgroundColor", "green" )] ]
  [
      (spacer lToolbarMarginLeft 1),
      menuOption,
      (spacer lTitleMarginLeft 1),
      (titleFromString title)
  ]
--}

hflow : List Html -> List Html
hflow nodes =
  map floatify nodes

floatify : Html -> Html
floatify node =
  Html.div [ style [("float", "left")] ] [
    node
  ]

{--
toolbar : Int -> String -> Html
toolbar width title =
  Html.div [ style [ ("backgroundColor", "green" )] ]
  [
    Flex.row
    [
      (spacer lToolbarMarginLeft 1),
      menuOption,
      (spacer lTitleMarginLeft 1),
      (titleFromString title)
    ]
  ]
--}

-- SIGNALS

toolbarMailbox : Signal.Mailbox Action
toolbarMailbox =
  Signal.mailbox (OpenNavDrawer)

