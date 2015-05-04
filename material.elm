module Material where

import Graphics.Element exposing
  (Element, middle, container, flow, right, down, color, spacer, opacity, midLeft, image, centered, leftAligned)
import Graphics.Input exposing (clickable)
import Color exposing (green, white, black)
import Text exposing (Style, fromString, defaultStyle, style)
import Signal

-- MODEL

type alias Page = { content:Element, title:String }
type alias Pages = List Page

type Action
    = OpenNavDrawer
    | CloseNavDrawer (Maybe Page) -- Might change page when nav bar closes

-- VIEW

drawerOption : Signal.Address Action -> Page -> Element
drawerOption address page =
      container 340 100 middle (centered (fromString page.title))
          |> clickable (Signal.message address (CloseNavDrawer (Just page)))

drawerOptions : Signal.Address Action -> Pages -> Element
drawerOptions address pages =
  flow down
    (List.map (drawerOption address) pages)
    |> color white

scrim : (Int, Int) -> Element
scrim (width, height) = 
    spacer (width) height
      |> color black
      |> opacity (0.5)

-- NavigationDrawer comes in from the left
-- Includes fading the rest of the screen
-- width and height should be Window.Dimensions
-- noOpMessage is when someone exits the navbar without selecting anything
navigationDrawer : (Int, Int) -> Pages -> Signal.Address Action -> Element
navigationDrawer (width, height) pages address =
  flow right
  [ drawerOptions address pages
  , scrim (width, height)
      |> clickable (Signal.message address (CloseNavDrawer Nothing))
  ]


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


-- toolbar takes in a width (of the screen) and a title string
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

headerStyle : Style
headerStyle = { defaultStyle | height <- Just 50 }

title : String -> Element
title string = leftAligned
  (fromString string
    |> style headerStyle)
