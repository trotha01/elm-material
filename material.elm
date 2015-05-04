module Material where

import Graphics.Element exposing
  (Element, middle, container, flow, right, down, color, spacer, opacity, midLeft, image, centered)
import Graphics.Input exposing (clickable)
import Color exposing (green, white, black)
import Text exposing (Style, fromString, defaultStyle, style)
import Signal

import Debug

-- MODEL

-- The Page string is for debugging
type alias Page = { content:Element, title:String }
type alias Pages = List Page

type Action
    = OpenNavDrawer
    | CloseNavDrawer (Maybe Element) -- Might change page when nav bar closes

-- VIEW

hamburger : Element
hamburger = image 60 60 "hamburger.svg"

drawerOption : Signal.Address Action -> Page -> Element
drawerOption address page =
      container 340 100 middle page.content
          |> clickable (Signal.message address (CloseNavDrawer (Just page.content)))

-- NavigationDrawer comes in from the left
-- Includes fading the rest of the screen              
-- width and height should be Window.Dimensions
-- noOpMessage is when someone exits the navbar without selecting anything
navigationDrawer : Int -> Int -> Pages -> Signal.Address Action -> Element
navigationDrawer width height pages address =
  flow right
  [ flow down
    (List.map (drawerOption address) pages)
    |> color white
  , spacer (width) height
      |> color black
      |> opacity (0.5)
      |> clickable (Signal.message address (CloseNavDrawer Nothing))
  ]

-- toolbar takes in a width (of the screen) and a title string
toolbar : (Int, Int) -> String -> Signal.Message -> Element
toolbar (width, height) string message =
  container width height midLeft
  (flow right
    [ spacer 60 1
    , hamburger
        |> clickable message
    , spacer 180 1
    , title string
    ]
  )
  |> color green

headerStyle : Style
headerStyle = { defaultStyle | height <- Just 50 }

title : String -> Element
title string = centered
  (fromString string
    |> style headerStyle)
