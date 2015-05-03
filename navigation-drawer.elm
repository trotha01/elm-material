import Window
import Color exposing (green, white, black)
import Graphics.Element exposing (Element)
import Graphics.Element as G -- exposing (Element, centered, container, color, flow, right, spacer)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)
import Debug

main : Signal Element
main = click.signal
  |> Signal.map2 view Window.dimensions

-- MODEL
type State
    = MainView
    | NavBar

-- INPUTS
click : Signal.Mailbox State
click =
  Signal.mailbox MainView

-- DISPLAY

title : String -> Element
title string = G.centered (fromString string)

hamburger : Element
hamburger = G.image 60 60 "hamburger.svg"

-- clickable : Signal.Message -> Element -> Element
-- button : Signal.Message -> String -> Element

-- toolbar takes in a width (of the screen) and a title string
toolbar : Int -> String -> Element
toolbar width string =
  G.container width 180 G.midLeft
  (G.flow G.right
    [ G.spacer 60 1
    , hamburger
        |> clickable (Signal.message click.address NavBar)
    , G.spacer 180 1
    , title string
    ]
  )
  |> G.color green

-- NavigationDrawer comes in from the left
navigationDrawer width height =
  G.flow G.right
  [ G.flow G.down
      [ G.container 340 100 G.middle (title "Hello World")
      , G.container 340 100 G.middle (title "More Stuff!")
      ]
    |> G.color white
  , G.spacer (width) height
      |> G.color black
      |> G.opacity (0.5)
      |> clickable (Signal.message click.address MainView)
  ]

view : (Int, Int) -> State -> Element
view (w, h) state =
    case state of
        MainView -> toolbar w "Title"
        NavBar -> G.layers
          [ toolbar w "Title"
          , navigationDrawer w h
          ]

