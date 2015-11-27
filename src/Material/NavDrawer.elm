module Material.NavDrawer where

import Color exposing (white)
import Graphics.Element exposing (Element, flow, right, container, middle, centered, down, color, empty)
import Graphics.Input exposing (clickable)
import Material.Foo exposing (Page, Pages, scrim)
import Text exposing (fromString)
import Signal exposing (Mailbox)
import Time exposing (Time)
import Easing exposing (easeInBounce, easeInElastic)
import Animation exposing (..)
import Debug

-- MODEL

type Action
    = SelectPage Page
    | Tick Time
    | Open Time
    | Close Time
    | CloseNow -- uses model.clock time to close
    | WindowResize (Int, Int)

type alias Model =
    { screenWidth: Int
    , screenHeight: Int
    , drawerWidth: Animation
    , page: Page
    , pages: Pages
    , mailbox: Mailbox Action
    , start: Time
    , clock: Time
    }

initialize : Int -> Int -> Page -> Pages -> Mailbox Action -> Time -> Time -> Model
initialize w h page pages mailbox start clock =
    let widthAnimation = (animation start |> from 0 |> to 0 |> duration 400)
     in Model w h widthAnimation page pages mailbox start clock

model0 : Page -> Pages -> (Int, Int) -> Mailbox Action -> Model
model0 page pages (w, h) mailbox = initialize w h page pages mailbox 0 0

drawerWidth = 340
drawerOptionHeight = 100

drawerOpenAnimation : Time -> Animation
drawerOpenAnimation t = (animation t |> from 0 |> to drawerWidth |> duration 400)

drawerCloseAnimation : Time -> Animation
drawerCloseAnimation t = (animation t |> from drawerWidth |> to 0 |> duration 400)

-- UPDATE

step : Action -> Model -> Model
step action model =
    case action of
        SelectPage page ->
            {model | page = page}
        Tick t ->
            {model | clock = model.clock + t}
        Open t ->
            {model | drawerWidth =  drawerOpenAnimation t }
        Close t ->
            {model | drawerWidth = drawerCloseAnimation t }
        CloseNow ->
            {model | drawerWidth = drawerCloseAnimation model.clock }
        WindowResize (w, h) ->
            {model | screenWidth = w, screenHeight = h }


-- VIEW

view : Model -> Element
view model =
  let width = (animate model.clock model.drawerWidth)
      pageScrim = scrim (model.screenWidth - (round width), model.screenHeight)
                       |> clickable (Signal.message model.mailbox.address (CloseNow))
      drawer = container (round width) model.screenHeight middle 
               (drawerOptions model.mailbox.address model.pages |> color white)
   in if width > 0 then
        flow right
        [ drawer
        , pageScrim
        ]
      else
        empty

{-|
  NavigationDrawer comes in from the left
  width and height should be Window.Dimensions
-}
navigationDrawer : Time -> (Int, Int) -> Pages -> Mailbox Action -> Time -> Element
navigationDrawer start (w, h) pages mailbox clock =
  let widthAnimation = (animation start |> from 0 |> to drawerWidth |> duration 400)
      width = animate clock widthAnimation 
   in container (round width) h middle 
      (drawerOptions mailbox.address pages
      |> color white)

-- Nav Drawer View
drawerOptions : Signal.Address Action -> Pages -> Element
drawerOptions address pages =
  flow down
    (List.map (drawerOption address) pages)

drawerOption : Signal.Address Action -> Page -> Element
drawerOption address page =
   container drawerWidth drawerOptionHeight middle (centered (fromString page.title))
    |> clickable (Signal.message address (SelectPage page))

-- SIGNALS

mailbox : Page -> Mailbox Action
mailbox page =
  Signal.mailbox (SelectPage page)

