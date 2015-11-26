module Material.NavDrawer where

import Color exposing (white)
import Graphics.Element exposing (Element, flow, right, container, middle, centered, down, color)
import Graphics.Input exposing (clickable)
import Material.Foo exposing (Page, Pages, scrim)
import Text exposing (fromString)
import Signal exposing (Mailbox)
import Time exposing (Time)
import Easing exposing (easeInBounce, easeInElastic)
import Animation exposing (..)

-- MODEL

type Action
    = SelectPage Page

-- VIEW

drawerWidth = 340
drawerOptionHeight = 100

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

openNavigationDrawer : Time -> (Int, Int) -> Pages -> Mailbox Action -> Time -> (Bool, Element)
openNavigationDrawer start (w, h) pages mailbox clock =
  let widthAnimation = (animation start |> from 0 |> to drawerWidth |> duration 400)
      width = animate clock widthAnimation 
      opened = width == drawerWidth
   in (opened, container (round width) h middle 
      (drawerOptions mailbox.address pages
      |> color white))

closeNavigationDrawer : Time -> (Int, Int) -> Pages -> Mailbox Action -> Time -> (Bool, Element)
closeNavigationDrawer start (w, h) pages mailbox clock =
  let widthAnimation = (animation start |> from drawerWidth |> to 0 |> duration 400)
      width = animate clock widthAnimation 
      closed = width == 0
   in (closed, container (round width) h middle 
      (drawerOptions mailbox.address pages
      |> color white))

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

