module Material.NavDrawer where

import Color exposing (white)
import Graphics.Element exposing (Element, flow, right, container, middle, centered, down, color)
import Graphics.Input exposing (clickable)
import Material.Foo exposing (Page, Pages, scrim)
import Text exposing (fromString)


-- MODEL

type Action
    = SelectPage Page

-- VIEW

{-|
  NavigationDrawer comes in from the left
  width and height should be Window.Dimensions
-}
navigationDrawer : (Int, Int) -> Pages -> Signal.Mailbox Action -> Element
navigationDrawer (width, height) pages mailbox =
  drawerOptions mailbox.address pages
    |> color white

-- Nav Drawer View
drawerOptions : Signal.Address Action -> Pages -> Element
drawerOptions address pages =
  flow down
    (List.map (drawerOption address) pages)

drawerOption : Signal.Address Action -> Page -> Element
drawerOption address page =
  container 340 100 middle (centered (fromString page.title))
    |> clickable (Signal.message address (SelectPage page))

-- SIGNALS

mailbox : Page -> Signal.Mailbox Action
mailbox page =
  Signal.mailbox (SelectPage page)

