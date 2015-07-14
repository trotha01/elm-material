module NavDrawer where
{-
  A Navigation Drawer meant to come in from the side
-}

import Color exposing (white)
import Graphics.Element exposing (Element, flow, right, container, middle, centered, down, color)
import Graphics.Input exposing (clickable)
import Page exposing (Page, Pages, scrim)
import Text exposing (fromString)


-- MODEL

type Action
    = SelectPage Page -- Might change page when nav bar closes

-- VIEW

{-|
  NavigationDrawer comes in from the left
  Includes fading the rest of the screen
  width and height should be Window.Dimensions
  noOpMessage is when someone exits the navbar without selecting anything
-}
navigationDrawer : (Int, Int) -> Pages -> Element
navigationDrawer (width, height) pages =
  let address = navDrawerMailbox.address
  in drawerOptions address pages

-- Nav Drawer View
drawerOption : Signal.Address Action -> Page -> Element
drawerOption address page =
      container 340 100 middle (centered (fromString page.title))
          |> clickable (Signal.message address (SelectPage page))

drawerOptions : Signal.Address Action -> Pages -> Element
drawerOptions address pages =
  flow down
    (List.map (drawerOption address) pages)
    |> color white

-- TODO: initialize with main page?
emptyPage : Page
emptyPage =
  { title = "Uh Oh!"
  , content = centered (fromString "Error: No Pages Found")
  }


-- SIGNALS

navDrawerMailbox : Signal.Mailbox Action
navDrawerMailbox =
  Signal.mailbox (SelectPage emptyPage)
