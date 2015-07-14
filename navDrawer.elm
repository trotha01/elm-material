module NavDrawer where
{-
  A Navigation Drawer meant to come in from the side
-}

import Color exposing (white)
import Graphics.Element exposing (Element, flow, right, container, middle, centered, down, color)
import Graphics.Input exposing (clickable)
import Page exposing (Page, Pages, scrim)
import Text exposing (fromString)


-- Model
type Action
    = CloseNavDrawer (Maybe Page) -- Might change page when nav bar closes

{-|
  NavigationDrawer comes in from the left
  Includes fading the rest of the screen
  width and height should be Window.Dimensions
  noOpMessage is when someone exits the navbar without selecting anything
-}
navigationDrawer : (Int, Int) -> Pages -> Signal.Address Action -> Element
navigationDrawer (width, height) pages address =
  flow right
  [ drawerOptions address pages
  , scrim (width, height)
      |> clickable (Signal.message address (CloseNavDrawer Nothing))
  ]

-- Nav Drawer View
drawerOption : Signal.Address Action -> Page -> Element
drawerOption address page =
      container 340 100 middle (centered (fromString page.title))
          |> clickable (Signal.message address (CloseNavDrawer (Just page)))

drawerOptions : Signal.Address Action -> Pages -> Element
drawerOptions address pages =
  flow down
    (List.map (drawerOption address) pages)
    |> color white

-- Action
navDrawerMailbox : Signal.Mailbox Action
navDrawerMailbox =
  Signal.mailbox (CloseNavDrawer Nothing)
