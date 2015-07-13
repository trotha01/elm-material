module Material where

import Graphics.Element exposing
  (Element, layers, middle, container, flow, right, down, color, spacer, opacity, midLeft, image, centered, leftAligned)
import Color exposing (green, white, black)
import Graphics.Input exposing (clickable)
import List
import Signal
import Text exposing (Style, fromString, defaultStyle, style)
import Window
import Toolbar

-- MODEL

type alias Page = { content:Element, title:String }
type alias Pages = List Page

type Action
    = OpenNavDrawer
    | CloseNavDrawer (Maybe Page) -- Might change page when nav bar closes

type State
    = MainView Page
    | NavBar Page

-- UPDATE

update : Action -> State -> State
update action state =
  let page =
    case state of
      MainView p -> p
      NavBar p -> p
  in case action of
    OpenNavDrawer -> NavBar page
    CloseNavDrawer (Just page) -> MainView page
    CloseNavDrawer (Nothing) -> MainView page


-- VIEW

view : Pages -> (Int, Int) -> State -> Element
view pages (w, h) state =
    case state of
        MainView page -> pageView (w, h) page
        NavBar page -> navView (w, h) pages page

pageView (w, h) page = flow down
  [ toolbarView w page.title
  , body (w, 180) page.content
  ]

navView (w, h) pages currentPage = layers
  [ flow down
    [ toolbarView w currentPage.title
    , body (w, 180) currentPage.content
    ]
  , navigationDrawer (w, h) pages action.address
  ]

body : (Int, Int) -> Element -> Element
body (w, h) content =
  container w h middle content

toolbarView : Int -> String -> Element
toolbarView w title =
    Toolbar.toolbar (w, 180) title (Signal.message action.address OpenNavDrawer)

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

{-|
  The scrim is used to fade the screen
-}
scrim : (Int, Int) -> Element
scrim (width, height) =
    spacer (width) height
      |> color black
      |> opacity (0.5)

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

emptyPage : Page
emptyPage =
  { title = "Uh Oh!"
  , content = centered (fromString "Error: No Pages Found")
  }

action : Signal.Mailbox Action
action =
  Signal.mailbox (OpenNavDrawer)

app : Pages -> Signal Element
app pages =
        let initialPage = case (List.head pages) of
                Just p -> p
                Nothing -> emptyPage
        in action.signal
           |> Signal.foldp update (MainView initialPage)
           |> Signal.map2 (view pages) Window.dimensions

