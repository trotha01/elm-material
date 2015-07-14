module Material where

import Graphics.Element exposing
  (Element, layers, middle, container, flow, right, down, color, spacer, opacity, centered)
import Color exposing (white, black)
import Graphics.Input exposing (clickable)
import List
import Signal
import Text exposing (fromString)
import Window

import Toolbar exposing (toolbarMailbox)
import NavDrawer exposing (navDrawerMailbox)
import Page exposing (Page, Pages)

-- MODEL

type Action
    = ToolbarAction Toolbar.Action
    | NavAction NavDrawer.Action

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
    ToolbarAction a -> NavBar page
    NavAction a -> case a of
      NavDrawer.CloseNavDrawer (Just newPage) -> MainView newPage
      NavDrawer.CloseNavDrawer Nothing -> MainView page

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
  , NavDrawer.navigationDrawer (w, h) pages navDrawerMailbox.address
  ]

body : (Int, Int) -> Element -> Element
body (w, h) content =
  container w h middle content

toolbarView : Int -> String -> Element
toolbarView w title =
    Toolbar.toolbar w title (Signal.message toolbarMailbox.address Toolbar.OpenNavDrawer)

emptyPage : Page
emptyPage =
  { title = "Uh Oh!"
  , content = centered (fromString "Error: No Pages Found")
  }

{-
action : Signal.Mailbox Action
action =
  Signal.mailbox (OpenNavDrawer)
  -}

app : Pages -> Signal Element
app pages =
        let initialPage = case (List.head pages) of
                Just p -> p
                Nothing -> emptyPage
        in (Signal.merge
            (Signal.map ToolbarAction toolbarMailbox.signal)
            (Signal.map NavAction navDrawerMailbox.signal))
           |> Signal.foldp update (MainView initialPage)
           |> Signal.map2 (view pages) Window.dimensions

