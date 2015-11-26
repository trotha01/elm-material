module Material where

import Graphics.Element exposing (Element, layers, middle, container, flow, down, right, centered, width, height)
import Graphics.Input exposing (clickable)
import List
import Signal exposing (Mailbox)
import Text exposing (fromString)
import Window
import Debug
import Html
import Time exposing (Time)

import Material.Toolbar as Toolbar exposing (toolbarMailbox)
import Material.NavDrawer as NavDrawer exposing (mailbox)
import Material.Foo exposing (Page, Pages, scrim)

-- TODO:
{-
 - Figure out what good comments look like
    http://package.elm-lang.org/help/documentation-format
 - Rename subpackage foo
-}

-- MODEL

type Action
    = ToolbarAction Toolbar.Action
    | NavAction NavDrawer.Action
    | CloseNavDrawer
    | Tick Time

type View
    = MainView
    | OpenNav Time -- gives the start time, for when the navbar is opened/closed
    | CloseNav Time -- gives the start time, for when the navbar is opened/closed

type alias State = 
    { view:View, page: Page, clock : Time }

-- UPDATE

update : Action -> State -> State
update action state =
  case action of
    ToolbarAction action ->
      {state|view=OpenNav state.clock}
    NavAction (NavDrawer.SelectPage newPage) ->
      {state|view=MainView, page = newPage}
    CloseNavDrawer ->
      {state|view=CloseNav state.clock}
    Tick t ->
      {state | clock = state.clock + t}

-- VIEW

view : Pages -> Mailbox NavDrawer.Action -> (Int, Int) -> State -> Element
view pages navMailbox (w, h) state =
    case state.view of
        MainView ->
          pageView (w, h) state.page
        OpenNav start ->
          navView True start state.clock (w, h) pages state.page navMailbox
        CloseNav start ->
          navView False start state.clock (w, h) pages state.page navMailbox

pageView : (Int, Int) -> Page -> Element
pageView (w, h) page = flow down
  [
    toolbarView w page.title,
    body (w, 180) page.content
  ]

navView : Bool -> Time -> Time -> (Int, Int) -> Pages -> Page -> Mailbox NavDrawer.Action -> Element
navView open start now (w, h) pages currentPage navMailbox =
  let (_, navDrawer) = 
        if open then
            NavDrawer.openNavigationDrawer start (w, h) pages navMailbox now
        else
            NavDrawer.closeNavigationDrawer start (w, h) pages navMailbox now
   in layers
    [
      flow down
      [
        toolbarView w currentPage.title,
        body (w, 180) currentPage.content
      ],

      flow right
      [
          navDrawer,
          scrim (w, h)
              |> clickable (Signal.message appMailbox.address (CloseNavDrawer))
      ]
    ]

body : (Int, Int) -> Element -> Element
body (w, h) content =
  container w h middle content

toolbarView : Int -> String -> Element
toolbarView w title =
    Toolbar.toolbar w title
    -- Html.toElement w 128 (Toolbar.toolbar w title)

errorPage : String -> Page
errorPage contents =
  {
    title = "Uh Oh!",
    content = centered (fromString contents)
  }

-- SIGNALS

app : Pages -> Signal Element
app pages =
        let initialPage = case (List.head pages) of
                Just page ->
                  page
                Nothing ->
                  errorPage "No pages found!"
            navMailbox = NavDrawer.mailbox initialPage
        in Signal.mergeMany
           [
             (Signal.map ToolbarAction toolbarMailbox.signal),
             (Signal.map NavAction navMailbox.signal),
             (Signal.map Tick (Time.fps 60)),
             appMailbox.signal
           ]
           |> Signal.foldp update ({view=MainView, page=initialPage, clock=0})
           |> Signal.map2 (view pages navMailbox) Window.dimensions

appMailbox : Mailbox Action
appMailbox =
  Signal.mailbox (CloseNavDrawer)

