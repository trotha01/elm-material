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

import Material.Toolbar as Toolbar exposing (mailbox)
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
    | Tick Time
    | WindowResize (Int,Int)
    | NoAction -- TODO: is this needed?

type View
    = MainView
    | OpenNav Time -- gives the start time, for when the navbar is opened/closed
    | CloseNav Time -- gives the start time, for when the navbar is opened/closed

type alias State = 
    { view: View
    , clock: Time
    , nav: NavDrawer.Model
    , screenWidth: Int
    , screenHeight: Int
    }

-- UPDATE

update : Action -> State -> State
update action state =
  case action of
    ToolbarAction (Toolbar.OpenNavDrawer) ->
      { state | nav = (NavDrawer.step (NavDrawer.Open state.clock) state.nav) }
    NavAction a ->
      { state | nav = (NavDrawer.step a state.nav)}
    Tick t ->
      { state | clock = state.clock + t
              , nav = (NavDrawer.step (NavDrawer.Tick t) state.nav)
      }
    WindowResize (w, h) ->
      { state | screenWidth =  w,
                screenHeight = h,
                nav = (NavDrawer.step (NavDrawer.WindowResize (w,h)) state.nav)
      }
    NoAction ->
        state

-- VIEW

view : State -> Element
view state =
    -- layers
    -- [
      flow down
      [
        toolbarView state.screenWidth state.nav.page.title,
        body (state.screenWidth, 180) state.nav.page.content
      ]
      -- flow right
      -- [
      --     NavDrawer.view state.nav
      -- ]
    -- ]

body : (Int, Int) -> Element -> Element
body (w, h) content =
  container w h middle content

toolbarView : Int -> String -> Element
toolbarView w title =
    Toolbar.view w title
    -- Html.toElement w 128 (Toolbar.toolbar w title)

-- TODO: errorPage : Int -> String -> Page
-- errorPage : statusCode contents =
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
            toolMailbox = Toolbar.mailbox
            model0 =
                { view=MainView
                , clock=0
                , nav=NavDrawer.model0 initialPage pages (1276, 365) navMailbox -- TODO: fix constant width and height
                , screenWidth = 1276 -- TODO: fix constant width and height
                , screenHeight = 365 -- TODO: fix constant width and height
                }
        in Signal.mergeMany
           [
             (Signal.map ToolbarAction toolMailbox.signal),
             (Signal.map NavAction navMailbox.signal),
             (Signal.map Tick (Time.fps 60)),
             (Signal.map WindowResize Window.dimensions),
             appMailbox.signal
           ]
           |> Signal.foldp update model0
           |> Signal.map view

appMailbox : Mailbox Action
appMailbox =
  Signal.mailbox (NoAction)

