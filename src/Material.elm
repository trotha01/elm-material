module Material where

import Graphics.Element exposing (Element, layers, middle, container, flow, down, right, centered, width, height)
import Graphics.Input exposing (clickable)
import List
import Signal exposing (Mailbox)
import Text exposing (fromString)
import Window
import Debug

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

type State
    = MainView Page
    | NavBar Page

-- UPDATE

update : Action -> State -> State
update action state =
  let page = case state of
      MainView page ->
        page
      NavBar page ->
        page
  in case action of
    ToolbarAction action ->
      NavBar page
    NavAction (NavDrawer.SelectPage newPage) ->
      MainView newPage
    CloseNavDrawer ->
      MainView page

-- VIEW

view : Pages -> Mailbox NavDrawer.Action -> (Int, Int) -> State -> Element
view pages navMailbox (w, h) state =
    case state of
        MainView page ->
          pageView (w, h) page
        NavBar page ->
          navView (w, h) pages page navMailbox

pageView : (Int, Int) -> Page -> Element
pageView (w, h) page = flow down
  [
    toolbarView w page.title,
    body (w, 180) page.content
  ]

navView : (Int, Int) -> Pages -> Page -> Mailbox NavDrawer.Action -> Element
navView (w, h) pages currentPage navMailbox =
  layers
    [
      flow down
      [
        toolbarView w currentPage.title,
        body (w, 180) currentPage.content
      ],

      flow right
      [
        NavDrawer.navigationDrawer (w, h) pages navMailbox,
        scrim (w, h)
          |> clickable  (Signal.message appMailbox.address (CloseNavDrawer))
      ]
    ]

body : (Int, Int) -> Element -> Element
body (w, h) content =
  container w h middle content

toolbarView : Int -> String -> Element
toolbarView w title =
    Toolbar.toolbar w title

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
             appMailbox.signal
           ]
           |> Signal.foldp update (MainView initialPage)
           |> Signal.map2 (view pages navMailbox) Window.dimensions

appMailbox : Mailbox Action
appMailbox =
  Signal.mailbox (CloseNavDrawer)

