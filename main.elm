import Window
import Color exposing (green, white, black)
import Graphics.Element exposing (Element)
import Graphics.Element as G -- exposing (Element, centered, container, color, flow, right, spacer)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)
import Maybe exposing (Maybe)
import List

import Debug

import Material exposing (Page, Pages, navigationDrawer)

main : Signal Element
main = action.signal
  |> Signal.foldp update (MainView helloWorld.content)
  |> Signal.map2 view Window.dimensions
  |> Debug.watch "action.signal"

-- MODEL
type State
    = MainView Element
    | NavBar Element


helloWorld : Page
helloWorld =
  { content = G.centered (fromString "Hello World")
  , title = "helloWorld page"                                                                                                             
  }

stuff : Page
stuff =
  { content = G.centered (fromString "Stuff")
  , title = "stuff page"
  }

pages : Pages
pages = [ helloWorld, stuff]


-- INPUTS

action : Signal.Mailbox Material.Action
action =
  Signal.mailbox (Material.OpenNavDrawer)

-- DISPLAY

body : (Int, Int) -> Element -> Element
body (w, h) content =
  G.container w h G.middle content

view : (Int, Int) -> State -> Element
view (w, h) state =
    case state of
        MainView content -> G.flow G.down
          [ Material.toolbar (w, 180) "Title" (Signal.message action.address Material.OpenNavDrawer)
          , body (w, (h-180)) content
          ]
          |> Debug.log "view mainview"
        NavBar content -> G.layers
          [ G.flow G.down
              [ Material.toolbar (w, 180) "Title" (Signal.message action.address Material.OpenNavDrawer)
              , body (w, (h-180)) content
              ]
          , navigationDrawer w h pages action.address
          ]
          |> Debug.log "view navBar"

-- UPDATE

update : Material.Action -> State -> State
update action state =
  let content =
    case state of
      MainView c -> c
      NavBar c -> c
  in case action of
    Material.OpenNavDrawer -> NavBar content
    Material.CloseNavDrawer (Just newContent) -> MainView newContent 
    Material.CloseNavDrawer (Nothing) -> MainView content
