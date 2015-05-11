import Window
import Color exposing (green, white, black)
import Graphics.Element exposing (Element)
import Graphics.Element exposing
  (Element, centered, middle, down, layers, container, color, flow, right, spacer)
import Graphics.Input exposing (clickable)
import Text exposing (fromString)
import Maybe exposing (Maybe)
import List

import Material exposing (Page, Pages)

main : Signal Element
main = action.signal
  |> Signal.foldp update (MainView helloWorld)
  |> Signal.map2 view Window.dimensions

-- MODEL
type State
    = MainView Page
    | NavBar Page

helloWorld : Page
helloWorld =
  { content = centered (fromString "Hello World")
  , title = "Material Design Sample"
  }

components : Page
components =
  { content = centered (fromString "Nothin` here yet")
  , title = "Components"
  }

pages : Pages
pages = [ helloWorld, components ]


-- INPUTS

action : Signal.Mailbox Material.Action
action =
  Signal.mailbox (Material.OpenNavDrawer)

-- DISPLAY

body : (Int, Int) -> Element -> Element
body (w, h) content =
  container w h middle content

toolbar : Int -> String -> Element
toolbar w title =
    Material.toolbar (w, 180) title (Signal.message action.address Material.OpenNavDrawer)

view : (Int, Int) -> State -> Element
view (w, h) state =
    case state of
        MainView page -> flow down
          [ toolbar w page.title
          , body (w, 180) page.content
          ]
        NavBar page -> layers
          [ flow down
              [ toolbar w page.title
              , body (w, 180) page.content
              ]
          , Material.navigationDrawer (w, h) pages action.address
          ]

-- UPDATE

update : Material.Action -> State -> State
update action state =
  let page =
    case state of
      MainView p -> p
      NavBar p -> p
  in case action of
    Material.OpenNavDrawer -> NavBar page
    Material.CloseNavDrawer (Just page) -> MainView page
    Material.CloseNavDrawer (Nothing) -> MainView page
